#' Calculation of river points
#' @export
#' @returns A three-element list, where the first element contains the data produced by the algorithm for all points along the river, and the second element is the sf LINESTRING object for the river, and the third denotes whether the river goes upstream (0) or downstream (1) 
#' @param reservoir An sf polygon, with an unstandardised raw reservoir
#' @param dem A rast, showing elevation
#' @param fac A rast, showing accumulated water flow along river
#' @param pourpoints An sf multipoint, showing the points where rivers flow in and out of reservoirs
#' @param river_distance A number, indicating the number of meters downstream and upstream for the area of interest. Defaults to 100000 (100km)
#' @param nn A number, indicating the number of nearest neighbours to consider in the algorithm to determine river course. Higher can be more accurate but is slower. Default 100.
#' @param ac_tolerance A number, indicating the tolerance to changes in flow accumulation. Default 2, which means that if accumulated flow changes by a factor of 2 (halved or doubled) the area of interest should not include any further downstream or upstream. This is to account for confluences.
#' @param e_tolerance A number indicating the tolerance to changes in elevation. Rivers flow downstream. But DEMs can show downstream areas of the river as higher, due to averaging nearby pixels. This is particularly true when rivers run through gorges. If there is no downstream lower river poitn nearby, the elevation tolerance allows the algorithm to select a point at a higher elevation, up to the threshold defined here.
#' @param ppid An integer to index through the pourpoints dataframe 

getriverpoints <- function(reservoir,
                           pourpoints,
                           ppid,
                           river_distance,
                           ac_tolerance,
                           e_tolerance,
                           nn,
                           fac = fac,
                           dem = dem) {
  # creates a buffer of 'river_distance' meters around the dam
  dam_buffer <- st_buffer(reservoir, river_distance)
  # crops the flow accumulation raster to the dam buffer
  fac_dam <- crop(fac, dam_buffer, snap = "out")
  # removes low/insignificant values of flow accumulation
  fac_dam[fac_dam <= 50] <- NA
  # crops the dem to the dam buffer
  dem_dam <- crop(dem, dam_buffer, snap = "out")
  # creates a raster for the dam extent
  dam_binary <- rasterize(vect(reservoir), fac_dam)
  pourpoint <- pourpoints[ppid,]
  st_crs(pourpoint) <- st_crs(reservoir)
  fac_pp <- terra::extract(x = fac_dam, y = vect(st_buffer(pourpoint, 1000)))
  mx <- max(fac_pp, na.rm = TRUE)
  mn <- min(fac_pp, na.rm = TRUE)
  mnmx <- minmax(fac_dam)
  mx <- ifelse(is.na(mx), mnmx[2],mx)
  if(mx < 1000) {
    warning("River extension too small")
    return(NULL)}
  mn <- ifelse(is.na(mn), mnmx[1],mx)
  fac_damextent <- fac_dam * dam_binary
  
  if(ext(dem_dam) != ext(dam_binary)){
    dem_dam <- resample(dem_dam, dam_binary)
  }
  dem_damextent <- dem_dam * dam_binary
  
  if(pourpoint$direction == 1) {
    # removes upstream
    fac_dam[fac_dam <= mx] <- NA
    # creates a relevant river binary mask
    fac_dam_binary <- fac_dam
    fac_dam_binary[!is.na(fac_dam_binary)] <- 1
    # removes upstream areas from the DEM
    dem_dam[dem_dam >= minmax(dem_damextent)[2]] <- NA
    # extracts elevations for river pixels
    dem_dam <- dem_dam * fac_dam_binary
    # creates a binary mask for dems
    dem_dam_binary <- dem_dam
    dem_dam_binary[!is.na(dem_dam_binary)] <- 1
    # removes upstream areas from river raster
    fac_dam <- fac_dam * dem_dam_binary
  } 
  if(pourpoint$direction == 0) {
    fac_dam[fac_dam >= mn] <- NA
    # creates a relevant river binary mask
    fac_dam_binary <- fac_dam
    fac_dam_binary[!is.na(fac_dam_binary)] <- 1
    # removes downstream areas from the DEM
    dem_dam[dem_dam <= minmax(dem_damextent)[1]] <- NA
    # extracts elevations for river pixels
    dem_dam <- dem_dam * fac_dam_binary
    # creates a binary mask for dems
    dem_dam_binary <- dem_dam
    dem_dam_binary[!is.na(dem_dam_binary)] <- 1
    # removes upstream areas from river raster
    fac_dam <- fac_dam * dem_dam_binary
  }
  # removes areas with under 1000 FAC values (to remove small stream points)
  fac_dam[fac_dam <= 1000] <- NA
  fac_sf <- terra::as.data.frame(fac_dam, xy = TRUE) %>% st_as_sf(coords = c("x","y")) %>% drop_na() %>% rename(ac = names(fac_dam))
  # matches the crs with the dam crs
  st_crs(fac_sf) <- st_crs(reservoir)
  # extracts the elevation information
  dem_sf <- terra::as.data.frame(dem_dam, xy = TRUE) %>% st_as_sf(coords = c("x","y")) %>% drop_na() %>% rename(e = names(dem_dam))
  st_crs(dem_sf) <- st_crs(reservoir)
  #joins this with the accumulation information
  points <- st_join(fac_sf, dem_sf)
  points$e <- round(points$e/10)
  # initialises the output value data frame ready to be populated
  points$dtostart <- as.numeric(st_distance(pourpoint, fac_sf))
  output <- cbind(rep(NA,nrow(fac_sf)),rep(NA,nrow(fac_sf)),rep(NA,nrow(fac_sf)),rep(NA,nrow(fac_sf)),rep(NA,nrow(fac_sf),),rep(NA,nrow(fac_sf)))
  #  initialise points id column
  points$id <- 1:nrow(points)
  closest <- points[points$dtostart == min(points$dtostart),]
  # sets initial distance at 0
  distance <- 0
  # sets incrementor for while loop
  incrementor <- 1
  
  while(incrementor < nrow(fac_sf)){ # while incrementor is less than the number of river points (the maximum length of the river)
    mp <- matrix(unlist(points$geometry), ncol = 2, byrow = T) # gets xy of all points
    if(nrow(points) <= 3){break} # knn doesn't make much sense at this point, and the algorithm breaks. Normally by the sea or the source.
    nd <- get.knnx(mp,mp,ifelse(nrow(mp) <= nn, nrow(mp), nn)) # finds k nearest neighbours (where k is set as nn, or if there are fewer than nn points remaining, it is all points)
    if(incrementor == 1){ # process for first point is different
      pl <- points[points$id == closest$id,] # pl (point last) is where the id is equal to the id of closest
      pl$d <- 0 # distance from pl to last point (there isn't one) is set at 0
    } else{
      pl <- points[points$id == pn$id,] # else sets the id of the last point to the id of the next point from previous iteration
    }
    suppressWarnings({
    if(pourpoint$direction == 1) {pn <- getnextpoint(points,pl,nd,ac_tolerance,e_tolerance,"downstream",nn)} # finds the next point
    if(pourpoint$direction == 0) {pn <- getnextpoint(points,pl,nd,ac_tolerance,e_tolerance,"upstream",nn)} # finds the next point
    })
    if(nrow(pn) != 1) {break} # this catches if there is an error finding the point (perhaps because the river finishes so there is no point that meets the conditions)
    if(!is.numeric(pn$d)) {break} # this catches if there is an error finding the point (perhaps because the river finishes so there is no point that meets the conditions)
    distance <- distance + pn$d # this creates a cumulative distance 
    if(distance >= river_distance) {break} # and breaks the loop if the distance is higher than the distance that we wish to consider part of the aoi - set at 100
    flowchange <- (pn$ac-pl$ac)/pl$ac # this calculates the change in accumulated flow from pl (last point) to pn (next point)
    if(!is.numeric(flowchange)){break} # this catches if there is an error calculating the change in flow (perhaps because the river terminates))
    output[incrementor,1:2] <- pl$geometry[[1]][1:2] # sets the output xy values for row [incrementor] to the geometry of pl
    output[incrementor,3] <- pl$d # sets the output distance value to the distance between pl and pn 
    output[incrementor,4] <- distance # sets the output cumulative distance value to the cumulative distance
    output[incrementor,5] <- pl$ac # sets the output flow to the flow of pl
    output[incrementor,6] <- flowchange # sets the output flow change to the flowchange between pl and pn
    points <- points[points$id != pl$id,] # removes the last point from the river calculation
    if(pourpoint$direction == 1) {points <- points[points$ac >= pl$ac,]} # if downstream removes all points with lower flow accumulation 
    if(pourpoint$direction == 0) {points <- points[points$ac <= pl$ac,]} # if upstream removes all points with higher flow accumulation 
    points$id <- 1:nrow(points) # resets the ids of all points
    newid <- which(points$geometry == pn$geometry) # gets the id of the point in the river to use for next iteration (by matching geometry)
    pn$id <- newid # and sets this as the next point's id
    incrementor = incrementor + 1 # resets the process to further down/up the river
  }
  # loop finished
  riverpoints <- data.frame(output) %>% drop_na() %>% mutate(direction = pourpoint$direction) # transforms output to a data frame and adds direction info
  colnames(riverpoints) <- c("x", "y", "dist", "dist_accum", "flow_accum", "flow_change", "direction")
  return(riverpoints)
  }
