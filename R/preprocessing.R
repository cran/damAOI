#' preprocessing
#' 
#' @export
#' @returns A list with utm transformed input data
#' @param reservoir An sf polygon, with an unstandardised raw reservoir
#' @param water_bodies A rast, where 1 indicates water, NA otherwise
#' @param dem A rast, showing elevation
#' @param pourpoints An sf multipoint, showing the points where rivers flow in and out of reservoirs
#' @param fac A rast, showing accumulated water flow along river
#' @param basins An sf multipolygon, with the basins in the area around the dam
#' @param river_distance A number, indicating the number of meters downstream and upstream for the area of interest. Defaults to 100000 (100km)

preprocessing <- function(reservoir, 
                          dem = NULL, 
                          fac = NULL, 
                          water_bodies = NULL, 
                          basins = NULL,
                          pourpoints = NULL,
                          river_distance) {
  # gets the longitude and latitude of the Reservoir
  latlon <- unname(st_centroid(st_geometry(reservoir))[[1]])
  # gets the utm zone for the reservoir
  utm <- getutm(latlon[2], latlon[1])
  # projects the reservoir to utm
  reservoir <- st_transform(reservoir, utm)
  
  rb <- st_buffer(reservoir, river_distance)
  rb <- st_transform(rb, 4326)

  if(!is.null(dem)){
  dem_c <- crop(dem, vect(rb))
  dem <- project(dem_c, crs(reservoir), method = "min")
}
  if(!is.null(fac)){
    fac_c <- crop(fac, vect(rb))
    fac_c[fac_c<1000] <- NA
    fac_points <- as.points(fac_c, values = T)
    fac_points_projected <- project(fac_points,crs(reservoir))
    fac <- rasterize(fac_points_projected, dem, fun = "mean", field = names(fac_c))
  }
  if(!is.null(basins)){
    basins_c <- basins[rb,]
    basins <- st_transform(basins_c, utm)
  }
  if(!is.null(water_bodies)){
    water_bodies_c <- crop(water_bodies, vect(rb))
    water_bodies <- project(water_bodies_c, crs(reservoir), method = "sum")
    water_bodies[water_bodies < 0.9] <- NA
    water_bodies[!is.na(water_bodies)] <- 1
  }
  if(!is.null(pourpoints)){
    pourpoints <- st_transform(pourpoints, utm)
  }
  
  return(list(reservoir, dem, fac, basins, water_bodies, pourpoints, utm))
}
