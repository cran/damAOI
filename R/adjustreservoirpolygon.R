#' adjust polygon of reservoir to reference surface water extent map
#' @export
#' @returns An sf polygon with an adjusted reservoir polygon
#' @param reservoir An sf polygon, with an unstandardised raw reservoir
#' @param water_bodies A rast, where 1 indicates water, NA otherwise
#' @param dem A rast, showing elevation
#' @param poss_expand A number, indicating the number of meters away from the raw reservoir the reservoir may expand to. Default is 20000 (20km).
#' @param wbjc A number, the water body join correction. This indicates the buffer zone for the reservoir, to ensure that it is contiguous (important where there are small channels connecting different parts of the same water body). Default is 0, but is necessary for some dams depending on the context. 

adjustreservoirpolygon <- function(reservoir, water_bodies, dem, poss_expand = 20000, wbjc =0) {
# creates a buffer zone around the GRanD polygon, size defined by "poss_expand parameter"
  rb <- st_buffer(reservoir, poss_expand)
# crops the water bodies raster by this buffer area  
  wb <- crop(water_bodies, rb)
# ensures water bodies raster only has one band
  wb <- wb$WB
# all water is set to 1, otherwise NA
  wb[!is.na(wb)] <- 1
# crops the dem to the buffered reservoir
  demcrop <- crop(dem, rb)  
# resamples to same res/extent as water bodies
  demcrop <- resample(demcrop, wb)  
# creates a raster version of the original reservoir  
  rr <- rasterize(vect(reservoir), wb)
# gets the min/max elevation for the reservoir area
  minmaxelev <- rr * demcrop
# and extracts minimums/maximums
  mi <- min(minmaxelev[], na.rm = TRUE)
  ma <- max(minmaxelev[], na.rm = TRUE)
# filters the expand so only areas between the minimum and maximum reservoir area are potentially expandable areas
  demcrop[demcrop > ma] <- NA
  demcrop[demcrop < mi] <- NA
# changes all values to 1 to create a presence/absence raster of eligible elevation pixels
  demcrop[!is.na(demcrop)] <- 1
# and masks the water bodies layer by this
  wb <- wb * demcrop
# we need a polygon, so this extracts the water bodies from the raster
  polywb <- as.polygons(wb)
# wbjc = water body join correction. 
# This is a parameter to correct for erroneously non-contiguous water bodies.
# this won't be needed always, so default is 0. 
# When necessary (i.e. narrow bottlenecks in reservoirs), it should be assigned the lowest possible value  
  polywb <- buffer(polywb, wbjc)
# Joins the buffered geometries
  polywb <- aggregate(makeValid(polywb))
# Converts to an sf polygon
  polywb <- st_as_sf(polywb) %>% 
    st_cast("POLYGON", warn = FALSE) %>% 
    mutate(id = row_number())
# caluculates the area of all the water bodies  
  polywb$area <- polywb %>% st_area()
# and selects the largest (e.g. the reservoir)
  polywb <- polywb[polywb$area == max(polywb$area),]
# smooths this to get rid of raster edge effects
  smoothres <- smooth(polywb, method = "chaikin")
  smoothres <- st_make_valid(smoothres)

# sometimes this doesn't work that well, particularly where the innundated areas are narrow, and following the path of the river.
# in these cases, we recommend using the original reservoir
  
  area_new <- st_area(smoothres)
  area_old <- st_area(reservoir)
  area_old > area_new
  if(area_old > area_new){
    smoothres <- reservoir
  }
  
  return(smoothres)
}
