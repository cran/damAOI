cropdata <- function(reservoir, dem, fac, water_bodies, basins, river_distance) {
  rb <- st_buffer(reservoir, river_distance)
  dc <- crop(dem, vect(rb))
  fc <- crop(fac, vect(rb))
  wb <- crop(water_bodies, vect(rb))
  bc <- basins[rb,]
  return(list(dc, fc, wb, bc))
}