#' Buffers the reservoir and the river, and clips to basin areas
#' @export
#' @returns A two element list. Element 1 is an sf multipolygon with the reservoir buffer, upstream and downstream areas. Element 2 is the same, but clipped to river basin polygons.
#' @param reservoir An sf polygon, with an unstandardised raw reservoir
#' @param upstream An sf line, following the river upstream of the reservoir 
#' @param downstream An sf line, following the river downstream of the reservoir 
#' @param basins An sf multipolygon, with the basins in the area around the dam 
#' @param streambuffersize A number indicating the distance around the upstream and downstream river to consider as impacted. Defaults to 2000 (2km).
#' @param reservoirbuffersize A number indicating the distance around the reserviur to consider as impacted. Defaults to 5000 (5km)

basinandbuffers <- function(reservoir,upstream,downstream,basins,streambuffersize,reservoirbuffersize){
  # combines reservoir and stream lines
  basearea <- rbind(reservoir$geometry,upstream,downstream)
  # buffers the reserrvoir by buffer size parameter.
  bufferreservoir <- st_buffer(reservoir$geometry, reservoirbuffersize)
  #buffers upstream by streambuffer parameter
  # areas which intersect with the reservoir buffer are excluded 
  usb <- getstreambuffers(line = upstream,reservoir = bufferreservoir, buffer_size = streambuffersize)[,1]
  #buffers downstream by streambuffer parameter
  # areas which intersect with the reservoir buffer are excluded   
  dsb <- getstreambuffers(line = downstream, reservoir = bufferreservoir, buffer_size = streambuffersize)[,1]
  # joins buffered areas to make general impact areas
  impactedarea <- rbind(bufferreservoir,dsb,usb)
  impactedarea <- st_make_valid(impactedarea)
  impactedarea <- impactedarea[st_is_valid(impactedarea) == TRUE,]
  # ensures that basins are within the impacted area
  cropbasin <- basins[impactedarea,]
  cropbasin$id <- 1:nrow(cropbasin)
  # and ensures that these basins intersect with the streams 
  # this is important, as the area which intersects with the stream buffers will likely include areas outside the basins which drain into the river
  st_agr(cropbasin) = "constant"
  intersectsbasearea <- cropbasin[cropbasin$id %in% st_intersection(cropbasin, st_make_valid(st_union(basearea)))$id,]
  # turn multipolygon into polygon
  dissolveintersectsbasearea <- st_as_sf(st_union(intersectsbasearea))
  # and get the intersection with the buffered area
  clippedbybasin <- st_intersection(impactedarea, dissolveintersectsbasearea)
  # removes slivery unimportant polygons that make their way between the reservoirs and the rivers in the geometry operations
  clippedbybasin <- clippedbybasin[units::drop_units(st_area(clippedbybasin)) >= 1000,]
  return(list(impactedarea, clippedbybasin))
}
