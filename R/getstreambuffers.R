getstreambuffers <- function(line, reservoir, buffer_size) {
  # buffers the line by the buffer size
  bufferstream <- st_buffer(line, buffer_size)
  # finds the intersection between the reservoir and the stream buffer  
  intersection <- st_intersection(reservoir, bufferstream, dimension = "polygon")
  intersection <- st_make_valid(intersection)
  # removes this intersection from the stream buffer
  stream <- st_difference(bufferstream, intersection, dimension = "polygon") %>% st_cast("POLYGON")
  # makes it valid and calculates the area
  stream$area <- stream %>% st_make_valid() %>% st_area()
  # removes mini streams. Some slivery polygons are created by the intersection/difference operation.
  # These are between the reservoir and the stream buffer, and are unimportant/removed in this operation
  stream <- stream[stream$area == max(stream$area),]
  return(stream)
}

