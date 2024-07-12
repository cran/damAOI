getnextpoint <- function(points,pl,nd,ac_tolerance,e_tolerance,direction,nn){

  # calculates the number of nearest neighbours to find.
  # either nn points or all remaining points if there are fewer than nn points left
  intersect <- dplyr::intersect
  nn <- ifelse(nrow(points) <= nn, nrow(points), nn)
  if(direction == "downstream"){
  # gets the index (lower means closer) for the points which meet the following conditions  
    c1 <- which(points$ac[nd$nn.index[pl$id,2:nn]] >= pl$ac)
    c2 <- which(points$e[nd$nn.index[pl$id,2:nn]] <= pl$e + e_tolerance)
    c3 <- which(points$ac[nd$nn.index[pl$id,2:nn]] / ac_tolerance <= pl$ac)
    index <- min(intersect(intersect(c1,c2),c3)) }
  if(direction == "upstream") {
    c1 <- which(points$ac[nd$nn.index[pl$id,2:nn]] <= pl$ac) # lower accumulation
    c2 <- which(points$e[nd$nn.index[pl$id,2:nn]] >= pl$e - e_tolerance) # higher elevation
    c3 <- which(points$ac[nd$nn.index[pl$id,2:nn]] * ac_tolerance >= pl$ac) # with accumulation tolerance
    index <- min(intersect(intersect(c1,c2),c3))
  }
  pn <- points[points$id == nd$nn.index[pl$id,index+1],] # stores id of next point (in relation to last point)
  pn$d <- nd$nn.dist[pl$id,index+1] # and the distance away from the last point
  return(pn)
}
