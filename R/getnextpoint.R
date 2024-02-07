getnextpoint <- function(points,pl,nd,ac_tolerance,e_tolerance,direction,nn){

  # calculates the number of nearest neighbours to find.
  # either nn points or all remaining points if there are fewer than nn points left
  intersect <- dplyr::intersect
  nn <- ifelse(nrow(points) <= nn, nrow(points), nn)
  if(direction == "downstream"){
  # gets the index (lower means closer) for the points which meet the following conditions  
    index <- min(
      intersect(intersect(intersect(
        which(
          points$ac[nd$nn.index[pl$id,2:nn]] >= pl$ac), # higher accumulation
        which(
          points$e[nd$nn.index[pl$id,2:nn]] <= pl$e)), # lower elevation
        which(
          points$ac[nd$nn.index[pl$id,2:nn]] / ac_tolerance <= pl$ac)), # within accumulation tolerance
        which(
          abs(points$e[nd$nn.index[pl$id,2:nn]]-pl$e) <= e_tolerance)) # within elevation tolerance
    )
  }
  if(direction == "upstream") {
    index <- min(
      intersect(intersect(intersect(
        which(
          points$ac[nd$nn.index[pl$id,2:nn]] <= pl$ac), # lower accumulation
        which(
          points$e[nd$nn.index[pl$id,2:nn]] >= pl$e)), # higher elevation
        which(
          points$ac[nd$nn.index[pl$id,2:nn]] * ac_tolerance >= pl$ac)), # within accumulation tolerance
        which(
          abs(points$e[nd$nn.index[pl$id,2:nn]]-pl$e) <= e_tolerance) # within elevation tolerance
      )
    )
  }
  pn <- points[points$id == nd$nn.index[pl$id,index+1],] # stores id of next point (in relation to last point)
  pn$d <- nd$nn.dist[pl$id,index+1] # and the distance away from the last point
  return(pn)
}
