#' pointstolines
#' 
#' @export
#' @returns An list of upstream lines (multilinestring) and downstream line (linestring)
#' @param riverpoints list of dataframes returned by the riverpoints function.


pointstolines <- function(riverpoints){
  espg <- riverpoints[[1]][[7]][[1]]
  up <- riverpoints[sapply(riverpoints, FUN = function(x){mean(x[,8]) == 0})]
  ids <- 1:(length(up))
  uplist <- lapply(ids, function(i){cbind(st_linestring(as.matrix(up[[i]][,1:2])), i)})
  up <- st_multilinestring(uplist)
  up <- st_sf(st_sfc(up,  crs = espg))
  up <- st_transform(up, 4326)
  st_geometry(up) <- "geometry"
  down <- riverpoints[sapply(riverpoints, FUN = function(x){mean(x[,8]) == 1})]
  down <- st_linestring(as.matrix(down[[1]][,1:2]))
  down <- st_sf(st_sfc(down,  crs = espg))
  down <- st_transform(down, 4326)
  st_geometry(down) <- "geometry"
  return(list(up, down))
}
