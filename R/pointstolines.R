#' pointstolines
#' 
#' @export
#' @returns An list of upstream lines (multilinestring) and downstream line (linestring)
#' @param riverpoints list of dataframes returned by the riverpoints function.
#' @param espg espg code of UTM zone

pointstolines <- function(riverpoints , espg){
  espg <- espg
  up <- riverpoints[sapply(riverpoints, FUN = function(x){mean(x[,7]) == 0})]
  ids <- 1:(length(up))
  uplist <- lapply(ids, function(i){cbind(st_linestring(as.matrix(up[[i]][,1:2])), i)})
  up <- st_multilinestring(uplist)
  up <- st_sf(st_sfc(up,  crs = espg))
  st_geometry(up) <- "geometry"
  down <- riverpoints[sapply(riverpoints, FUN = function(x){mean(x[,7]) == 1})]
  down <- st_linestring(as.matrix(down[[1]][,1:2]))
  down <- st_sf(st_sfc(down,  crs = espg))
  st_geometry(down) <- "geometry"
  return(list(up, down))
}
