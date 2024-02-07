getutm <- function(latitude, longitude) {
  ESPG <-32700-round((45+latitude)/90,0)*100+round((183+longitude)/6,0)}
# code taken from stack exchange
# https://gis.stackexchange.com/questions/190198/how-to-get-appropriate-crs-for-a-position-specified-in-lat-lon-coordinates/190209#190209
