## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 

## -----------------------------------------------------------------------------
library(terra)
library(sf)
library(damAOI)

requireNamespace("ggplot2", quietly = TRUE)
tehri_wb <- rast(system.file("extdata", "wb_tehri.tif", package="damAOI"))
tehri_dem <- rast(system.file("extdata", "dem_tehri.tif", package="damAOI"))
tehri_adjusted <- adjustreservoirpolygon(tehri, tehri_wb, tehri_dem, 20000, 0)
ggplot2::ggplot() + 
  ggplot2::geom_sf(data = tehri_adjusted, fill = "skyblue", col = "skyblue") +
  ggplot2::geom_sf(data = tehri, fill = "blue", col = "blue") +
  ggplot2::theme_void()


## -----------------------------------------------------------------------------
tehri_fac <- rast(system.file("extdata", "fac_tehri.tif", package="damAOI"))
pourpoints <- autogetpourpoints(tehri_adjusted, tehri_fac)
# See commented code at the end of the document for the process to get pour points manually

ppid <- as.vector(1:nrow(pourpoints), mode = "list")
riverpoints <- lapply(X = ppid, FUN = getriverpoints, 
                      reservoir = tehri_adjusted, 
                      pourpoints = pourpoints,
                      river_distance = 100000,
                      ac_tolerance = 50,
                      e_tolerance = 10, 
                      nn = 100, 
                      fac = tehri_fac,
                      dem = tehri_dem)
riverpoints[sapply(riverpoints, is.null)] <- NULL 
# if pour points have very small river distances flowing into them, they will be NULL elements in the list of riverpoints
# this removes the NULL values
riverlines <- pointstolines(riverpoints)

ggplot2::ggplot(tehri_adjusted) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = riverlines[[1]], col = "red") +
  ggplot2::geom_sf(data = riverlines[[2]], col = "blue") +
  ggplot2::theme_void()


## -----------------------------------------------------------------------------
bnb <- basinandbuffers(
  reservoir = tehri_adjusted,
  upstream = riverlines[[1]],
  downstream = riverlines[[2]],
  basins = basins_tehri,
  streambuffersize = 1500,
  reservoirbuffersize = 3000)
buffer <- bnb[[1]]
buffer$area <- c("res", "down", "up")
bufferandclipped <- bnb[[2]]
bufferandclipped$area <- c("res", "down", "up")
ggplot2::ggplot() +
  ggplot2::geom_sf(data = buffer, ggplot2::aes(fill = as.factor(area)), alpha = 0.3) +
  ggplot2::geom_sf(data = bufferandclipped, ggplot2::aes(fill = as.factor(area))) +
  ggplot2::geom_sf(data = tehri_adjusted, fill = "grey") +
  ggplot2::theme_void()
          
          

## -----------------------------------------------------------------------------

# This manual process works for reservoirs where the autogetpourpoints function does not work as expected

### shinyparams <- getshinyparams(res) # sets the app parameters for a specific reservoir
### shinyApp(ui = shinyparams[[1]], server = shinyparams[[3]], options=c(shiny.launch.browser = .rs.invokeShinyPaneViewer))
### pourout <- unname(c(pour, 1)) # assigns pour out to new variable with direction 1 (downstream)
###
### n <- 1 (this is the number of pour points in - i.e. how many rivers join the reservoir)
### i <- 1
### pourin <- data.frame(longitude = rep(0, n), latitude = rep(0,n), direction = rep(0, n))

### # rerun for new pour in points. Not elegant, but you can't lauch a shiny app within a loop.
### shinyApp(ui = shinyparams[[2]], server = shinyparams[[3]], options=c(shiny.launch.browser = .rs.invokeShinyPaneViewer))
### pourin[i,1:2] <- pour
### i <- i + 1
### # rerun up to here
### pourpoints <- rbind(pourout,pourin)
### pourpoints <- st_as_sf(pourpoints, coords = c("longitude", "latitude"))


