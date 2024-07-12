## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%",
  warning = FALSE, 
  message = FALSE
)
require(ggplot2)
require(terra)
require(damAOI)
require(sf)
require(dplyr)

## ----reprojectandalign--------------------------------------------------------

# load package data

fac_tehri <- rast(system.file("extdata", "fac_tehri.tif", package = "damAOI"))
dem_tehri <- rast(system.file("extdata", "dem_tehri.tif", package = "damAOI"))
wb_tehri <- rast(system.file("extdata", "wb_tehri.tif", package = "damAOI"))

# preprocess data

preprocessed <- preprocessing(
    reservoir = tehri, 
    dem = dem_tehri, 
    fac = fac_tehri, 
    water_bodies = wb_tehri,
    basins = basins_tehri, 
    river_distance = 30000)

  tehri_utm <- preprocessed[[1]]
  tehri_dem_utm <- preprocessed[[2]]
  tehri_fac_utm <- preprocessed[[3]]
  basins_tehri_utm <- preprocessed[[4]]
  tehri_wb_utm <- preprocessed[[5]]
  espg <- preprocessed[[7]]

## ----adjust-------------------------------------------------------------------

# adjust original polygon using flow accumulation and water bodies data.

tehri_adjusted <- adjustreservoirpolygon(
  reservoir = tehri_utm, 
  water_bodies = tehri_wb_utm, 
  dem = tehri_dem_utm)

# create a data frame for water body data for plotting
wbdf <- as.data.frame(crop(tehri_wb_utm,tehri_adjusted), xy=T)

ggplot() + 
  geom_sf(data = tehri_adjusted, col = '#228833', fill = NA, lwd = 1) + # adjusted polygon
  geom_sf(data = tehri_utm, col = '#EE6677', fill = NA, lwd = 1) + # original polygon
  geom_tile(data = wbdf, aes(x = x, y = y), fill =  '#4477AA', alpha = 0.5) + # CCI water bodies
  theme_void()


## ----pourpoints---------------------------------------------------------------

# automatically get pour points using reservoir and flow accumulation data 
pourpoints <- autogetpourpoints(
    reservoir = tehri_adjusted, 
    fac = tehri_fac_utm)
st_crs(pourpoints) <- st_crs(tehri_adjusted) 

# convert flow accumulation data to a data frame for plotting

facdf <- as.data.frame(tehri_fac_utm, xy = T)

ggplot() +
  geom_tile(data = facdf, aes(x = x, y = y)) +
  geom_sf(data = tehri_adjusted) +
  geom_sf(data = pourpoints, aes(col = as.factor(direction))) +
  scale_color_manual(
    values = c('#228833','#EE6677'),
    labels = c("Pour in", "Pour out")) +
  theme_void() +
  labs(col = "")


## ----riverpoints--------------------------------------------------------------

riverpoints <- lapply(X = 1:2, # for each of the two pourpoints
                      FUN = getriverpoints, # run the getriverpoints function
                      reservoir = tehri_adjusted, 
                      pourpoints = pourpoints,
                      river_distance = 30000,
                      ac_tolerance = 50,
                      e_tolerance = 10, 
                       nn = 20, 
                      fac = tehri_fac_utm,
                      dem = tehri_dem_utm)

# converts each list of points into an sf LINESTRING

riverlines <- pointstolines(riverpoints, espg = espg)

ggplot(tehri_adjusted) +
  geom_sf() +
  geom_sf(data = riverlines[[1]], col = '#EE6677') +
  geom_sf(data = riverlines[[2]], col = '#4477AA') +
  theme_void()


## ----basins-------------------------------------------------------------------

# creates buffers around river lines and reservoir buffers.

bnb <- basinandbuffers(
  reservoir = tehri_adjusted,
  upstream = riverlines[[1]],
  downstream = riverlines[[2]],
  basins = basins_tehri_utm,
  streambuffersize = 1500,
  reservoirbuffersize = 3000)

ggplot(bnb[[1]] %>% mutate(area = c("res", "down", "up"))) +
  geom_sf(aes(fill = as.factor(area)), alpha = 0.3) +
  geom_sf(data = bnb[[2]] %>% mutate(area = c("res", "down", "up")),
          aes(fill = as.factor(area))) +
  geom_sf(data = tehri_adjusted, fill = "grey") +
  theme_void()

## ----systems------------------------------------------------------------------

# gets names of reservoirs to be combined into the same system

system <- damAOI::system %>% st_set_crs(3857)
names <- unique(system$name)

# Combines AOIs in a dam system

system_corrected <- makesystem(
  names = names, 
  aois = system)

ggplot() +
  geom_sf(data = system_corrected, aes(fill = area))  +
  theme_void() +
  labs(fill = "")

