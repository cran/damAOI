test_that("getimpactedarea", {
  fac_tehri <- rast(system.file("extdata", "fac_tehri.tif", package="damAOI"))
  dem_tehri <- rast(system.file("extdata", "dem_tehri.tif", package="damAOI"))
  wb_tehri <- rast(system.file("extdata", "wb_tehri.tif", package="damAOI"))

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
  
tehri_adjusted <- adjustreservoirpolygon(tehri_utm, tehri_wb_utm, tehri_dem_utm, 20000, 0)
pourpoints_utm <- autogetpourpoints(tehri_adjusted, tehri_fac_utm)

aoi <- getimpactedarea(
  reservoir = tehri_utm,
  water_bodies = tehri_wb_utm,
  pourpoints = pourpoints_utm,
  dem = tehri_dem_utm,
  fac = tehri_fac_utm,
  basins = basins_tehri_utm,
  toprocess = FALSE,
  espg = espg,
  toadjust = FALSE,
  poss_expand = 10000,
  river_distance = 10000,
  nn = 100,
  ac_tolerance = 2,
  e_tolerance = 5,
  streambuffersize = 2000,
  reservoirbuffersize = 5000,
  wbjc = 0)

expect_equal(nrow(aoi), 3)
expect_equal(sum(class(aoi) == "sf"),1)
})
