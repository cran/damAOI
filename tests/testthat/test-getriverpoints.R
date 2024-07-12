test_that("getriverpoints_checks", {
  fac_tehri <- rast(system.file("extdata", "fac_tehri.tif", package="damAOI"))
  dem_tehri <- rast(system.file("extdata", "dem_tehri.tif", package="damAOI"))
  wb_tehri <- rast(system.file("extdata", "wb_tehri.tif", package="damAOI"))
  
  pourpoints <- autogetpourpoints(damAOI::tehri, fac_tehri)
  st_crs(pourpoints) <- crs(damAOI::tehri)
  
  preprocessed <- preprocessing(
    reservoir = damAOI::tehri, 
    dem = dem_tehri, 
    fac = fac_tehri, 
    water_bodies = wb_tehri,
    basins = basins_tehri, 
    pourpoints = pourpoints,
    river_distance = 30000)
  tehri_utm <- preprocessed[[1]]
  tehri_dem_utm <- preprocessed[[2]]
  tehri_fac_utm <- preprocessed[[3]]
  basins_tehri_utm <- preprocessed[[4]]
  tehri_wb_utm <- preprocessed[[5]]
  pourpoints_utm <- preprocessed[[6]]
  espg <- preprocessed[[7]]
  ppid <- as.vector(1:nrow(pourpoints_utm), mode = "list")
  riverpoints <- lapply(X = ppid, FUN = getriverpoints, 
                       reservoir = tehri_utm, 
                       pourpoints = pourpoints_utm,
                       river_distance = 10000,
                       ac_tolerance = 50,
                       e_tolerance = 10, 
                       nn = 100, 
                       fac = tehri_fac_utm,
                       dem = tehri_dem_utm)
  riverpoints[sapply(riverpoints, is.null)] <- NULL
  expect_equal(class(riverpoints[[1]]), "data.frame")
  expect_equal(ncol(riverpoints[[1]]), 7)
  riverlines <- pointstolines(riverpoints, espg)
  expect_equal(as.character(droplevels(st_geometry_type(riverlines[[1]]))), "MULTILINESTRING")
  expect_equal(as.character(droplevels(st_geometry_type(riverlines[[2]]))), "LINESTRING")
  })
