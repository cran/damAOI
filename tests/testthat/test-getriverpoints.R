test_that("getriverpoints_checks", {
  tehri_fac <- rast(system.file("extdata", "fac_tehri.tif", package="damAOI"))
  tehri_wb <- rast(system.file("extdata", "wb_tehri.tif", package="damAOI"))
  tehri_dem <- rast(system.file("extdata", "dem_tehri.tif", package="damAOI"))
  tehri <- adjustreservoirpolygon(tehri, tehri_wb, tehri_dem, 20000, 0)
  pourpoints <- autogetpourpoints(tehri, tehri_fac)
  ppid <- as.vector(1:nrow(pourpoints), mode = "list")
  riverpoints <- lapply(X = ppid, FUN = getriverpoints, 
                       reservoir = tehri, 
                       pourpoints = pourpoints,
                       river_distance = 10000,
                       ac_tolerance = 50,
                       e_tolerance = 10, 
                       nn = 100, 
                       fac = tehri_fac,
                       dem = tehri_dem)
  riverpoints[sapply(riverpoints, is.null)] <- NULL
  expect_equal(class(riverpoints[[1]]), "data.frame")
  expect_equal(ncol(riverpoints[[1]]), 8)
  riverlines <- pointstolines(riverpoints)
  expect_equal(as.character(droplevels(st_geometry_type(riverlines[[1]]))), "MULTILINESTRING")
  expect_equal(as.character(droplevels(st_geometry_type(riverlines[[2]]))), "LINESTRING")
  })
