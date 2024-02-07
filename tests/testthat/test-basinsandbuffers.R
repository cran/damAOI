test_that("basinsandbuffers", {
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
  bnb <- basinandbuffers(
    reservoir = tehri,
    upstream = riverlines[[1]],
    downstream = riverlines[[2]],
    basins = basins_tehri,
    streambuffersize = 1500,
    reservoirbuffersize = 3000)
  expect_length(bnb, 2)
  bnb_clip <- bnb[[2]]
  expect_equal(sum(class(bnb_clip) == "sf"),1)
  expect_equal(sum(st_area(bnb_clip)) > st_area(tehri), TRUE)
})
