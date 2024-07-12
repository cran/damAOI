test_that("basinsandbuffers", {
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
  
tehri_adjusted <- adjustreservoirpolygon(tehri_utm, tehri_wb_utm, tehri_dem_utm,20000,0)
pp <- autogetpourpoints(tehri_adjusted, tehri_fac_utm)

ppid <- as.vector(1:nrow(pp), mode = "list")
  riverpoints <- lapply(X = ppid, FUN = getriverpoints, 
                        reservoir = tehri_utm, 
                        pourpoints = pp,
                        river_distance = 100000,
                        ac_tolerance = 50,
                        e_tolerance = 10, 
                        nn = 100, 
                        fac = tehri_fac_utm,
                        dem = tehri_dem_utm)
  riverpoints[sapply(riverpoints, is.null)] <- NULL
  expect_equal(class(riverpoints[[1]]), "data.frame")
  expect_equal(ncol(riverpoints[[1]]), 7)
  riverlines <- pointstolines(riverpoints, espg)
  bnb <- basinandbuffers(
    reservoir = tehri_adjusted,
    upstream = riverlines[[1]],
    downstream = riverlines[[2]],
    basins = basins_tehri_utm,
    streambuffersize = 1500,
    reservoirbuffersize = 3000)
  expect_length(bnb, 2)
  bnb_clip <- bnb[[2]]
  expect_equal(sum(class(bnb_clip) == "sf"),1)
  expect_equal(sum(st_area(bnb_clip)) > st_area(tehri), TRUE)
})
