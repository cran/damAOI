test_that("adjustreservoirpolygon", {
  tehri_wb <- rast(system.file("extdata", "wb_tehri.tif", package="damAOI"))
  tehri_dem <- rast(system.file("extdata", "dem_tehri.tif", package="damAOI"))
  preprocessed <- preprocessing(
    reservoir = tehri, 
    water_bodies = tehri_wb,
    dem <- tehri_dem,
    river_distance = 20000)
  reservoir_utm <- preprocessed[[1]]
  dem_utm <- preprocessed[[2]]
  water_bodies_utm <- preprocessed[[5]]
  out <- adjustreservoirpolygon(reservoir_utm, 
                                water_bodies_utm, 
                                dem_utm, 
                                poss_expand = 10000, 
                                wbjc = 0)
  expect_equal(sum(class(out) == "sf"),1)
  expect_equal(class(out$area),"units")
  expect_length(out, 3)
})
