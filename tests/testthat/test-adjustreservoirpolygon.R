test_that("adjustreservoirpolygon", {
  tehri_wb <- rast(system.file("extdata", "wb_tehri.tif", package="damAOI"))
  tehri_dem <- rast(system.file("extdata", "dem_tehri.tif", package="damAOI"))
  out <- adjustreservoirpolygon(tehri, tehri_wb, tehri_dem, 20000, 0)
  expect_equal(sum(class(out) == "sf"),1)
  expect_equal(class(out$area),"units")
  expect_length(out, 3)
})
