test_that("makesystem", {
  dem_system <- rast(system.file("extdata", "dem_system.tif", package="damAOI"))
  system <- damAOI::system %>% st_set_crs(3857)
  names <- unique(system$name)
  sys <- makesystem(names, system, dem_system)
  expect_true(nrow(sys) %in% c(4,5))
  expect_true(length(unique(sys$area)) %in% c(4,5))
})
