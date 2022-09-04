test_that("resample() creates an object of class 'datacube'", {

  dc = datacube("SENTINEL_5P_L2") %>%
    slice(.extent = list(west = 6.09, south = 46.15, east = 6.99, north = 46.5)) %>%
    slice(.extent = c("2018-07-01","2018-10-31")) %>%
    filter(.bands = "NO2") %>%
    resample(.resolution = 10/111)

  expect_equal(
    all(
      inherits(dc, "datacube"),
      inherits(dc, "ProcessNode")),
    TRUE)
})
