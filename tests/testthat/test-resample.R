test_that("resample() creates an object of class 'datacube'", {

  con = connect(host = "https://openeo.cloud")
  dc = datacube("SENTINEL_5P_L2") %>%
    filter(.extent = list(west = 6.09, south = 46.15, east = 6.99, north = 46.5)) %>%
    filter(.extent = c("2018-07-01","2018-10-31")) %>%
    select(.bands = "NO2") %>%
    resample(.resolution = 10/111)

  expect_equal(
    all(
      inherits(dc, "datacube"),
      inherits(dc, "ProcessNode")),
    TRUE)
})
