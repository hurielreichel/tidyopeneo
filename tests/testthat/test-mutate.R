test_that("mutate() writes a 'datacube' object", {

  dc = datacube(id = "SENTINEL_5P_L2")

  dc_no2 = dc %>%
      slice(.extent = list(west = 6.09, south = 46.15, east = 6.99, north = 46.5)) %>%
      slice(.extent = c("2020-01-01", "2020-04-31")) %>%
      select(.bands = "NO2")

  dc_cloud = dc %>%
      slice(.extent = list(west = 6.09, south = 46.15, east = 6.99, north = 46.5)) %>%
      slice(.extent = c("2020-01-01", "2020-04-31")) %>%
      select(.bands = "CLOUD_FRACTION")

  p = openeo::processes()
  threshold_ <- function(data, context) {
       threshold <- p$gte(data[1], 0.7)
       return(threshold)
  }

  cloud_threshold = dc_cloud %>%
    mutate(.process = threshold_)

  dc_final <- p$mask(dc_no2, cloud_threshold)

  expect_equal(
    all(
      inherits(cloud_threshold, "datacube"),
      inherits(dc_final, "ProcessNode")),
    TRUE)
})
