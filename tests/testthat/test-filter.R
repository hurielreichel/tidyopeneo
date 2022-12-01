test_that("filter creates object of class 'datacube'", {

  library(sf)
  dc = datacube(id = "SENTINEL_5P_L2")
  dc_1 = dc %>%
    filter(.extent = c("2021-01-01", "2021-03-03")) %>%
    filter(.extent = c(west = 6.09, east = 6.99, south = 46.15, north = 46.57))

  lon = c(6.22, 6.24)
  lat = c(46.20, 46.25)
  pol_coords = dplyr::tibble(lon, lat)
  pol <- pol_coords %>%
    st_as_sf(coords = c("lon", "lat"), crs = 3857) %>%
    st_bbox() %>%
    st_as_sfc()

  dc_2 = dc %>%
    filter(.geometries = pol)

  expect_equal(all(
    inherits(dc_1, "datacube"),
    inherits(dc_1, "ProcessNode"),
    inherits(dc_2, "datacube"),
    inherits(dc_2, "ProcessNode")),
    TRUE)
})

