test_that("group_by() writes a 'datacube' object", {

library(sf)

dc = datacube("SENTINEL_5P_L2") %>%
      slice(.extent = list(west = 6.09, south = 46.15, east = 6.99, north = 46.5)) %>%
      slice(.extent = c("2018-07-01","2018-10-31")) %>%
      filter(.bands = "NO2")

  lon = c(6.09, 6.99)
  lat = c(46.15, 46.5)

  bbox_df = tibble(lon, lat)
  pol = st_polygon(
    list(
      cbind(
         bbox_df$lon[c(1,2,2,1,1)],
         bbox_df$lat[c(1,1,2,2,1)])
        )
  )
  polygons = st_sfc(pol, crs=4326)
  polygons = st_sf(polygons)

  polygons$anAttribute <- 4

  p = openeo::processes()

  dc_mean <- dc %>% group_by(.reducer = function(data, context) { p$mean(data) },
       .geometries = polygons)

  expect_equal(
    all(
      inherits(dc_mean, "datacube"),
      inherits(dc_mean, "ProcessNode")),
    TRUE)
})

