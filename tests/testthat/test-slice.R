test_that("slice creates object of class 'datacube'", {

  con = connect(host = "https://openeo.cloud")
  dc = datacube(id = "SENTINEL_5P_L2")
  dc_1 = dc %>%
    filter(.extent = c("2021-01-01", "2021-03-03")) %>%
    slice(n = 10)

  dc_2 = dc %>%
    filter(.extent = c("2021-01-01", "2021-03-03")) %>%
    slice(n = -5)

  dc_3 = dc %>%
    filter(.extent = c("2021-01-01", "2021-03-03")) %>%
    slice(prop = 0.55)

  dc_4 = dc %>%
    filter(.extent = c("2021-01-01", "2021-03-03")) %>%
    slice(prop = -0.30)

  expect_equal(all(
    inherits(dc_1, "datacube"),
    inherits(dc_1, "ProcessNode"),
    inherits(dc_2, "datacube"),
    inherits(dc_2, "ProcessNode"),
    inherits(dc_3, "datacube"),
    inherits(dc_3, "ProcessNode"),
    inherits(dc_4, "datacube"),
    inherits(dc_4, "ProcessNode")),
    TRUE)
})
