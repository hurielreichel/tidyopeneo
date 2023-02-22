test_that("select() writes a 'datacube' object", {
  con = connect(host = "https://openeo.cloud")
  expect_equal(
    all(
      inherits(datacube(id = "SENTINEL_5P_L2") %>% select(.bands = "CO2"), "datacube"),
      inherits(datacube(id = "SENTINEL_5P_L2") %>% select(.bands = "CO2"), "ProcessNode")),
    TRUE)
})
