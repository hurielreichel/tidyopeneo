test_that("filter() writes a 'datacube' object", {
  expect_equal(
    all(
    inherits(datacube(id = "SENTINEL_5P_L2") %>% filter(.bands = "CO2"), "datacube"),
    inherits(datacube(id = "SENTINEL_5P_L2") %>% filter(.bands = "CO2"), "ProcessNode")),
    TRUE)
})
