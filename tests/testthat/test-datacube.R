test_that("datacube() works", {

  con = connect(host = "https://openeo.cloud")
  p = processes()

  dc = p$load_collection(id = "SENTINEL1_GRD")

  dc = datacube(dc)

  expect_equal(
    all(
      inherits(datacube("SENTINEL1_GRD"), "datacube"),
      inherits(datacube("SENTINEL1_GRD"), "ProcessNode"),
      inherits(dc, "datacube"),
      inherits(dc, "ProcessNode")),
      TRUE)
})

