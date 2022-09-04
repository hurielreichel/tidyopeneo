#' @title Resample Datacube
#' @description Resample datacube wraps the resample_spatial (https://processes.openeo.org/#resample_spatial),
#' resample_cube_spatial (https://processes.openeo.org/#resample_cube_spatial),
#' and resample_cube_temporal(https://processes.openeo.org/#resample_cube_temporal),
#'  functions into a newly created resample function, therefore simplifying it.
#' @param .data datacube object from openeowrap
#' @param .resolution For **resample_spatial** : Resamples the data cube to the
#' target resolution, which can be specified either as separate values for x and
#' y or as a single value for both axes. Specified in the units of the target
#' projection. Doesn't change the resolution by default (`0`).
#' @param .projection For **resample_spatial** : Warps the data cube to the
#' target projection, specified as as EPSG code(http://www.epsg-registry.org/),
#' WKT2 (ISO 19162) string (http://docs.opengeospatial.org/is/18-010r7/18-010r7.html,
#' PROJ definition (deprecated) (https://proj.org/usage/quickstart.html), By default
#' (`null`), the projection is not changed.
#' @param .method For **resample_spatial** or **resample_cube_spatial**: Resampling method to use. The
#' following options are available and are meant to align with
#' `gdalwarp` (https://gdal.org/programs/gdalwarp.html#cmdoption-gdalwarp-r) :
#' * `average`: average (mean) resampling, computes the weighted average of all valid pixels*
#' `bilinear`: bilinear resampling* `cubic`: cubic resampling* `cubicspline`:
#' cubic spline resampling* `lanczos`: Lanczos windowed sinc resampling* `max`:
#' maximum resampling, selects the maximum value from all valid
#' pixels* `med`: median resampling, selects the median value of all valid pixels*
#' `min`: minimum resampling, selects the minimum value from all valid pixels*
#' `mode`: mode resampling, selects the value which appears most often of all the
#' sampled points* `near`: nearest neighbour resampling (default)* `q1`:
#' first quartile resampling, selects the first quartile value of all valid
#' pixels* `q3`: third quartile resampling, selects the third quartile value
#' of all valid pixels* `rms` root mean square (quadratic mean) of all valid
#' pixels* `sum`: compute the weighted sum of all valid pixels Valid
#' pixels are determined based on the function ``is_valid()``.
#' @param .align For **resample_spatial** : Specifies to which corner of the spatial
#' extent the new resampled data is aligned to.
#' @param .target For **resample_cube_spatial** : A data cube that describes the
#' spatial target resolution. For **resample_cube_temporal** : A data cube that
#' describes the temporal target resolution.
#' @param .dimension For **resample_cube_temporal** (optional) : he name of the temporal
#' dimension to resample, which must exist with this name in both data cubes.
#' If the dimension is not set or is set to `null`, the process resamples all
#' temporal dimensions that exist with the same names in both data cubes.The
#' following exceptions may occur:* A dimension is given, but it does not exist
#' in any of the data cubes: `DimensionNotAvailable`* A dimension is given, but
#' one of them is not temporal: `DimensionMismatch`* No specific dimension name
#' is given and there are no temporal dimensions with the same name in the
#' data: `DimensionMismatch`.
#' @param .valid_within For **resample_cube_temporal** (optional): Setting this parameter
#' to a numerical value enables that the process searches for valid values within
#' the given period of days before and after the target timestamps. Valid values
#' are determined based on the function ``is_valid()``. For example, the limit
#' of `7` for the target timestamps `2020-01-15 12:00:00` looks for a nearest
#' neighbor after `2020-01-08 12:00:00` and before `2020-01-22 12:00:00`.
#' If no valid value is found within the given period, the value will be set
#' to no-data (`null`).
#' @param .target_process either "spatial" to apply **resample_cube_spatial** or
#' "temporal" to apply **resample_cube_temporal**.
#' @return datacube
#' @details if arg .target is not defined, resample_spatial is gonna be run.
#' Else, if "spatial" is set as .target_process, resample_cube_spatial is gonna
#' be run, or if "temporal" is set, resample_cube_temporal is gonna be run.
#' @seealso [openeo::list_processes()]
#' @import dplyr openeo cli
#' @examples
#' library(openeowrap)
#'
#' # bounding box
#' w = 6.09
#' s = 46.15
#' e = 6.99
#' n = 46.5
#'
#' ## time extent
#' date1 = "2018-07-01"
#' date2 = "2018-10-31"
#'
#'dc = datacube("SENTINEL_5P_L2") %>%
#'    slice(.extent = list(west = w, south = s, east = e, north = n)) %>%
#'    slice(.extent = c(date1, date2)) %>%
#'    filter(.bands = "NO2") %>%
#'    resample(.resolution = 10/111)
#' @export
resample <- function(.data = NULL, .resolution = 0, .projection = NULL,
                     .method = "near", .align = "upper-left",
                     .target = NULL, .dimension = NULL,
                     .valid_within = NULL, .target_process = NULL) {
  UseMethod("resample")
}

#' @rdname resample
#' @export
resample.datacube <- function(.data = NULL, .resolution = 0, .projection = NULL,
                              .method = "near", .align = "upper-left",
                              .target = NULL, .dimension = NULL,
                              .valid_within = NULL, .target_process = NULL) {

  #con = openeo::connect(host = "https://openeo.cloud")
  p = openeo::processes()

  # resample_spatial
  if (all(!is.null(.data), is.null(.target))) {
    dc = p$resample_spatial(data = .data,
                            resolution = .resolution, projection = .projection,
                            method = .method, align = .align)
    cli::cli_alert_success("resample_spatial applied")
  }

  # cube process
  if (all(!is.null(.data), !is.null(.target))) {

    # resample_cube_spatial
    if (.target_process == "spatial"){ # cube process

      dc = p$resample_cube_spatial(data = .data, target = .target, method = .method)
      cli::cli_alert_success("resample_cube_spatial applied")

    # resample_cube_temporal
    } else if (.target_process == "spatial") {

      dc = p$resample_cube_temporal(data = .data, target = .target,
                                    dimension = .dimension,
                                    valid_within = .valid_within)
      cli::cli_alert_success("resample_cube_temporal applied")

    # internal argument must be passed when passing cube process
    } else {

      cli::cli_alert_danger(paste(".target_process must be either 'spatial' or 'temporal' in",
                "", "order to define the openeo function to run resample_cube_spatial",
                "", "or resample_cube_temporal."))

    }}

  class(dc) = c(class(dc), "datacube")

  dc

}
