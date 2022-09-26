
#' @export
slice.datacube <- function(.data = NULL,
                           .extent = NULL, .dimension = NULL,
                           .geometries = NULL,
                           .condition = NULL, .context = NULL) {

  #con = openeo::connect(host = "https://openeo.cloud")
  p = openeo::processes()

  #filter_temporal
  if (length(.extent) == 2 & is.null(.geometries) & is.null(.condition)&
      is.null(.context)){
    dc = p$filter_temporal(data = .data, extent = .extent, dimension = .dimension)
    cli::cli_alert_success("filter_temporal applied")

  #filter_bbox
  } else if (length(.extent == 4) & is.null(.geometries) & is.null(.condition)&
             is.null(.context)) {
    dc = p$filter_bbox(data = .data, extent = .extent)
    cli::cli_alert_success("filter_bbox applied")

  } else if (length(.extent != 2) | length(.extent != 4)){
    cli::cli_alert_danger(paste0("if willing to perform a filter_temporal, extent must be length 2,", "\n",
    "else if willing to perform a filter_bbox length extension must be 4.",  "\n",
    "You object has length ", length(.extent)))
  }

  #filter_spatial
  if (!is.null(.geometries) & is.null(.extent) & is.null(.condition) &
      is.null(.context) & is.null(.dimension)) {
    dc = p$filter_spatial(data = .data, geometries = .geometries)
    cli::cli_alert_success("filter_spatial applied")
  }

  #array_filter
  if (all(is.null(.geometries), is.null(.extent), !is.null(.condition),
      is.null(.dimension))) {
    dc = p$array_filter(data = .data, condition = .condition, context = .context)
    cli::cli_alert_success("array_filter applied")

  }
  class(dc) = c(class(dc), "datacube")

  dc

}

#' @title Slice Datacube
#' @description Slice datacube wraps the filter_temporal, filter_bbox,
#' filter_spatial and array_filter functions into a simulated dplyr's \code{\link[dplyr]{slice}}.
#' @name slice
#' @rdname slice
#' @param .data datacube object from tidyopeneo. For
#' array_filter : an array.
#' @param .extent For **filter_temporal**, the Left-closed temporal interval, i.e.
#' an array with exactly two elements: The first element is the start of the
#' temporal interval. The specified instance in time is **included** in the
#' interval. The second element is the end of the temporal interval. The
#' specified instance in time is **excluded** from the interval. The specified
#' temporal strings follow [RFC 3339](https://www.rfc-editor.org/rfc/rfc3339.html).
#' Also supports open intervals by setting one of the boundaries to `null`, but
#' never both.
#' For **filter_bbox**, the bounding box, which may include a vertical axis
#' (see `base` and `height`).
#' @param .dimension For **filter_temporal** : The name of the temporal dimension
#' to filter on. If no specific dimension is specified or it is set to `null`,
#' the filter applies to all temporal dimensions. Fails with a `DimensionNotAvailable`
#' exception if the specified dimension does not exist.
#' @param .geometries For **filter_spatial** : one or more geometries used for
#' filtering, specified as GeoJSON.
#' @param .condition For **array_filter** : A condition that is evaluated against
#' each value, index and/or label in the array. Only the array elements for
#' which the condition returns `true` are preserved.
#' @param .context For **array_filter** (optional): Additional data to be passed to the condition.
#' @return datacube
#' @import dplyr openeo cli sf
#' @importFrom dplyr slice
#' @details if arg extent is defined as a length two vector, filter_temporal is
#' gonna be called. If it has length of four, filter_bbox is gonna be called.
#' In case the argument geometries is defined, filter_spatial is gonna run, and,
#' finally, if .data is an array, array_filter will be wrapped.
#' @seealso [openeo::list_processes()]
#' @examples
#' library(tidyopeneo)
#' library(sf)
#'
#' dc = datacube(id = "SENTINEL_5P_L2")
#'
#' # filter_temporal and filter_bbox
#' dc = dc %>%
#'     slice(.extent = c("2021-01-01", "2021-03-03")) %>%
#'     slice(.extent = c(west = 6.09, east = 6.99, south = 46.15, north = 46.57))
#'
#' # filter_spatial
#' lon = c(6.22, 6.24)
#' lat = c(46.20, 46.25)
#' pol_coords = dplyr::tibble(lon, lat)
#' pol <- pol_coords %>%
#'     st_as_sf(coords = c("lon", "lat"), crs = 3857) %>%
#'     st_bbox() %>%
#'     st_as_sfc()
#'
#' dc = dc %>% slice(.geometries = pol)
#'
#' # array_filter
#' # ToDO...
#' @export

NULL
