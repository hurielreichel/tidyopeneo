#' @title Summarise Datacube
#' @description Summarise datacube wraps the reduce_dimension(https://processes.openeo.org/#reduce_dimension),
#'  function into a simulated dplyr's \code{\link[dplyr]{summarise}}.
#' @param .data datacube object from openeowrap.
#' @param .reducer A reducer to apply on the specified dimension.
#' A reducer is a single process such as ``mean()`` or a set of processes, which
#' computes a single value for a list of values, see the category 'reducer'
#' for such processes.
#' @param .dimension The name of the dimension over which to reduce. Fails with
#' a `DimensionNotAvailable` exception if the specified dimension does not exist.
#' @param .context Additional data to be passed to the reducer (optional).
#' @return datacube
#' @import dplyr openeo cli
#' @seealso [openeo::list_processes()]
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
#'    summarise(.dimension = "t", .reducer = mean)
#' @export
summarise <- function(.data = NULL, .reducer = NULL,
                      .dimension = NULL, .context = NULL) {
  UseMethod("summarise")
}

#' @rdname summarise
#' @export
summarise.datacube <- function(.data = NULL, .reducer = NULL,
                              .dimension = NULL, .context = NULL
) {

  #con = openeo::connect(host = "https://openeo.cloud")
  p = openeo::processes()

  # reduce_dimension
  dc = p$reduce_dimension(data = .data, reducer = .reducer,
                          dimension = .dimension, context = .context)
  cli::cli_alert_success("reduce_dimension applied")

  class(dc) = c(class(dc), "datacube")

  dc

}
