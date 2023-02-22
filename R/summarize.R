#' @title Summarize Datacube
#' @description Summarize datacube wraps the reduce_dimension(https://processes.openeo.org/#reduce_dimension),
#'  function into a simulated dplyr's \code{\link[dplyr]{summarize}}.
#' @name summarize
#' @rdname summarize
#' @param .data datacube object from tidyopeneo.
#' @param ... any parameter inherited from dplyr
#' @param .reducer character or function. A reducer to apply on the specified dimension.
#' A reducer is a single process such as "mean or a set of processes, which
#' computes a single value for a list of values, see the category 'reducer'
#' for such processes.
#' @param .dimension The name of the dimension over which to reduce. Fails with
#' a `DimensionNotAvailable` exception if the specified dimension does not exist.
#' @param .context (optional) Additional data to be passed to the reducer (optional).
#' @param .con (optional) openeo connection. Default to NULL
#' @param .p (optional) processes available at .con
#' @return datacube
#' @import dplyr openeo cli
#' @importFrom dplyr summarize
#' @seealso [openeo::list_processes()]
#' @examples
#' library(tidyopeneo)
#' con = connect(host = "https://openeo.cloud")
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
#'    filter(.extent = list(west = w, south = s, east = e, north = n)) %>%
#'    filter(.extent = c(date1, date2)) %>%
#'    select(.bands = "NO2") %>%
#'    summarize(.dimension = "t", .reducer = mean)
#' @export
summarize.datacube <- function(.data = NULL, ..., .reducer = NULL,
                               .dimension = NULL, .context = NULL,
                               .p = openeo::processes(.con), .con = NULL) {

  #check dots ...
  dots = list(...)

  for (i in dots){
    if (length(dots) != 0){
      inherits(dots)
    }
  }

  # check mandatory argument
  if (is.null(.data)) {
    stop(cli::format_error(
      "a datacube of class 'ProcessNode' and 'datacube' from
      tidyopeneo MUST be passed"
    ))}

  # reduce_dimension
  dc = .p$reduce_dimension(data = .data, reducer = .reducer,
                          dimension = .dimension, context = .context)
  cli::cli_alert_success("reduce_dimension applied")

  structure(dc, class = c("datacube", class(dc)))

}
