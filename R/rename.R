
#' @export
rename.datacube <- function(.data = NULL, .source, .target
) {

  #con = openeo::connect(host = "https://openeo.cloud")
  p = openeo::processes()

  # rename_dimension
  dc = p$rename_dimension(data = .data, source = .source,
                          target = .target)
  cli::cli_alert_success("rename_dimension applied")

  class(dc) = c(class(dc), "datacube")

  dc

}

#' @title Rename Datacube Dimension
#' @description Rename datacube dimension  wraps the rename_dimension(https://processes.openeo.org/#rename_dimension),
#'  function into a simulated dplyr's \code{\link[dplyr]{rename}}.
#' @name rename
#' @rdname rename
#' @param .data datacube object from tidyopeneo.
#' @param .source The current name of the dimension. Fails with a
#' `DimensionNotAvailable` exception if the specified dimension does not exist.
#' @param .target A new Name for the dimension. Fails with a `DimensionExists`
#' exception if a dimension with the specified name exists.
#' @return datacube
#' @import dplyr openeo cli
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr rename
#' @examples
#' library(tidyopeneo)
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
#' dc = datacube("SENTINEL_5P_L2") %>%
#'    slice(.extent = list(west = w, south = s, east = e, north = n)) %>%
#'    slice(.extent = c(date1, date2)) %>%
#'    filter(.bands = "NO2") %>%
#'    rename(.source = "spatial", .target = "space")
#' @export

NULL
