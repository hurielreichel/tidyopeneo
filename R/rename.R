#' @title Rename Datacube Dimension
#' @description Rename datacube dimension  wraps the rename_dimension(https://processes.openeo.org/#rename_dimension),
#'  function into a simulated dplyr's \code{\link[dplyr]{rename}}.
#' @name rename
#' @rdname rename
#' @param .data datacube object from tidyopeneo.
#' @param ... any parameter inherited from dplyr
#' @param .source The current name of the dimension. Fails with a
#' `DimensionNotAvailable` exception if the specified dimension does not exist.
#' @param .target A new Name for the dimension. Fails with a `DimensionExists`
#' exception if a dimension with the specified name exists.
#' @param .con (optional) openeo connection. Default to NULL
#' @param .p (optional) processes available at .con
#' @return datacube
#' @import dplyr openeo cli
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr rename
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
#' dc = datacube("SENTINEL_5P_L2") %>%
#'    filter(.extent = list(west = w, south = s, east = e, north = n)) %>%
#'    filter(.extent = c(date1, date2)) %>%
#'    select(.bands = "NO2") %>%
#'    rename(.source = "spatial", .target = "space")
#' @export
rename.datacube <- function(.data = NULL, ..., .source, .target,
                            .p = openeo::processes(.con), .con = NULL) {

  #check dots ...
  if (length(list(...)) > 0) {
    cli::cli_alert_warning("Additional arguments were passed")
  }

  # check mandatory argument
  if (is.null(.data)) {
    stop(cli::format_error(
      "a datacube of class 'ProcessNode' and 'datacube' from
      tidyopeneo MUST be passed"
    ))}

  if (is.null(.source)) {
    stop(cli::format_error(
      "argument .source must be provided"
    ))}

  if (is.null(.target)) {
    stop(cli::format_error(
      "argument .target must be provided"
    ))}

  # rename_dimension
  dc = .p$rename_dimension(data = .data, source = .source,
                          target = .target)
  cli::cli_alert_success("rename_dimension applied")

  structure(dc, class = c("datacube", class(dc)))

}
