
#' @title Filter Datacube
#' @description Filter datacube wraps the filter_labels function
#' into a simulated dplyr's \code{\link[dplyr]{filter}}.
#' @name filter
#' @rdname filter
#' @param .data datacube object from tidyopeneo.
#' @param ... any parameter inherited from dplyr
#' @param .condition logical. A condition that is evaluated
#' against each dimension label in the specified dimension. A dimension label and
#' the corresponding data is preserved for the given dimension, if the condition
#' returns true.
#' @param .dimension (optional) : character The name of the dimension
#'  to filter on. Fails with a DimensionNotAvailable exception if the specified
#'  dimension does not exist.
#' @param .context (optional) : any Additional data to be passed to the condition.
#' @return datacube
#' @import dplyr openeo cli
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr filter
#' @examples
#' # TODO
#' @export
filter.datacube <- function(.data = NULL, ...,
                            .condition = NULL, .dimension = NULL, .context = NULL) {

  #con = openeo::connect(host = "https://openeo.cloud")
  p = openeo::processes()

  #check dots ...
  dots = list(...)

  for (i in dots){
    if (length(dots) != 0){
      inherits(dots)
    }
  }

  if (is.null(.data)) {
    cli::cli_alert_danger(
      "a datacube of class 'ProcessNode' and 'datacube' from
      tidyopeneo must be passed"
      )}

  if (!is.null(.condition) | !is.null(.dimension) | !is.null(.context)) {

    if (inherits(.dimension, "character", TRUE) == 0){
      cli::cli_alert_danger("dimension arg must be character")}

    if (inherits(.condition, "logical", "TRUE") == 0){
      cli::cli_alert_danger("dimension arg must be logical")}

    dc = p$filter_labels(.data, condition = .condition,
                               dimension = .dimension, context = .context)
    cli::cli_alert_success("filter_labels applied")
  }

  class(dc) = c(class(dc), "datacube")

  dc

}
