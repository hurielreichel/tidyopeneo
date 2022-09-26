
#' @export
filter.datacube <- function(.data = NULL,
                            .bands = NULL, .wavelength = NULL,
                            .condition = NULL, .dimension = NULL, .context = NULL) {

  #con = openeo::connect(host = "https://openeo.cloud")
  p = openeo::processes()

  if (is.null(.data)) {
    cli::cli_alert_danger(
      "a datacube of class 'ProcessNode' and 'datacube' from
      tidyopeneo must be passed"
      )}

  if (!is.null(length(.bands)) | !is.null(.wavelength)) {

    if (inherits(class(.bands), "character", TRUE) == 0 |
        inherits(.wavelength, "character", TRUE) == 0){
      cli::format_error("bands or wavelenght args must be character")}

    dc = p$filter_bands(.data, bands = .bands,
                              wavelengths = .wavelength)
    cli::cli_alert_success("filter_bands applied")

  }

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

#' @title Filter Datacube
#' @description Filter datacube wraps the filter_bands and filter_labels function
#' into a simulated dplyr's \code{\link[dplyr]{filter}}.
#' @name filter
#' @rdname filter
#' @param .data datacube object from tidyopeneo.
#' @param .bands For **filter_bands** : character. A list of band names. Either
#' the unique band name (metadata field name in bands) or one of the common band
#' names (metadata field common_name in bands). If the unique band name and the
#' common name conflict, the unique band name has a higher priority.
#' The order of the specified array defines the order of the bands in the data
#' cube. If multiple bands match a common name, all matched bands are included in
#' the original order.
#' @param .wavelength For **filter_bands** : character. A list of sub-lists with
#' each sub-list consisting of two elements. The first element is the minimum
#' wavelength and the second element is the maximum wavelength. Wavelengths are
#' specified in micrometers. The order of the specified array defines the
#' order of the bands in the data cube. If multiple bands match the wavelengths,
#' all matched bands are included in the original order.
#' @param .condition For **filter_labels** : logical. A condition that is evaluated
#' against each dimension label in the specified dimension. A dimension label and
#' the corresponding data is preserved for the given dimension, if the condition
#' returns true.
#' @param .dimension For **filter_labels** (optional) : character The name of the dimension
#'  to filter on. Fails with a DimensionNotAvailable exception if the specified
#'  dimension does not exist.
#' @param .context For **filter_labels** (optional) : any Additional data to be passed to the condition.
#' @return datacube
#' @import dplyr openeo cli
#' @details if args bands and/or wavelengths are defined,
#' openeo::list_processes()$filter_bands
#' will be invoked. If condition and dimensions are defined,
#' openeo::list_processes()$filter_labels
#' will be called.
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr filter
#' @examples
#' library(tidyopeneo)
#' dc = datacube(id = "SENTINEL_5P_L2")
#'
#' dc_no2 <- dc %>% filter(.bands = "NO2")
#' @export

NULL
