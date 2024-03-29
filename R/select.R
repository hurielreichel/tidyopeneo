#' @title Select Datacube
#' @description Select datacube wraps the filter_bands function
#' into a simulated dplyr's \code{\link[dplyr]{select}}.
#' @name select
#' @rdname select
#' @param .data datacube object from tidyopeneo.
#' @param ... any parameter inherited from dplyr
#' @param .bands character. A list of band names. Either
#' the unique band name (metadata field name in bands) or one of the common band
#' names (metadata field common_name in bands). If the unique band name and the
#' common name conflict, the unique band name has a higher priority.
#' The order of the specified array defines the order of the bands in the data
#' cube. If multiple bands match a common name, all matched bands are included in
#' the original order.
#' @param .wavelength character. A list of sub-lists with
#' each sub-list consisting of two elements. The first element is the minimum
#' wavelength and the second element is the maximum wavelength. Wavelengths are
#' specified in micrometers. The order of the specified array defines the
#' order of the bands in the data cube. If multiple bands match the wavelengths,
#' all matched bands are included in the original order.
#' @param .con openeo connection. Default to NULL
#' @param .p processes available at .con
#' @return datacube
#' @import dplyr openeo cli
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr select
#' @examples
#' library(tidyopeneo)
#' con = connect(host = "https://openeo.cloud")
#' dc = datacube(id = "SENTINEL_5P_L2")
#'
#' dc_no2 <- dc %>% select(.bands = "NO2")
#' @export
select.datacube <- function(.data = NULL, ...,
                            .bands = NULL, .wavelength = NULL,
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

  if (!is.null(length(.bands)) | !is.null(.wavelength)) {

    if (inherits(class(.bands), "character", TRUE) == 0 |
        inherits(.wavelength, "character", TRUE) == 0){
      cli::format_error("bands or wavelenght args must be character")}

    dc = .p$filter_bands(.data, bands = .bands,
                        wavelengths = .wavelength)
    cli::cli_alert_success("filter_bands applied")

  }

  structure(dc, class = c("datacube", class(dc)))

}

