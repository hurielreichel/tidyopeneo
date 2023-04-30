#' @title Slice Datacube
#' @description Slice datacube wraps the filter_temporal, just as filter.datacube does.
#' into a simulated dplyr's \code{\link[dplyr]{slice}}. The "n" from dplyr's slice is
#' interpreted as "days".
#' @name slice
#' @rdname slice
#' @param .data datacube object from tidyopeneo.
#' @param ... any parameter inherited from dplyr
#' @param n integer. Provide either n, the number of days, or prop, the proportion of available days to select.
#' If neither are supplied, n = 1 will be used. If a negative value of n or prop is provided, the specified
#' number or proportion of days will be deselected.
#'
#' If n is greater than the number of days available in the collection or datacube (or prop > 1), the result
#' will be truncated to the group size. If the proportion of a group size does not yield an integer number of
#'rows, the absolute value of prop * ndays(.data) is rounded down.
#' @param prop numeric Provide either n, the number of days, or prop, the proportion of available days to select.
#' If neither are supplied, n = 1 will be used. If a negative value of n or prop is provided, the specified
#' number or proportion of days will be deselected.
#'
#' If n is greater than the number of days available in the collection or datacube (or prop > 1), the result
#' will be truncated to the group size. If the proportion of a group size does not yield an integer number of
#'rows, the absolute value of prop * ndays(.data) is rounded down.
#' @param .con (optional) openeo connection. Default to NULL
#' @param .p (optional) processes available at .con
#' @return datacube
#' @import dplyr openeo cli
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr slice
#' @importFrom rjson fromJSON
#' @importFrom methods as
#' @examples
#' library(tidyopeneo)
#' con = connect(host = "https://openeo.cloud")
#'
#' dc = datacube("TERRASCOPE_S5P_L3_NO2_TD_V1")
#'
#' dc1 = dc %>%
#'     filter(.extent = c("2021-01-01", "2021-03-03")) %>%
#'     slice(n = 3)
#'
#' dc2 = dc %>% slice(n = -30)
#'
#' dc3 = dc %>%
#'     filter(.extent = c("2021-05-01", "2021-09-31")) %>%
#'     slice(prop = 0.55)
#' @export
slice.datacube <- function(.data = NULL, ...,
                  n = 1, prop = NULL,
                  .p = openeo::processes(.con), .con = NULL){

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

  if(any(n == 0, prop == 0)){
    stop(cli::format_error("either n or prop are zero, this cannot be"))
  }

  # datacube may have filter temporal applied (more likely)
  process_json =  .p$save_result(data = .data, format = list_file_formats()$output$JSON) %>%
    as("Process") %>% openeo::toJSON() %>% rjson::fromJSON() %>% suppressWarnings()
  time_ext = process_json$process_graph$filter_temporal$arguments$extent

  # if not, datacube may have temporal extent
  if (is.null(time_ext)){
    time_ext = process_json$process_graph$load_collection$arguments$temporal_extent
  }

  # otherwise get from collection
  if (is.null(time_ext)){
    describe = describe_collection(process_json$process_graph$load_collection$arguments$id)
    time_ext = describe$extent$temporal %>% unlist() %>% as.Date() %>% as.character()
  }

  # safer for NA in end date
  if (is.na(time_ext[2])){time_ext[2] = Sys.Date() %>% as.character()}

  ###############################################################################

  # use prop
  if( all( is.null(n), !is.null(prop), prop > 0) ){

    n = ((as.Date(time_ext[2]) - as.Date(time_ext[1])) %>% as.integer() * prop) %>% as.integer()

  }

  # use negative prop
  if( all( is.null(n), !is.null(prop), prop < 0) ){

    n = -1 * (((as.Date(time_ext[2]) - as.Date(time_ext[1])) %>% as.integer() * prop) %>% as.integer())

  }

  # use n
  if ( all( !is.null(n), is.null(prop), n > 0) ){

    # calculate new temporal extent using n as the number of days
    new_end = ((time_ext[1] %>% as.Date()) + as.integer(n)) %>% as.character()

    # safer for when new end is further than the one from data
    if (new_end > time_ext[2]){
      new_end = time_ext[2]
      cli::cli_alert_danger("n slices the cube further than it has data,
                              truncating it to the nearest date available")
    }

    time_ext = c(time_ext[1], new_end)

  }

  # use negative n
  if( all(!is.null(n), is.null(prop), n < 0) ){

    # calculate new temporal extent using n as the number of days
    new_start = (time_ext[2] %>% as.Date()) + as.integer(n) # attention to the "+"

    # safer for NA starting date and new start further than the existing one
    if (new_start < time_ext[1]){
      new_start = time_ext[1]
      cli::cli_alert_danger("n slices the cube further than it has data,
                              truncating it to the nearest date available")
    }

    time_ext = c(new_start, time_ext[2])

}

dc = .data %>% filter(.extent = time_ext)

}
