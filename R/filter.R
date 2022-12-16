
#' @title Filter Datacube
#' @description Filter datacube wraps the filter_labels, filter_temporal, filter_bbox,
#' filter_spatial and array_filter functions into a simulated dplyr's \code{\link[dplyr]{filter}}.
#' @name filter
#' @rdname filter
#' @param .data datacube object from tidyopeneo.
#' @param ... any parameter inherited from dplyr
#' @param .condition (optional) logical. For **array_filter** : A condition that is evaluated
#' against each dimension label in the specified dimension. A dimension label and
#' the corresponding data is preserved for the given dimension, if the condition
#' returns true.
#' @param .extent (optional) For **filter_temporal**, the Left-closed temporal interval, i.e.
#' an array with exactly two elements: The first element is the start of the
#' temporal interval. The specified instance in time is **included** in the
#' interval. The second element is the end of the temporal interval. The
#' specified instance in time is **excluded** from the interval. The specified
#' temporal strings follow [RFC 3339](https://www.rfc-editor.org/rfc/rfc3339.html).
#' Also supports open intervals by setting one of the boundaries to `null`, but
#' never both.
#' For **filter_bbox**, the bounding box, which may include a vertical axis
#' (see `base` and `height`).
#' @param .dimension (optional) For **filter_temporal** : The name of the temporal dimension
#' to filter on. If no specific dimension is specified or it is set to `null`,
#' the filter applies to all temporal dimensions. Fails with a `DimensionNotAvailable`
#' exception if the specified dimension does not exist.
#' @param .geometries (optional) For **filter_spatial** : one or more geometries used for
#' filtering, specified as GeoJSON.
#' @param .context (optional) : any Additional data to be passed to the condition.
#' Mandatory for filter_labels or array_filter processes.
#' @param .con (optional) openeo connection. Default to "https://openeo.cloud"
#' @param .p (optional) processes available at .con
#' @return datacube
#' @import dplyr openeo cli
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr filter
#' @examples
#' library(tidyopeneo)
#' library(sf)
#'
#' dc = datacube(id = "SENTINEL_5P_L2")
#'
#' # filter_temporal and filter_bbox
#' dc = dc %>%
#'     filter(.extent = c("2021-01-01", "2021-03-03")) %>%
#'     filter(.extent = c(west = 6.09, east = 6.99, south = 46.15, north = 46.57))
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
#' dc = dc %>% filter(.geometries = pol)
#'
#' # array_filter
#' # ToDO...
#' # filter_labels
#' # ToDO
#' @export
filter.datacube <- function(.data = NULL, ...,
                            .condition = NULL, .dimension = NULL, .context = NULL,
                            .extent = NULL, .geometries = NULL,
                            .p = openeo::processes(.con), .con = openeo::connect("openeo.cloud")) {

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

  #filter_temporal
  if (length(.extent) == 2 & is.null(.geometries) & is.null(.condition)&
      is.null(.context)){
    dc = .p$filter_temporal(data = .data, extent = .extent, dimension = .dimension)
    cli::cli_alert_success("filter_temporal applied")

    #filter_bbox
  } else if (length(.extent == 4) & is.null(.geometries) & is.null(.condition)&
             is.null(.context)) {
    dc = .p$filter_bbox(data = .data, extent = .extent)
    cli::cli_alert_success("filter_bbox applied")

  } else if (length(.extent != 2) | length(.extent != 4)){
    cli::cli_alert_danger(paste0("for performing a filter_temporal, extent must have length 2,", "\n",
                                 "else for performing a filter_bbox .extent must have length 4.",  "\n",
                                 "Your object has length ", length(.extent)))
  }

  #filter_spatial
  if (!is.null(.geometries) & is.null(.extent) & is.null(.condition) &
      is.null(.context) & is.null(.dimension)) {
    dc = .p$filter_spatial(data = .data, geometries = .geometries)
    cli::cli_alert_success("filter_spatial applied")
  }

  #array_filter
  if (all(is.null(.geometries), is.null(.extent), !is.null(.condition),
          is.null(.dimension))) {
    dc = .p$array_filter(data = .data, condition = .condition, context = .context)
    cli::cli_alert_success("array_filter applied")
  }

  # filter_labels
  if (!is.null(.condition) | !is.null(.dimension) | !is.null(.context)) {

    if (inherits(.dimension, "character", TRUE) == 0){
      cli::cli_alert_danger("dimension arg must be character")}

    if (inherits(.condition, "logical", "TRUE") == 0){
      cli::cli_alert_danger("dimension arg must be logical")}

    dc = .p$filter_labels(.data, condition = .condition,
                               dimension = .dimension, context = .context)
    cli::cli_alert_success("filter_labels applied")
  }

  structure(dc, class = c("datacube", class(dc)))

}
