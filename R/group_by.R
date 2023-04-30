
#' @title Group by Datacube
#' @description Group by datacube works similarly to the dplyr's  \code{\link[dplyr]{group_by}}.
#' It does not truly modifiy the datacube, but it registers a grouping or aggregation
#' strategy. One can aggregate a datacube by its spatial dimension, or maybe its
#' temporal dimension, or even a geometry (sf object).
#'
#' The group_by function interacts directly with summarise and it basically will create
#' a subclass called "grouped datacube", with its aggregation method in its environment.
#' That will be searched by the summarise function, when summarising.
#' @name group_by
#' @rdname group_by
#' @param .data datacube object from tidyopeneo
#' @param ... any parameter inherited from dplyr
#' @param .by aggregation method, such as:
#' 1. "hour", "day", "week", "dekad", "month", "season", "tropical-season", "year", "decade", "decade-ad" for
#' aggregate temporal period.
#'
#' 2. sf object for aggregate spatial
#'
#' 3. list with 2 intervals for aggreggate temporal period
#'
#' 4. 'time', 'temporal', 't' for reduce temporal dimension
#'
#' 5. 'space', 'spatial', 's' for reduce spatial dimension
#' @return grouped datacube
#' @import dplyr openeo sf
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr group_by
#' @examples
#' library(tidyopeneo)
#' library(sf)
#' con = connect(host = "https://openeo.cloud")
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
#'    select(.bands = "NO2")
#'
#' lon = c(w, e)
#' lat = c(s, n)
#'
#' bbox_df = tibble(lon, lat)
#' pol = st_polygon(
#'  list(
#'    cbind(
#'       bbox_df$lon[c(1,2,2,1,1)],
#'       bbox_df$lat[c(1,1,2,2,1)])
#'      )
#' )
#' polygons = st_sfc(pol, crs=4326)
#' polygons = st_sf(polygons)
#'
#' # add any attribute as a workaround
#' polygons$anAttribute <- 4
#'
#' p = openeo::processes()
#'
#' # aggregate spatially
#' dc_mean <- dc %>%
#' group_by(polygons) %>%
#' summarise("mean")
#'
#' # reduce temporal dimension
#' dc_sum <- dc %>%
#' group_by("t") %>%
#' summarise("sum")
#'
#' @export
group_by.datacube <- function(.data = NULL, .by = NULL, ...) {

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

  # add a tag to grouped dc
  # .data$group = .by
  group_env <- environment(.data)
  group_env$group <- .by

  # check if there's a by
  attr(.data, "group_env") <- group_env
  structure(.data, class = unique(c("grouped datacube", class(.data)))) # Class definition
  return(.data)
}
