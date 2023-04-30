#' @title Summarise Datacube
#' @name summarise
#' @rdname summarise
#' @description Summarise datacube wraps the reduce_dimension(https://processes.openeo.org/#reduce_dimension),
#'  function into a simulated dplyr's \code{\link[dplyr]{summarise}}.
#' @param .data datacube object from tidyopeneo.
#' @param ... any parameter inherited from dplyr
#' @param .reducer A reducer to apply on the specified dimension.
#' A reducer is a single process such as ``mean()`` or a set of processes, which
#' computes a single value for a list of values, see the category 'reducer'
#' for such processes.
#' @param .dimension The name of the dimension over which to reduce. Fails with
#' a `DimensionNotAvailable` exception if the specified dimension does not exist.
#' @param .context (optional) Additional data to be passed to the reducer (optional).
#' @param .period (optional) For **aggregate_temporal_period** : The time intervals to aggregate.
#' The following pre-defined values are available:* `hour`: Hour of the day* `day`:
#' Day of the year* `week`: Week of the year* `dekad`: Ten day periods,
#' counted per year with three periods per month (day 1 - 10, 11 - 20 and 21 -
#' end of month). The third dekad of the month can range from 8 to 11 days.
#' For example, the fourth dekad is Feb, 1 - Feb, 10 each year.
#' * `month`: Month of the year* `season`: Three month periods of the calendar
#' seasons (December - February, March - May, June - August, September - November).
#' * `tropical-season`: Six month periods of the tropical seasons (November -
#' April, May - October).* `year`: Proleptic years* `decade`: Ten year periods
#' ([0-to-9 decade](https://en.wikipedia.org/wiki/Decade#0-to-9_decade)), from a
#' year ending in a 0 to the next year ending in a 9.* `decade-ad`: Ten year
#' periods ([1-to-0 decade](https://en.wikipedia.org/wiki/Decade#1-to-0_decade))
#' better aligned with the anno Domini (AD) calendar era, from a year ending in
#' a 1 to the next year ending in a 0.
#' @param .geometries (optional). For **aggregate_spatial** : Geometries as GeoJSON on which
#' the aggregation will be based.
#' One value will be computed per GeoJSON `Feature`, `Geometry` or
#' `GeometryCollection`. For a `FeatureCollection` multiple values will be computed,
#' one value per contained `Feature`. For example, a single value will be computed
#' for a `MultiPolygon`, but two values will be computed for a `FeatureCollection`
#' containing two polygons.- For **polygons**, the process considers all
#' pixels for which the point at the pixel centre intersects with the corresponding
#' polygon (as defined in the Simple Features standard by the OGC).
#' For **points**, the process considers the closest pixel centre.
#' For **lines** (line strings), the process considers all the pixels whose centres
#' are closest to at least one point on the line.Thus, pixels may be part of
#' multiple geometries and be part of multiple aggregations.To maximize
#' interoperability, a nested `GeometryCollection` should be avoided.
#' Furthermore, a `GeometryCollection` composed of a single type of geometries
#' should be avoided in favour of the corresponding multi-part type
#' (e.g. `MultiPolygon`).
#' @param .target_dimension (optional). For **aggregate-spatial** (optional) : The new dimension name
#' to be used for storing the results. Defaults to `result`.
#' @param .intervals (optional). For **aggregate_temporal** : Left-closed temporal intervals,
#' which are allowed to overlap.
#' Each temporal interval in the array has exactly two elements:1.
#' The first element is the start of the temporal interval. The specified instance
#' in time is **included** in the interval.2. The second element is the end of
#' the temporal interval. The specified instance in time is **excluded** from the
#' interval.The specified temporal strings follow
#' RFC 3339(https://www.rfc-editor.org/rfc/rfc3339.html). Although RFC 3339 prohibits
#' the hour to be '24'(https://www.rfc-editor.org/rfc/rfc3339.html#section-5.7),
#' **this process allows the value '24' for the hour** of an end time in order
#' to make it possible that left-closed time intervals can fully cover the day.
#' @param .labels (optional). For **aggregate_temporal** (optional) : Distinct labels for the intervals, which can contain dates
#' and/or times. Is only required to be specified if the values for the start of
#' the temporal intervals are not distinct and thus the default labels would not
#' be unique. The number of labels and the number of groups need to be equal.
#' @param .con (optional) openeo connection. Default to NULL
#' @param .p (optional) processes available at .con
#' @return datacube
#' @import dplyr openeo cli
#' @importFrom dplyr summarise
#' @seealso [openeo::list_processes()]
#' @examples
#' library(tidyopeneo)
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
#'dc = datacube("SENTINEL_5P_L2") %>%
#'    filter(.extent = list(west = w, south = s, east = e, north = n)) %>%
#'    filter(.extent = c(date1, date2)) %>%
#'    select(.bands = "NO2") %>%
#'    summarise(.dimension = "t", .reducer = mean)
#'
#'    #' library(tidyopeneo)
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
#' # reduce temporal dimension
#' dc = datacube("SENTINEL_5P_L2") %>%
#'    filter(.extent = list(west = w, south = s, east = e, north = n)) %>%
#'    filter(.extent = c(date1, date2)) %>%
#'    summarise("sum")
#'
#' @export
summarise.datacube <- function(.data = NULL, .reducer = NULL, ...,
                              .dimension = NULL, .context = NULL, .target_dimension = NULL,
                              .period = NULL, .intervals = NULL, .geometries = NULL,
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

  # if reducer is present, it can be either a function call or a function name as string
  if (!is.null(.reducer)){

    if(inherits(.reducer, "character")){
      reducing_process = .reducer
      .reducer = function(data, context) {.p[[reducing_process]](data)}
    }

  }else{
    stop(cli::format_error("ERROR : no reducer passed or not implemented"))
  }

  # check if there's a by - grouped datacube
  by = attr(.data, "group_env")$group
  if (!is.null(by)){

    # aggregate_temporal_period
    if (any(by %in% c("hour", "day", "week", "dekad", "month", "season", "tropical-season", "year", "decade", "decade-ad"))){
        .period = by
        dc = .p$aggregate_temporal_period(data = .data, period = .period, reducer = .reducer,
                                          dimension = .dimension, context = .context)
        cli::cli_alert_success("aggregate_temporal_period applied")
      }

    # aggregate_temporal
     else if(length(by) > 1 & !inherits(by, "sf")){
      .intervals = by
      dc = .p$aggregate_temporal(data = .data, intervals = .intervals,
                                 reducer = .reducer, dimension = .dimension,
                                 context = .context)
      cli::cli_alert_success("aggregate_temporal applied")

    # reduce_temporal
    } else if(any(by %in% c('time', 'temporal', 't'))){
      .dimension = 't'
      dc = .p$reduce_dimension(data = .data, reducer = .reducer,
                               dimension = .dimension, context = .context)
      cli::cli_alert_success("reduce_dimension applied")

    # reduce_spatial
    } else if(any(by %in% c('space', 'spatial', 's'))){
      .dimension = 's'
      dc = .p$reduce_dimension(data = .data, reducer = .reducer,
                               dimension = .dimension, context = .context)
      cli::cli_alert_success("reduce_dimension applied")

    # aggregate_spatial
    } else if(inherits(by, "sf")){
      .geometries = by
      .target_dimension = "s"
      dc = .p$aggregate_spatial(data = .data, geometries = .geometries,
                                reducer = .reducer, target_dimension = .target_dimension,
                                context = .context)
      cli::cli_alert_success("aggregate_spatial applied")

    } else{
      cli::cli_warn("Aggregation method not found or not applicable")
    }
  # In case the datacube is not group, apply a summarise the same way, but awaits on the arguments
  } else{

    # reduce_dimension
    dc = .p$reduce_dimension(data = .data, reducer = .reducer,
                             dimension = .dimension, context = .context)
    cli::cli_alert_success("reduce_dimension applied")

}

  structure(dc, class = c("datacube", class(dc)))
  return(dc)

}
