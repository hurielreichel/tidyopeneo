#' @title Group by Datacube
#' @description Group by datacube wraps the aggregate_temporal_period(https://processes.openeo.org/#aggregate_temporal_period),
#' aggregate_spatial (https://processes.openeo.org/#aggregate_spatial),
#' and aggregate_temporal(https://processes.openeo.org/#aggregate_temporal),
#'  functions into a simulated dplyr's \code{\link[dplyr]{group_by}}.
#' @param .data datacube object from tidyopeneo
#' @param .period For **aggregate_temporal_period** : The time intervals to aggregate.
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
#' @param .reducer A reducer to be applied for the values contained in each period.
#' A reducer is a single process such as ``mean()`` or a set of processes, which
#' computes a single value for a list of values, see the category 'reducer' for
#' such processes. Periods may not contain any values, which for most reducers
#' leads to no-data (`null`) values by default.
#' @param .dimension For **aggregate_temporal_period** and **aggregate_temporal** (optional) :
#' The name of the temporal dimension for aggregation. All
#' data along the dimension is passed through the specified reducer. If the
#' dimension is not set or set to `null`, the data cube is expected to only
#' have one temporal dimension. Fails with a `TooManyDimensions` exception if
#' it has more dimensions. Fails with a `DimensionNotAvailable` exception if the
#' specified dimension does not exist.
#' @param .context (optional) Additional data to be passed to the reducer.
#' @param .geometries For **aggregate_spatial** : Geometries as GeoJSON on which
#' the aggregation will be based.
#' One value will be computed per GeoJSON `Feature`, `Geometry` or
#' `GeometryCollection`. For a `FeatureCollection` multiple values will be computed,
#' one value per contained `Feature`. For example, a single value will be computed
#' for a `MultiPolygon`, but two values will be computed for a `FeatureCollection`
#' containing two polygons.- For **polygons**, the process considers all
#' pixels for which the point at the pixel center intersects with the corresponding
#' polygon (as defined in the Simple Features standard by the OGC).
#' For **points**, the process considers the closest pixel center.
#' For **lines** (line strings), the process considers all the pixels whose centers
#' are closest to at least one point on the line.Thus, pixels may be part of
#' multiple geometries and be part of multiple aggregations.To maximize
#' interoperability, a nested `GeometryCollection` should be avoided.
#' Furthermore, a `GeometryCollection` composed of a single type of geometries
#' should be avoided in favour of the corresponding multi-part type
#' (e.g. `MultiPolygon`).
#' @param .target_dimension For **aggreagte-spatial** (optional) : The new dimension name
#' to be used for storing the results. Defaults to `result`.
#' @param .intervals For **aggregate_temporal** : Left-closed temporal intervals,
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
#' @param .labels For **aggregate_temporal** (optional) : Distinct labels for the intervals, which can contain dates
#' and/or times. Is only required to be specified if the values for the start of
#' the temporal intervals are not distinct and thus the default labels would not
#' be unique. The number of labels and the number of groups need to be equal.
#' @return datacube
#' @import dplyr openeo cli sf
#' @details If .period is defined, aggregate_temporal_period is run. Else if
#' .geometries is defined, aggregate_spatial runs. Otherwise, if .intervals is passed,
#' aggregate_temporal runs.
#' @seealso [openeo::list_processes()]
#' @examples
#' library(tidyopeneo)
#' library(sf)
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
#'    filter(.bands = "NO2")
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
#' dc_mean <- dc %>% group_by(.reducer = function(data, context) { p$mean(data) },
#'     .geometries = polygons)
#' @export
group_by <- function(.data = NULL, .period = NULL, .reducer = NULL,
                     .dimension = NULL, .context = NULL,
                     .geometries = NULL, .target_dimension = "result",
                     .intervals = NULL, .labels = array()) {
  UseMethod("group_by")
}

#' @rdname group_by
#' @export
group_by.datacube <- function(.data = NULL, .period = NULL, .reducer = NULL,
                              .dimension = NULL, .context = NULL,
                              .geometries = NULL, .target_dimension = "result",
                              .intervals = NULL, .labels = array()
                              ) {

  #con = openeo::connect(host = "https://openeo.cloud")
  p = openeo::processes()

  # aggregate_temporal_period
  if (all(!is.null(.data), !is.null(.period), is.null(.geometries), is.null(.intervals))) {
    dc = p$aggregate_temporal_period(data = .data, period = .period, reducer = .reducer,
                                     dimension = .dimension, context = .context)
    cli::cli_alert_success("aggregate_temporal_period applied")
  }

  # aggregate_spatial
  if (all(!is.null(.data), !is.null(.geometries), is.null(.period), is.null(.intervals))) {
    dc = p$aggregate_spatial(data = .data, geometries = .geometries,
                             reducer = .reducer, target_dimension = .target_dimension,
                             context = .context)
    cli::cli_alert_success("aggregate_spatial applied")
  }

  # aggregate_temporal
  if (all(!is.null(.data), is.null(.geometries), is.null(.period), !is.null(.intervals))) {
    dc = p$aggregate_temporal(data = .data, intervals = .intervals,
                             reducer = .reducer, dimension = .dimension,
                             context = .context)
    cli::cli_alert_success("aggregate_temporal applied")
  }

  class(dc) = c(class(dc), "datacube")

  dc

}
