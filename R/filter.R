
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
#' @param .year integer or list of integers stating the years you want to filter on the
#' datacube.
#' @param .month integer or list of integers referring to the months you want to filter
#' on the datacube.
#' @param .dimension (optional) For **filter_temporal** : The name of the temporal dimension
#' to filter on. If no specific dimension is specified or it is set to `null`,
#' the filter applies to all temporal dimensions. Fails with a `DimensionNotAvailable`
#' exception if the specified dimension does not exist.
#' @param .geometries (optional) For **filter_spatial** : one or more geometries used for
#' filtering, specified as GeoJSON.
#' @param .context (optional) : any Additional data to be passed to the condition.
#' Mandatory for filter_labels or array_filter processes.
#' @param .con (optional) openeo connection. Default to NULL
#' @param .p (optional) processes available at .con
#' @return datacube
#' @import dplyr openeo cli
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr filter
#' @examples
#' library(tidyopeneo)
#' library(sf)
#'
#' con = connect(host = "https://openeo.cloud")
#' dc = datacube(id = "SENTINEL1_GRD")
#'
#' # filter_temporal
#' dc_y = dc %>% filter(.year = c(2020, 2021, 2022))
#'
#' dc_m = dc %>% filter(.month = c(6,7,8))
#'
#' dc_ym = dc %>%  filter(.year = c(2020, 2021, 2022), .month = c(6,7,8))
#'
#' # filter_temporal and filter_bbox
#' dc2 = dc %>%
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
#' dc = dc2 %>% filter(.geometries = pol)
#'
#' # array_filter
#' # ToDO...
#' # filter_labels
#' # ToDO
#' @export
filter.datacube <- function(.data = NULL, ...,
                            .condition = NULL, .dimension = NULL, .context = NULL,
                            .extent = NULL, .geometries = NULL,
                            .year = NULL, .month = NULL,
                            .p = openeo::processes(.con), .con = NULL) {

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

  ## Working with years
  } else if (!is.null(.year) & is.null(.month) & is.null(.geometries) & is.null(.condition)&
             is.null(.context)){

    ### Extract starting year of collection
    process_json = .p$save_result(data = .data, format = list_file_formats()$output$JSON) %>%
      as("Process") %>% openeo::toJSON() %>% rjson::fromJSON() %>% suppressWarnings()
    id = process_json$process_graph[[1]]$arguments$id

    collections = list_collections() %>% suppressWarnings()
    native_time_ext = c(collections[[id]]$extent$temporal[[1]][[1]] %>% as.Date() %>% format("%04Y-%m-%d"),
                        collections[[id]]$extent$temporal[[1]][[2]])
    native_time_ext = ifelse(is.na(native_time_ext), Sys.Date() %>% format("%04Y-%m-%d"), native_time_ext)

    ### stop if year outside of range
    if (min(.year) < native_time_ext[1] %>% substr(1,4) | max(.year) > native_time_ext[2] %>% substr(1,4)){
      stop(
        cli::cli_alert_danger("You're trying to filter outside of the range of the collection {native_time_ext}")
      )
    }

    ### Iterate through the called years
    dcs = list()
    for (i in 1:length(.year)){
     dcs[[i]] <- .p$filter_temporal(
        data = .data,
        extent = c(paste(.year[i], "01-01", sep = "-"), paste(.year[i], "12-31", sep = "-")),
        dimension = .dimension)
    }

    ### merge generated data cubes
    if (length(dcs) > 1){
      for (i in 1:length(dcs)){
        if (i == 1){i = i+1}
        else if (i == 2){
          dc = .p$merge_cubes(dcs[[i]], dcs[[i-1]])
        } else {
          dc = .p$merge_cubes(dc, dcs[[i]])
        }
      }
      cli::cli_alert_success("merge_cubes applied")

      ### Do not merge --- simple filter for a single year
    } else {
      dc = .p$filter_temporal(
        data = .data,
        c(paste(.year, "01-01", sep = "-"), paste(.year, "12-31", sep = "-")),
        dimension = .dimension)
      cli::cli_alert_success("filter_temporal applied")
    }

    ## Working with Months
  } else if (is.null(.year) & !is.null(.month) & is.null(.geometries) & is.null(.condition)&
             is.null(.context)){

    ### Extract time extent of collection
    process_json = .p$save_result(data = .data, format = list_file_formats()$output$JSON) %>%
      as("Process") %>% openeo::toJSON() %>% rjson::fromJSON() %>% suppressWarnings()
    id = process_json$process_graph[[1]]$arguments$id

    collections = list_collections() %>% suppressWarnings()
    native_time_ext = c(collections[[id]]$extent$temporal[[1]][[1]] %>% as.Date() %>% format("%04Y-%m-%d"),
                        collections[[id]]$extent$temporal[[1]][[2]])
    native_time_ext = ifelse(is.na(native_time_ext), Sys.Date() %>% format("%04Y-%m-%d"), native_time_ext)

    ### Iterate through the existing years and called months
    years = seq(native_time_ext[1] %>% substr(1, 4), native_time_ext[2] %>% substr(1, 4), 1)
    months = ifelse(.month < 10, paste0("0", .month), .month) %>% as.character()
    next_months = ifelse((.month + 1) == 13, 1, .month)
    next_months =  ifelse(next_months < 10, paste0("0", next_months), next_months) %>% as.character()
    dcs = list()
    for (y in 1:length(years)){
      for (m in 1:length(months)){
        date1 =  paste(years[y], months[m], "01", sep = "-")
        date2 = paste(years[y], next_months[m], "01", sep = "-")
        date2 = as.Date(date2) - 1
        date2 = date2 %>% format("%04Y-%m-%d")

        if (date1 < native_time_ext[1] | date2 > native_time_ext[2]){
          next
        }

      dcs <- append(dcs, .p$filter_temporal(
        data = .data, extent = c(date1, date2, dimension = .dimension)))
      }
    }
    cli::cli_alert_success("filter_temporal applied")

    ### merge generated data cubes
      for (i in 1:length(dcs)){
        if (i == 1){i = i+1}
        else if (i == 2){
          dc = .p$merge_cubes(dcs[[i]], dcs[[i-1]])
        } else {
          dc = .p$merge_cubes(dc, dcs[[i]])
        }
      }
    cli::cli_alert_success("merge_cubes applied")

    ## Working with Months and Years
  } else if  (!is.null(.year) & !is.null(.month) & is.null(.geometries) & is.null(.condition)&
              is.null(.context)){

    ### Extract time extent of collection
    process_json = .p$save_result(data = .data, format = list_file_formats()$output$JSON) %>%
      as("Process") %>% openeo::toJSON() %>% rjson::fromJSON() %>% suppressWarnings()
    id = process_json$process_graph[[1]]$arguments$id

    collections = list_collections() %>% suppressWarnings()
    native_time_ext = c(collections[[id]]$extent$temporal[[1]][[1]] %>% as.Date() %>% format("%04Y-%m-%d"),
                        collections[[id]]$extent$temporal[[1]][[2]])
    native_time_ext = ifelse(is.na(native_time_ext), Sys.Date() %>% format("%04Y-%m-%d"), native_time_ext)

    ### stop if year outside of range
    if (min(.year) < native_time_ext[1] %>% substr(1,4) | max(.year) > native_time_ext[2] %>% substr(1,4)){
      stop(
        cli::cli_alert_danger("You're trying to filter outside of the range of the collection {native_time_ext}")
      )
    }

    ### Iterate through the existing years and called months
    years = .year
    months = ifelse(.month < 10, paste0("0", .month), .month) %>% as.character()
    next_months = ifelse((.month + 1) == 13, 1, .month)
    next_months =  ifelse(next_months < 10, paste0("0", next_months), next_months) %>% as.character()
    dcs = list()
    for (y in 1:length(years)){
      for (m in 1:length(months)){
        date1 =  paste(years[y], months[m], "01", sep = "-")
        date2 = paste(years[y], next_months[m], "01", sep = "-")
        date2 = as.Date(date2) - 1
        date2 = date2 %>% format(., "%04Y-%m-%d")

        if (date1 < native_time_ext[1] | date2 > native_time_ext[2]){
          next
        }

        dcs <- append(dcs, .p$filter_temporal(
          data = .data, extent = c(date1, date2, dimension = .dimension)))
      }
    }
    cli::cli_alert_success("filter_temporal applied")

    ### merge generated data cubes
    if (length(dcs) > 1){
      for (i in 1:length(dcs)){
        if (i == 1){i = i+1}
        else if (i == 2){
          dc = .p$merge_cubes(dcs[[i]], dcs[[i-1]])
        } else {
          dc = .p$merge_cubes(dc, dcs[[i]])
        }
      }
      cli::cli_alert_success("merge_cubes applied")

      ### Do not merge --- simple filter for a single year
    } else {
      dc = .p$filter_temporal(
        data = .data,
        c(paste(.year, "01-01", sep = "-"), paste(.year, "12-31", sep = "-")),
        dimension = .dimension)
      cli::cli_alert_success("filter_temporal applied")
    }

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
