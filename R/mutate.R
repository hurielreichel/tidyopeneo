#' @title Mutate Datacube
#' @description Mutate Datacube wraps the apply (https://processes.openeo.org/#apply),
#' apply_kernel (https://processes.openeo.org/#apply_kernel),
#' apply_dimension (https://processes.openeo.org/#apply_dimension),
#' and apply_neighborhood (https://processes.openeo.org/#apply_neighborhood)
#' from openeo into a simulated dplyr's \code{\link[dplyr]{mutate}}.
#' @name mutate
#' @rdname mutate
#' @param .data datacube object from tidyopeneo.
#' @param ... any parameter inherited from dplyr
#' @param .process (optional) for **apply** : A process that accepts and returns a single value
#' and is applied on each individual value in the data cube. The process may
#' consist of multiple sub-processes and could, for example, consist of processes
#' such as ``abs()`` or ``linear_scale_range()``.
#' For **apply_kernel** : Process to be applied on all pixel values. The specified
#' process needs to accept an array and must return an array with at least one
#' element. A process may consist of multiple sub-processes.
#' For **apply_neighborhood** : Process to be applied on all neighborhoods.
#' @param .context (optional) Additional data to be passed to the process.
#' @param .kernel (optional) For **apply_kernel** : ernel as a two-dimensional array of
#' weights. The inner level of the nested array aligns with the `x` axis and the
#' outer level aligns with the `y` axis. Each level of the kernel must have an
#' uneven number of elements, otherwise the process throws a
#' `KernelDimensionsUneven` exception.
#' @param .factor (optional) For **apply_kernel** : A factor that is multiplied to each value
#' after the kernel has been applied.This is basically a shortcut for explicitly
#' multiplying each value by a factor afterwards, which is often required for some
#' kernel-based algorithms such as the Gaussian blur.
#' @param .border (optional). For **apply_kernel** : Determines how the data is extended when
#' the kernel overlaps with the borders. Defaults to fill the border with zeroes.
#' The following options are available:
#' * *numeric value* - fill with a user-defined constant number `n`: `nnnnnn|abcdefgh|nnnnnn` (default, with `n` = 0)*
#' `replicate` - repeat the value from the pixel at the border: `aaaaaa|abcdefgh|hhhhhh`*
#' `reflect` - mirror/reflect from the border:
#' `fedcba|abcdefgh|hgfedc`* `reflect_pixel` - mirror/reflect from the center of the pixel at the border:
#' `gfedcb|abcdefgh|gfedcb`* `wrap` - repeat/wrap the image: `cdefgh|abcdefgh|abcdef`
#' @param .replace_invalid For **apply_kernel** : specifies the value to replace
#' non-numerical or infinite numerical values with. By default, those values are
#' replaced with zeroes.
#' @param .dimension (optional) For **apply_dimension** : The name of the source dimension
#' to apply the process on. Fails with a `DimensionNotAvailable` exception if the
#' specified dimension does not exist.
#' @param .target_dimension (optional) For **apply_dimension** (optional): The name of the target
#' dimension or `null` (the default) to use the source dimension specified in the
#' parameter `dimension`.By specifying a target dimension, the source dimension
#' is removed. The target dimension with the specified name and the type `other`
#' (see ``add_dimension()``) is created, if it doesn't exist yet.
#' @param .size (optional) For **apply_neighborhood** : Neighborhood sizes along each dimension.
#' This object maps dimension names to either a physical measure (e.g. 100 m, 10 days)
#' or pixels (e.g. 32 pixels). For dimensions not specified, the default is to provide
#' all values. Be aware that including all values from overly large dimensions may
#' not be processed at once.
#' @param .overlap (optional) For **apply_neighborhood** (optional): Overlap of neighborhoods along
#' each dimension to avoid border effects. For instance a temporal dimension
#' can add 1 month before and after a neighborhood. In the spatial dimensions,
#' this is often a number of pixels. The overlap specified is added before and
#' after, so an overlap of 8 pixels will add 8 pixels on both sides of the window,
#' so 16 in total.Be aware that large overlaps increase the need for computational
#' resources and modifying overlapping data in subsequent operations have no effect.
#' @param .con (optional) openeo connection
#' @param .p (optional) processes available at .con
#' @return datacube
#' @import dplyr openeo cli
#' @details For **apply** define .data, .process, and optionally .context.
#' For #' **apply_kernel** define .data, .kernel, and optionally .factor, .border,
#' and .replace_invalid.
#' For **apply_dimension** define .data, .process, .dimension, and optionally
#' .target_dimension, and .context.
#' For  **apply_neighborhood** define .data, .process, size, and, optionally,
#' .overlap, and .context.
#' @seealso [openeo::list_processes()]
#' @importFrom dplyr mutate
#' @examples
#' library(tidyopeneo)
#'
#' dc = datacube(id = "SENTINEL_5P_L2")
#'
#' # apply
#' ## bounding box
#' w = 6.09
#' s = 46.15
#' e = 6.99
#' n = 46.5
#'
#' ## time extent
#' date1 = "2018-07-01"
#' date2 = "2018-10-31"
#'
#' dc_no2 = datacube("SENTINEL_5P_L2") %>%
#'    filter(.extent = list(west = w, south = s, east = e, north = n)) %>%
#'    filter(.extent = c(date1, date2)) %>%
#'    select(.bands = "NO2")
#'
#' dc_cloud = datacube("SENTINEL_5P_L2") %>%
#'    filter(.extent = list(west = w, south = s, east = e, north = n)) %>%
#'    filter(.extent = c(date1, date2)) %>%
#'    select(.bands = "CLOUD_FRACTION")
#'
#' ## mask for cloud cover
#' p = openeo::processes()
#' threshold_ <- function(data, context) {
#'
#'     threshold <- p$gte(data[1], 0.7)
#' return(threshold)
#' }
#'
#' # apply the threshold to the cube
#' cloud_threshold = dc_cloud %>% mutate(.process = threshold_)
#'
#' #ToDO : apply_kernel, apply_dimension, apply_neighborhood
#' @export
mutate.datacube <- function(.data = NULL, ...,
                                .process = NULL, .context = NULL,
                                .kernel = NULL, .factor = 1, .border = 0,
                                .replace_invalid = 0, .dimension = NULL, .target_dimension = NULL,
                                .size = NULL, .overlap = NULL,
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

  # apply
  if (all(!is.null(.data), !is.null(.process),
          is.null(.kernel), is.null(.dimension), is.null(.target_dimension),
          is.null(.size), is.null(.overlap), is.null(.context))){

    dc = .p$apply(data = .data, process = .process, context = .context)
    cli::cli_alert_success("apply process applied")

  }

  # apply_kernel
  if (all(!is.null(.data), !is.null(.kernel),
          is.null(.process), is.null(.context),
          is.null(.dimension), is.null(.target_dimension),
          is.null(.size), is.null(.overlap))){

    dc = .p$apply_kernel(data = .data, kernel = .kernel,
                        factor = .factor, border = .border, replace_invalid = .replace_invalid)
    cli::cli_alert_success("apply_kernel applied")

  }

  # apply_dimension
  if (all(!is.null(.data), !is.null(.dimension), !is.null(.process),
          is.null(.kernel), is.null(.size), is.null(.overlap))){

    dc = .p$apply_dimension(data = .data, process = .process, dimension = .dimension,
                           target_dimension = .target_dimension, context = .context)
    cli::cli_alert_success("apply_dimension applied")
  }

  # apply_neighborhood
  if (all(!is.null(.data), !is.null(.process), !is.null(.size),
          is.null(.dimension), is.null(.target_dimension))){

    dc = .p$apply_neighborhood(data = .data, process = .process, size = .size,
                              overlap = .overlap, context = .context)
    cli::cli_alert_success("apply_neighborhood applied")

  }

  structure(dc, class = c("datacube", class(dc)))

}
