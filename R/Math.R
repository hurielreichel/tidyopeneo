
# Math.datacube <- function(x, ..., p = openeo::processes()){
#   if (!.Generic %in% names(p)){
#     stop(cli::format_error(paste(.Generic, "is not an available process")))
#   }
#   p[[.Generic]](x)
# }
#
# get_fn <- function(op) {
#   tbl = data.frame(sym = c("==", "!=", "<", ">", "<=", ">="), fn = c("eq", "neq", "lt", "gt", "lte", "gte")) # etc...
#   fn = tbl[match(x, tbl$sym),]$fn
#   if (is.na(fn)){
#     stop(cli::format_error(paste(op, "not available as operator")))
#   }
#   fn
# }
#
# Ops.datacube <- function(e1, e2) {
#   p = openeo::processes() # can't pass as parameter here
#   fn = get_fn(.Generic)
#   if (!fn %in% names(p)){
#     stop(cli::format_error(paste(.Generic, "is not available as a process")))
#   }
#   p[[fn]](e1, e1)
# }

