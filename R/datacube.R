#' @title Create Datacube Class
#' @description Defines the class datacube based on collection name
#' @param id character containing the collection name to be passed to
#' openeo::processes()$load_collection() (https://processes.openeo.org/#load_collection).
#' This is understood as the **starting point** when working with openeowrap.
#' @return datacube
#' @import openeo
#' @export
datacube=function(id){
  con = openeo::connect(host = "https://openeo.cloud")
  p = openeo::processes()
  dc = p$load_collection(id)
  class(dc) = c(class(dc), "datacube") # Class definition
  dc
}
