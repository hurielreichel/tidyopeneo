#' @title Create Datacube Class
#' @description Defines the class datacube based on collection name
#' @param id character containing the collection name to be passed to
#' openeo::processes()$load_collection() (https://processes.openeo.org/#load_collection).
#' This is understood as the **starting point** when working with tidyopeneo. (optional)
#' @param data ProcessNode datacube from openeo (optional)
#' @return datacube
#' @import openeo
#' @details either data or id must be called, and never both of them. Using id will call an
#' openEO connection and create a datacube object (tidyopeneo object) with the given collection.
#' Defining data is recommended to use once you need to apply a processes that is not included in
#' tidyopeneo and you want to come back to use tidyopeneo functions.
#' @export
datacube=function(id, data = NULL){

  if (is.null(id) & !is.null(data)){
    class(data) = c(class(dc), "datacube") # Class definition
    data
  }else{
    con = openeo::connect(host = "https://openeo.cloud")
    p = openeo::processes()
    dc = p$load_collection(id)
    class(dc) = c(class(dc), "datacube") # Class definition
    dc
  }
}
