#default connection handlers
#===============================================================================
#' @name getDefaultDBIHandler
#' @aliases getDefaultDBIHandler
#' @title getDefaultDBIHandler
#' @export
#' @description \code{getDefaultDBI} allows to get a default DBI Handler
#'
#' @usage getDefaultDBIHandler()
#'
#' @examples
#'   getDefaultDBIHandler()
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
getDefaultDBIHandler <- function(){
  return(.dbi.options[["dbi_handler"]])
}

#' @name setDefaultDBIHandler
#' @aliases setDefaultDBIHandler
#' @title setDefaultDBIHandler
#' @export
#' @description \code{setDefaultDBIHandler} allows to set the default DBI handler (function)
#'
#' @usage setDefaultDBIHandler(handler)
#'
#' @param handler a DB connection handler (function)
#'
#' @examples
#'   setDefaultDBIHandler(DB_IOTDB)
#'
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#
setDefaultDBIHandler <- function(handler){
  if(!is(handler, "function")){
    stop("The DBI handler should be a function!")
  }
  .dbi.options$dbi_handler = handler
}
