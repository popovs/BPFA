# Functions to set the db path, initialize the first
# instance of the db, and connect/disconnect to the db

# Code modelled off tidyhydat:
# https://github.com/ropensci/tidyhydat

#' @title Output path to the BPFA sqlite database
#'
#' @description Prints the BPFA sqlite file storage location, regardless of OS, for \link{initialize_bpfa}.
#'
#' @param ... arguments potentially passed to \code{rappdirs::user_data_dir}.
#'
#' @export
#'
#' @examples \dontrun{
#' bpfa_dir()
#' }
bpfa_dir <- function(...) {
  path.expand(
    rappdirs::user_data_dir(appname = "BPFA", ...)
  )
}
