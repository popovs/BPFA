# Functions to set the db path, initialize the first
# instance of the db, and connect/disconnect to the db

# Code modelled off tidyhydat:
# https://github.com/ropensci/tidyhydat

#' @title Output path to the BPFA sqlite database
#'
#' @description Prints the BPFA sqlite file storage location, regardless of OS, for \link{initialize_bpfa}.
#' E.g., for Mac:
#'
#' `~Library/Application Support/BPFA`
#'
#' If on Windows, either:
#'
#' `C:\\Documents and Settings\<User>\\Application Data\\Local Settings\<AppAuthor>\\BPFA`
#'
#' Or:
#'
#' `C:\\Documents and Settings\<User>\\Application Data\<AppAuthor>\\BPFA`
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

#' Create a new BPFA database
#'
#' This function will create a new, clean copy of the Brunswick Point
#' Fatty Acid (BPFA) database. A new copy of the database comes with
#' the location and sample tables pre-loaded, but no PESC results data.
#'
#' If the function detects that you already have a copy of the BPFA
#' database on your computer, it will ask if you would like to keep
#' a backup copy of the existing database or if you would like to
#' overwrite it instead.
#'
#' @export
#'
#' @examples \dontrun{
#' initialize_bpfa()
#' }
initialize_bpfa <- function() {
  db_dir <- bpfa_dir()
  message("Creating bpfa.db sqlite file in: ", crayon::cyan(db_dir))
  db_path <- file.path(db_dir, "bpfa.db")

  dir.create(db_dir, showWarnings = FALSE)

  # Check if existing db already there  prompt user
  # to keep backup copy or overwrite if exists
  if (exists(db_path)) {
    new_db <- FALSE

    message("File bpfa.db already exists. Would you like to keep a backup of the previous db? \n 1. Yes, create a backup copy of the existing database. \n 2. No, overwrite the old database with a fresh copy.")
    overwrite <- readline("Option 1 or 2: ")
    while(!(overwrite %in% c(1, 2))) {
      message("Sorry, you must enter either 1 (keep backup) or 2 (overwrite).")
      overwrite <- readline("Option 1 or 2: ")
    }

    if (overwrite == 1) {
      # Create backups folder
      dir.create(file.path(db_dir, "backups"), showWarnings = FALSE)
      # Rename old db to add datetime; copy & move to backup folder
      st <- format(Sys.time(), "%Y%m%d_%H%M") # get system time
      fs::file_copy(db_path, paste0(db_dir, "/backups/", "bpfa_", st, ".db"))
    }

  } else {
    new_db <- TRUE
  }

  # Now create the database
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  DBI::dbWriteTable(db, bpfa::locations, overwrite = TRUE)
  DBI::dbWriteTable(db, bpfa::samples, overwrite = TRUE)

  if (new_db) {
    message(paste0("🗂 Congratulations, you've created your first copy of the BPFA database at ", crayon::bold$underline(db_path), "!"))
  } else if (new_db == FALSE & overwrite == 1) {
    message(paste0("📥 A new copy of the BPFA database has been created at ", crayon::bold$underline(db_path), "!\n🗃 The old copy of the database has been moved into the ", sQuote(crayon::bold$underline(paste0(db_dir, '/backups'))), " directory."))
  } else {
    message(paste0("📥 A new copy of the BPFA database has been created at ", crayon::bold$underline(db_path), "!\n✏️ A backup copy of the previous database was ", crayon::bold$italic('not'), " saved. Previous data was overwritten."))
  }

}
