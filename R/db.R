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
  if (file.exists(db_path)) {
    new_db <- FALSE

    message("File bpfa.db already exists. Would you like to keep a backup of the previous db? \n 1. Yes, create a backup copy of the existing database. \n 2. No, overwrite the old database with a fresh copy.")
    overwrite <- readline("Option 1 or 2: ")
    while(!(overwrite %in% c(1, 2))) {
      message("Sorry, you must enter either 1 (keep backup) or 2 (overwrite).")
      overwrite <- readline("Option 1 or 2: ")
    }

    if (overwrite == 1) {
      # Create backups folder
      dir.create(file.path(db_dir, "Previous db backups"), showWarnings = FALSE)
      # Rename old db to add datetime; copy & move to backup folder
      st <- format(Sys.time(), "%Y%m%d_%H%M") # get system time
      fs::file_move(db_path, paste0(db_dir, "/Previous db backups/", "bpfa_", st, ".db"))
    } else if (overwrite == 2) {
      fs::file_delete(db_path)
    }

  } else {
    new_db <- TRUE
  }

  # Now create the database
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Create base tables
  DBI::dbWriteTable(db, "locations", bpfa::locations, overwrite = TRUE)
  DBI::dbWriteTable(db, "samples", bpfa::samples, overwrite = TRUE)
  DBI::dbCreateTable(db, "pesc_data", fields = c("pesc_id" = 'TEXT',
                                                 "replicate" = 'INTEGER',
                                                 "lims_sample" = 'TEXT',
                                                 "assay" = 'TEXT',
                                                 "ng_per_dry" = 'REAL',
                                                 "dry_batch_id" = 'TEXT',
                                                 "ng_whole" = 'REAL',
                                                 "whole_batch_id" = 'TEXT',
                                                 "lims_ref" = 'TEXT',
                                                 "import_date" = 'TEXT'))
  DBI::dbCreateTable(db, "pesc_benchtop", fields = c("pesc_id" = 'TEXT',
                                                     "tube_no" = 'INTEGER',
                                                     "tube_g" = 'REAL',
                                                     "tube_sample_wet_g" = 'REAL',
                                                     "wet_g" = 'REAL',
                                                     "wet_extracted_g" = "REAL",
                                                     "dilution_solution_ml" = 'REAL',
                                                     "tube_sample_dry_g" = 'REAL',
                                                     "dry_g_per_ml" = 'REAL',
                                                     "dried_g" = 'REAL',
                                                     "prct_dry_weight" = 'REAL',
                                                     "start_date" = 'TEXT',
                                                     "analyst_initials" = 'TEXT',
                                                     "notes" = 'TEXT',
                                                     "import_date" = 'TEXT'))
  DBI::dbCreateTable(db, "pesc_batch", fields = c("pesc_id" = 'TEXT',
                                                  "site" = 'TEXT',
                                                  "sample" = 'TEXT',
                                                  "bag" = 'INTEGER',
                                                  "lims_ref" = 'TEXT',
                                                  "date_collected" = 'TEXT',
                                                  "date_to_pesc" = 'TEXT',
                                                  "import_date" = 'TEXT'))

  # Create views
  # bpfa_pesc_link
  # Table linking bpfa sample_id to pesc_id
  DBI::dbExecute(db, "drop view if exists bpfa_pesc_link;")
  DBI::dbExecute(db, "create view bpfa_pesc_link as
                  select sample_id, pesc_id, pb.site,
                    pb.sample, pb.date_collected, pb.bag,
                    count(*) as n_matches
                  from samples s natural join locations l
                  left join pesc_batch pb
                    on l.site = pb.site
                    and l.waypoint = pb.sample
                    and s.date_collected = pb.date_collected
                  group by sample_id, pesc_id, pb.site, pb.sample, pb.bag;")

  # samples_locs
  # Table merging sample info to location info + pesc_id
  DBI::dbExecute(db, "drop view if exists samples_locs;")
  DBI::dbExecute(db, "create view samples_locs as
                 select s.sample_id, pesc_id,
                    code || '-' || replace(waypoint, '-', '') || '-' || replace(s.date_collected, '-', '') as sample,
                    l.site, waypoint, latitude, longitude,
                    s.date_collected, time_collected, substrate, rain_yn,
                    salinity_ppt, temperature_c, photo_no, sampler,
                    comments
                 from samples s natural join locations l
                    left join bpfa_pesc_link bpl
                  on s.sample_id = bpl.sample_id;")

  # results
  # pesc_data linked to location and sample information
  DBI::dbExecute(db, "drop view if exists results;")
  DBI::dbExecute(db, "create view results as
                  select sample_id, pd.pesc_id, sl.sample,
                    pb.site, pb.sample as waypoint,
                    pb.date_collected, latitude, longitude,
                    time_collected, substrate, rain_yn,
                    salinity_ppt, temperature_c, photo_no,
                    sampler, comments, pd.lims_ref, pb.bag,
                    assay, ng_per_dry, ng_whole
                  from pesc_data pd left join pesc_batch pb
                    on pd.pesc_id = pb.pesc_id
                  left join samples_locs sl
                    on pd.pesc_id = sl.pesc_id;")

  # unmatched_pesc_ids
  # PESC data that Taylor sent that does not line up with any
  # sample data we have
  DBI::dbExecute(db, "drop view if exists unmatched_pesc_ids;")
  DBI::dbExecute(db, "create view unmatched_pesc_ids as
                 select distinct pb.pesc_id, lims_sample, pb.site,
                    pb.sample, pb.bag, pb.lims_ref, pb.date_collected,
                    date_to_pesc
                 from pesc_batch pb left join pesc_data pd
                    on pb.pesc_id = pd.pesc_id
                 left join bpfa_pesc_link bpl
                    on pb.pesc_id = bpl.pesc_id
                 where bpl.pesc_id is null;")

  DBI::dbDisconnect(db)

  if (new_db) {
    message(paste0("🗂 Congratulations, you've created your first copy of the BPFA database at ", crayon::bold$underline(db_path), "!"))
  } else if (new_db == FALSE & overwrite == 1) {
    message(paste0("📥 A new copy of the BPFA database has been created at ", crayon::bold$underline(db_path), "!\n🗃 The old copy of the database has been moved into the ", sQuote(crayon::bold$underline(paste0(db_dir, '/Previous db backups'))), " directory."))
  } else {
    message(paste0("📥 A new copy of the BPFA database has been created at ", crayon::bold$underline(db_path), "!\n✏️ A backup copy of the previous database was ", crayon::bold$italic('not'), " saved. Previous data was overwritten."))
  }

}

#' Save a copy of the BPFA database to your local machine
#'
#' This function will update your local BPFA database with a new copy of the database.
#' For example, if you were emailed a copy of the database with updated data, you would
#' use `copy_bpfa()` to update your local database with the copy you received via email.
#' Your local BPFA database (the one that the R package `{bpfa}` uses) will then be updated
#' and usable with any `{bpfa}` package functions.
#'
#' @param path A file path to the copy of the database you wish to save.
#'
#' @export
#'
#' @examples \dontrun{
#' copy_bpfa("downloads/new_bpfa.db")}
copy_bpfa <- function(path) {
  # Some health checks first
  stopifnot("The provided path must be to an SQLite database, with a file extension ending in '.db'." = grepl(".db$", path))
  stopifnot("The provided path must be a valid filepath." = file.exists(path))

  db_dir <- bpfa_dir()
  db_path <- file.path(db_dir, "bpfa.db")
  create_backup <- ifelse(file.exists(db_path), TRUE, FALSE)

  if (create_backup) {
    # Create backups folder
    dir.create(file.path(db_dir, "Previous db backups"), showWarnings = FALSE)
    # Rename old db to add datetime; copy & move to backup folder
    st <- format(Sys.time(), "%Y%m%d_%H%M") # get system time
    fs::file_move(db_path, paste0(db_dir, "/Previous db backups/", "bpfa_", st, ".db"))
    # Move db to be copied ('path') into pathdir_dir() ('db_dir')
    fs::file_copy(path, db_path)
    message("📥 The database file in ", sQuote(crayon::bold$underline(paste(path))), " was successfully copied to the `bpfa` package. \n🗃 The previous copy of the database has been moved into the ", sQuote(crayon::bold$underline(paste0(db_dir, '/Previous db backups'))), " directory.")
  } else {
    # Move db to be copied ('path') into pathdir_dir() ('db_dir')
    dir.create(file.path(db_dir), showWarnings = FALSE)
    fs::file_copy(path, db_path)
    message("📥 The database file in ", sQuote(crayon::bold$underline(paste(path))), " was successfully copied to the `bpfa` package.")
  }

}

#' Import processed PESC data into the BPFA database
#'
#' After data are read in and cleaned by the `read_lims` function,
#' they can then be added to the three PESC data tables in the BPFA
#' database: `pesc_batch`, `pesc_benchtop`, and `pesc_data`.
#'
#' @param lims_out Output from the read_lims function.
#' @param verbose Logical (T/F). Should the number of new records appended into each table be printed?
#'
#' @export
#'
#' @examples \dontrun{
#' initialize_bpfa()
#' lims <- read_lims('~Documents/path/to/lims/file.xlsx')
#' import_bpfa(lims)
#' }
import_bpfa <- function(lims_out,
                        verbose = TRUE) {
  # Check provided data is correct
  stopifnot("`lims_out` must be a list output produced by `read_lims()`." = inherits(lims_out, "list"))
  stopifnot("`lims_out` must be a list output of length three, produced by `read_lims().`" = (length(lims_out) == 3))
  stopifnot("`lims_out` must be a list output produced by `read_lims() with names 'batch', 'ng data', and 'bench sheet'.`." = all(names(lims_out) %in% c("batch", "ng data", "bench sheet")))

  batch <- lims_out$batch
  ng_dat <- lims_out$`ng data`
  benchtop <- lims_out$`bench sheet`

  # Add import date
  import_date <- Sys.time()
  batch$import_date <- import_date
  ng_dat$import_date <- import_date
  benchtop$import_date <- import_date

  # Normalize tables prior to db import
  benchtop <- dplyr::select(benchtop, -bag)

  # Connect to db
  db_dir <- bpfa_dir()
  db_path <- file.path(db_dir, "bpfa.db")
  stopifnot("There is no bpfa database to connect to yet! Did you run initialize_bpfa() yet?" = file.exists(db_path))
  bpfa <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Check if the records you are trying to import already exist in the db.
  if (scan_for_dupes(db = bpfa, lims_out = lims_out)) {
    # TODO: tests for this
    message("It looks like these samples already exist in the db. Importing them may result in duplicate data in the database. Would you like to continue? \n 1. Yes, I would like to continue importing this data into the database. \n 2. No, I would like to cancel this import.")
    cancel <- readline("Option 1 or 2: ")

    while(!(cancel %in% c(1, 2))) {
      message("Sorry, you must enter either 1 (continue importing) or 2 (cancel import).")
      cancel <- readline("Option 1 or 2: ")
    }

    if (cancel == 1) {
      message("Are you sure? \n 1. Yes, continue importing. \n 2. No, cancel the import.")
      cancel <- readline("Option 1 or 2: ")
    }

    if (cancel == 2) {
      stop("Data import cancelled.")
    }
  }

  # If no dupes found, import the data
  DBI::dbWriteTable(bpfa, "pesc_batch", batch, append = TRUE)
  DBI::dbWriteTable(bpfa, "pesc_benchtop", benchtop, append = TRUE)
  DBI::dbWriteTable(bpfa, "pesc_data", ng_dat, append = TRUE)

  # Font colors for messages
  purple <- crayon::make_style("#AF00AF")
  darkcyan <- crayon::make_style("#00D787")
  fuschia <- crayon::make_style("#FF005F")
  if (verbose) {
    message(paste("Database table", crayon::bold(purple('pesc_batch')), "updated with", nrow(batch), "new records."))
    message(paste("Database table", crayon::bold(fuschia('pesc_benchtop')), "updated with", nrow(benchtop), "new records."))
    message(paste("Database table", crayon::bold(darkcyan('pesc_data')), "updated with", nrow(ng_dat), "new records."))
  }

  DBI::dbDisconnect(bpfa)

}


#' Connect to the BPFA database
#'
#' A simple wrapper for `DBI::dbConnect(RSQLite::SQLite(), file.path(bpfa_dir(), "bpfa.db"))`.
#'
#' @return An SQLiteConnection object.
#' @export
#'
#' @examples \dontrun{
#' bpfa <- connect_bpfa()
#' DBI::dbListTables(bpfa)
#' DBI::dbDisconnect(bpfa)
#' }
connect_bpfa <- function() {
  db_dir <- bpfa_dir()
  db_path <- file.path(db_dir, "bpfa.db")
  stopifnot("There is no bpfa database to connect to yet! Did you run initialize_bpfa() yet?" = file.exists(db_path))
  message("Don't forget to run DBI::dbDisconnect(<database object>) when you are done!")
  DBI::dbConnect(RSQLite::SQLite(), db_path)
}


#' Update BPFA sample metadata
#'
#' @description Sometimes the metadata for samples, e.g. locations or sampling dates,
#' needs to be updated in the database. If the metadata bundled with the package
#' has been updated in a new package release, these functions can be used to
#' quickly write those updates to your local copy of the BPFA database.
#'
#' Note that sample locations are stored in a seperate table from the sample
#' information itself, because multiple samples are collected repeatedly from the
#' sample field sites and waypoints.
#'
#' @return
#' @export
#'
#' @describeIn update_samples Update sample information (e.g., sampling date, sampler, weather conditions)
update_samples <- function() {
  db <- suppressMessages(connect_bpfa())
  data(samples)
  DBI::dbWriteTable(db, "samples", samples, overwrite = TRUE)
  DBI::dbDisconnect(db)
}

#' @describeIn update_samples Update sampling locations (lat/long)
update_locations <- function() {
  db <- suppressMessages(connect_bpfa())
  data(locations)
  DBI::dbWriteTable(db, "locations", locations, overwrite = TRUE)
  DBI::dbDisconnect(db)
}

#' @describeIn update_samples Update both sample information and sample locations
update_metadata <- function() {
  update_samples()
  update_locations()
}
