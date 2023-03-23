#' Clean PESC site names
#'
#' Site names are slightly different and formatted differently
#' between the sample sheets PESC sends vs the site names in
#' the BPFA location table. This function quickly cleans and
#' standardizes PESC site names to match BPFA site names.
#'
#' @param x A string of PESC site names.
#'
#' @return A cleaned string containing BPFA site names.
#' @export
clean_locs <- function(x) {
  stopifnot("x must be a character vector." = inherits(x, "character"))

  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_to_title(x)

  # Replace specific issues
  x <- gsub("North|South", "", x)
  x <- gsub("Chl-A", "", x)
  x <- gsub("Foreshore", "", x)
  x <- gsub("Banks", "Bank", x)
  x <- gsub("Roberts Bank", "Brunswick Point", x)
  x <- gsub("^Cowichan$", "Cowichan Bay", x)

  # Trim up again
  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_to_title(x)

  return(x)
}


#' Clean PESC sample names
#'
#' PESC sample names do not match BPFA sample names and need to be
#' cleaned prior to being matched to BPFA sample metadata. This
#' function extracts out the waypoint number, bag number (if present),
#' replicate (if present), and cleans up any superfluous text/characters.
#'
#' Some samples are run multiple times, e.g. have are replicated.
#' In these cases PESC will append the word "dup" to the end of the
#' sample name to indicate it is a duplicate. If duplicate samples
#' are detected, the function will note that the sample is a replicate.
#'
#' PESC often reports sample locations as "TOFINO"; however, BPFA
#' locations differentiate between Jensen's Bay and Maltby Slough.
#' PESC deals with this by appending "JB" or "MS" to the start of
#' sample names. If the function detects if Jensen's Bay or Maltby Slough
#' location information is contained within the sample name string,
#' the function returns this information in the 'jb' and 'ms' column.
#'
#' @param x A string of PESC sample names.
#'
#' @return A dataframe consisting of the original sample name, sample bag number (if detected), cleaned sample number, replicate status (if detected), and two logical columns indicating if the sample was collected in Jensen's Bay (`jb == TRUE`) or Maltby Slough (`mb == TRUE`).
#' @export
clean_samples <- function(x) {
  stopifnot("x must be a character vector." = inherits(x, "character"))

  x <- toupper(x)
  x <- as.data.frame(x)

  # Extract bag
  x$bag <- stringr::str_extract(x$x, "BAG\\d+|BAG\\s+\\d+")
  x$bag <- as.numeric(gsub("[^0-9.-]", "", x$bag))

  # Extract remaining sample name
  x$sample <- stringr::str_trim(gsub("BAG\\d+|BAG\\s+\\d+", "", x$x, ignore.case = TRUE))

  # Extract duplicate/replicates
  x$replicate <- grepl("DUP|DP", x$sample, ignore.case = TRUE)
  x$sample <- gsub("DUP|DP", "", x$sample, ignore.case = TRUE)

  # Remove location names from sample
  # Samples containing 'JB' or 'MB' need to be noted
  # so the site column can be updated in read_lims function
  x$jb <- grepl("JB", x$sample)
  x$ms <- grepl("MS", x$sample)

  x$sample <- gsub("BN|JB|MS|TOFINO|BOUNDARY BAY|IONA|IONA NORTH|IONA SOUTH|ROBERTS BANK|ROBERT'S BANK|JENSENS BAY|JENSEN'S BAY|BRUNSWICK POINT|COWICHAN", "", x$sample)

  # Remove year
  x$sample <- gsub("2020|2021|2022", "", x$sample)

  # Replace '0P' typo with 'OP'
  x$sample <- gsub("0P", "OP", x$sample)

  # Trim up whitespace
  x$sample <- stringr::str_trim(x$sample)
  x$sample <- stringr::str_squish(x$sample)

  # Clean '#', trailing A/B or trailing -1/-2
  x$sample <- gsub("#|A$|B$|-1$|-2$", "", x$sample)

  # Clean leading/trailing dash
  x$sample <- gsub("^-|-$", "", x$sample)

  # Clean leading zeroes
  x$sample <- gsub("^0", "", x$sample)

  # Final trim up
  x$sample <- stringr::str_trim(x$sample)
  x$sample <- stringr::str_squish(x$sample)

  return(x)
}


#' Scan for any pre-existing lims data in database
#'
#' This function will scan the database `pesc_batch` and `pesc_benchtop` tables
#' and compare them to the provided batch and benchtop data in `lims_out` to
#' determine if that particular `lims_out` dataset has been imported into the
#' database already. (Scanning `pesc_data` would take awhile and is likely unnecessary.)
#'
#' @param db SQLite connection object pointing to the bpfa db
#' @param lims_out Output from `read_lims`.
#'
#' @return Logical (T/F) - does the data in `lims_out` already exist in the database?
#'
#' @noRd
scan_for_dupes <- function(db, lims_out) {
  # TODO: definitely need to make a bunch of tests for this one...
  # Check provided data is correct
  stopifnot("`lims_out` must be a list output produced by `read_lims()`." = inherits(lims_out, "list"))
  stopifnot("`lims_out` must be a list output of length three, produced by `read_lims().`" = (length(lims_out) == 3))
  stopifnot("`lims_out` must be a list output produced by `read_lims() with names 'batch', 'ng data', and 'bench sheet'.`." = all(names(lims_out) %in% c("batch", "ng data", "bench sheet")))

  db_batch <- DBI::dbGetQuery(db, "select * from pesc_batch;")
  db_bench <- DBI::dbGetQuery(db, "select * from pesc_benchtop")

  batch <- lims_out$batch
  bench <- lims_out$`bench sheet`

  m1 <- nrow(merge(batch, db_batch)) > 0 #== nrow(batch) # Dunno which is better honestly
  m2 <- nrow(merge(bench, db_bench)) > 0 #== nrow(bench)

  return(any(m1, m2))
}
