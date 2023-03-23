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
  x <- gsub(" Chl-A", "", x)
  x <- gsub(" Foreshore", "", x)
  x <- gsub("Banks", "Bank", x)
  x <- gsub("Roberts Bank", "Brunswick Point", x)
  x <- gsub("^Cowichan$", "Cowichan Bay", x)

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
#' @return A dataframe consisting of the original sample name, sample bag number (if detected), waypoint, replicate status (if detected), and two logical columns indicating if the sample was collected in Jensen's Bay (`jb == TRUE`) or Maltby Slough (`mb == TRUE`).
#' @export
clean_samples <- function(x) {
  stopifnot("x must be a character vector." = inherits(x, "character"))

  x <- toupper(x)
  x <- as.data.frame(x)

  # Extract bag
  x$bag <- stringr::str_extract(x$x, "BAG\\d+|BAG \\d+")
  x$bag <- as.numeric(gsub("[^0-9.-]", "", x$bag))

  # Extract remaining sample name
  x$waypoint <- stringr::str_trim(gsub("BAG\\d+|BAG \\d+", "", x$x, ignore.case = TRUE))

  # Extract duplicate/replicates
  x$replicate <- grepl("DUP|DP", x$waypoint, ignore.case = TRUE)
  x$waypoint <- gsub("DUP|DP", "", x$waypoint, ignore.case = TRUE)

  # Clean '#', trailing A/B or trailing -1/-2
  x$waypoint <- gsub("#|A$|B$|-1$|-2$", "", x$waypoint)

  # Clean leading BN, JB, MS
  # Samples that start with JB or MB need to be noted
  # so the site column can be updated in read_lims function
  x$jb <- grepl("JB", x$waypoint)
  x$ms <- grepl("MS", x$waypoint)

  x$waypoint <- gsub("BN", "", x$waypoint)
  x$waypoint <- gsub("JB", "", x$waypoint)
  x$waypoint <- gsub("MS", "", x$waypoint)

  # Remove year
  x$waypoint <- gsub("2020|2021|2022", "", x$waypoint)

  # Replace '0P' typo with 'OP'
  x$waypoint <- gsub("0P", "OP", x$waypoint)

  # Trim up whitespace
  x$waypoint <- stringr::str_trim(x$waypoint)
  x$waypoint <- stringr::str_squish(x$waypoint)

  # Clean leading/trailing dash
  x$waypoint <- gsub("^-|-$", "", x$waypoint)

  # Clean leading zeroes
  x$waypoint <- gsub("^0", "", x$waypoint)

  # Final trim up
  x$waypoint <- stringr::str_trim(x$waypoint)
  x$waypoint <- stringr::str_squish(x$waypoint)

  return(x)
}
