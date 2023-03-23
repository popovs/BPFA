#' Read and clean up LIMS data
#'
#' @param path Path to an Excel file containing LIMS data.
#'
#' @return A list containing two dataframes.
#' @export
#'
#' @examples \dontrun{
#' read_lims("~/Documents/path/to/lims/file.xslx")
#' }
read_lims <- function(path) {
  stopifnot("File path provided does not end in .xlsx." = grepl(".xlsx$", path) == TRUE)
  sheets <- readxl::excel_sheets(path)
  stopifnot("File has more than 4 tabs. Do you need to merge benchtop sheets together?" = length(sheets) <= 4)
  x <- lapply(sheets, function(j) readxl::read_excel(path, sheet = j, .name_repair = "minimal"))
  x <- lapply(x, as.data.frame)
  names(x) <- c("dry", "whole", "benchtop", "batch")

  # Remove empty rows
  x <- lapply(x, janitor::remove_empty, "rows")

  # Save alphanumeric name as lims_ref
  lims_ref <- names(x$batch)[grep("(\\w\\d+\\w+)", names(x$batch))]

  dry_lims_id <- names(x$dry)[2]
  whole_lims_id <- names(x$whole)[2]

  # Quick data check - do the LIMS IDs in the other two tabs match the batch tab?
  # grab col number that has LIMS headers
  lims_col <- grep("LIMS", colnames(x$batch))
  # grab col number that has the actual IDs. NOTE: this will fail if the IDs ever start with anything other than 'V'
  lims_id_col <- grep("^V", names(x$batch))
  # grab row number that has lims DRY ID
  dry_row <- grep("dry", x$batch[[lims_col]])
  whole_row <- grep("whole", x$batch[[lims_col]])

  dry_lims_id2 <- x$batch[dry_row, lims_id_col]
  whole_lims_id2 <- x$batch[whole_row, lims_id_col]

  # Data check
  stopifnot("LIMS batch ID for dry samples in the 'ng per dry' tab does not match the ID in the 'Batch' tab." = dry_lims_id == dry_lims_id2)
  stopifnot("LIMS batch ID for whole samples in the 'ng whole sample' tab does not match the ID in the 'Batch' tab." = whole_lims_id == whole_lims_id2)

  # Resume processing

  # BATCH TAB ###
  # Pull out data from batch header row onward and from "location" col to "date to PESC" col
  loc_col <- grep("site|location", x$batch, ignore.case = TRUE)
  pesc_col <- grep("PESC", x$batch, ignore.case = TRUE) # TODO: this will fail in any spreadsheets that are missing a "date to PESC" col
  x$batch <- x$batch[-(1:grep("site|location", x$batch[[loc_col]], ignore.case = TRUE) - 1), loc_col:pesc_col]
  names(x$batch) <- x$batch[1,] # rename header
  x$batch <- x$batch[-1,] # remove first row
  # Pull out and clean up batch names
  batch_names <- janitor::make_clean_names(names(x$batch))
  batch_names <- gsub("point", "sample", batch_names)
  batch_names <- gsub("collection", "date_collected", batch_names)
  batch_names <- gsub("location", "site", batch_names)
  names(x$batch) <- batch_names

  # Clean batch data
  # Clean location names
  x$batch$site <- clean_locs(x$batch$site)
  # Clean batch dates... sqlite plays nicer with string dates, so they are converted to chr
  x$batch$date_collected <- janitor::excel_numeric_to_date(as.numeric(x$batch$date_collected))
  x$batch$date_collected <- as.character(x$batch$date_collected)

  x$batch$date_to_pesc <- janitor::excel_numeric_to_date(as.numeric(x$batch$date_to_pesc))
  x$batch$date_to_pesc <- as.character(x$batch$date_to_pesc)
  # Clean batch numerics
  x$batch$bag <- as.numeric(x$batch$bag)
  # Clean batch sample names
  x$batch$sample <- clean_samples(x$batch$sample)[["sample"]]
  # Add LIMS ref column so we can easily reference the original file if needed
  x$batch$lims_ref <- lims_ref

  # DATA TABS ###
  names(x$dry)[2] <- "lims_sample"
  names(x$whole)[2] <- "lims_sample"

  # Remove rows without sample info
  x$dry <- x$dry[!is.na(x$dry[[1]]),]
  x$whole <- x$whole[!is.na(x$whole[[1]]),]

  # Coerce numeric columns to numeric
  x$dry[,-c(1,2)] <- dplyr::mutate_all(x$dry[,-c(1,2)], as.numeric)
  x$whole[,-c(1,2)] <- dplyr::mutate_all(x$whole[,-c(1,2)], as.numeric)

  # Pivot longer
  x$dry <- as.data.frame(tidyr::pivot_longer(x$dry, !c(Sample, lims_sample), names_to = "assay", values_to = "ng_per_dry"))
  x$whole <- as.data.frame(tidyr::pivot_longer(x$whole, !c(Sample, lims_sample), names_to = "assay", values_to = "ng_whole"))

  x$dry$dry_batch_id <- dry_lims_id
  x$whole$whole_batch_id <- whole_lims_id

  # Merge dry and whole sheets
  # First get diagnostics to throw possible warnings
  n_dry <- nrow(x$dry)
  n_whole <- nrow(x$whole)
  if (n_dry != n_whole) warning("The number of records between the 'ng per g dry' and 'ng whole sample' tabs do not match.")
  # Now merge
  ng_dat <- merge(x$dry, x$whole, by = c("Sample", "lims_sample", "assay"), all = TRUE)
  names(ng_dat)[1] <- "pesc_id"
  ng_dat$lims_ref <- lims_ref
  # Another diagnostic test - warn if the merge produced uneven no of records,
  # which would suggest a sample name typo in one of the sheets
  if (nrow(ng_dat) != n_dry | nrow(ng_dat) != n_whole) warning("Merging the 'ng per g dry' and 'ng whole sample' tabs resulted in extra records. Perhaps there is typo in the sample name in one of the tabs?")
  # Cleanup pesc_id
  ng_dat$pesc_id <- gsub("dup|dp", "", ng_dat$pesc_id, ignore.case = TRUE)
  ng_dat$pesc_id <- stringr::str_trim(ng_dat$pesc_id)
  # Cleanup the sample name so we can later match it to the batch tab
  ng_dat <- cbind(ng_dat, clean_samples(ng_dat$lims_sample)[-1])
  # Misc cleanup
  ng_dat$replicate <- ifelse(ng_dat$replicate, 2, 1)

  # Merge PESC sample ID into the batch tab by bag + sample
  # This won't be perfect but it should match most of them
  ng_samples <- unique(ng_dat[,c("pesc_id", "bag", "sample", "lims_ref", "jb", "ms")])
  # Throw warning if # of samples detected in ng_dat don't match # of samples in batch tab
  if (nrow(ng_samples) != nrow(x$batch)) warning("The number of unique samples detected in the 'ng per g dry' and 'ng whole sample' tabs (n = ", nrow(ng_samples), ") does not match the number of samples in the 'batch' tab (n = ", nrow(x$batch), ").")
  # Now merge
  x$batch <- unique(merge(x$batch, ng_samples, by = c("sample", "bag", "lims_ref"), all = TRUE))
  # Throw warning if any PESC IDs are duplicated
  if (any(plyr::count(x$batch[["pesc_id"]][!is.na(x$batch$pesc_id)])[["freq"]] > 1)) {
    warning("Duplicated PESC IDs: ", paste(plyr::count(x$batch$pesc_id)[plyr::count(x$batch$pesc_id)[2] > 1,"x"], collapse = ", "))
  }

  # Update batch sites
  x$batch$site <- ifelse(x$batch$jb, "Jensen's Bay", x$batch$site)
  x$batch$site <- ifelse(x$batch$ms, "Maltby Slough", x$batch$site)
  x$batch <- dplyr::select(x$batch, -jb, -ms)

  # Clean benchtop tab
  names(x$benchtop) <- c("bag", "pesc_id", "tube_g", "tube_sample_wet_g", "wet_g", "wet_extracted_g",
                         "dilution_solution_ml", "tube_sample_dry_g", "dry_g_per_ml", "dried_g",
                         "prct_dry_weight", "start_date", "analyst_initials", "notes")
  x$benchtop <- x$benchtop[!is.na(x$benchtop$pesc_id),]
  x$benchtop[,-c(2,12,13,14)] <- dplyr::mutate_all(x$benchtop[,-c(2,12,13,14)], as.numeric)
  # Extract tube number, if present
  x$benchtop$tube_no <- stringr::str_extract(tolower(x$benchtop$pesc_id), "tube\\d+|tube\\s+\\d+")
  x$benchtop$tube_no <- as.numeric(stringr::str_extract(x$benchtop$tube_no, '\\d'))
  x$benchtop$tube_no <- ifelse(is.na(x$benchtop$tube_no), 1, x$benchtop$tube_no)
  # Cleanup pesc_id
  x$benchtop$pesc_id <- gsub("dup|dp", "", x$benchtop$pesc_id, ignore.case = TRUE)
  x$benchtop$pesc_id <- gsub("tube|tube\\d+|tube\\s+\\d+", "", x$benchtop$pesc_id, ignore.case = TRUE)
  x$benchtop$pesc_id <- stringr::str_trim(x$benchtop$pesc_id)
  # Clean start_date... sqlite plays nicer with string dates, so they are converted to chr
  x$benchtop$start_date <- as.character(x$benchtop$start_date)

  # Miscellaneous data checks
  # Check that the bag #s in benchtop matches bench bag #s in batch
  # TODO: make this better... it's annoying to get this error if
  # the combos match but the length is different, i.e. batch has info
  # on samples that simply weren't run
  #if (nrow(dplyr::setdiff(unique(x$batch[,c("pesc_id", "bag")]),
  #               unique(x$benchtop[,c("pesc_id", "bag")]))) > 0) warning("PESC sample ID - bag number combinations do not match between the 'batch' and 'benchtop' tabs.")

  # Normalize tables before returning output
  # batch
  x$batch <- dplyr::select(x$batch, pesc_id, site, sample, bag, lims_ref, date_collected, date_to_pesc)
  x$batch <- dplyr::arrange(x$batch, pesc_id)
  # bench
  #x$benchtop <- dplyr::select(x$benchtop, -bag)
  x$benchtop <- dplyr::select(x$benchtop, pesc_id, bag, tube_no, tube_g:notes)
  # ng dat
  ng_dat <- dplyr::select(ng_dat, pesc_id, replicate, lims_sample:lims_ref)

  # Finally, return the cleaned data
  out <- list(x$batch, ng_dat, x$benchtop)
  names(out) <- c("batch", "ng data", "bench sheet")

  return(out)
}


#' Merge full sample metadata with PESC data
#'
#' @param lims_out List output from read_lims()
#' @param samples Fatty acid sample metadata following the BPFA data entry template
#'
#' @return A list of containing PESC data matched to full sample metadata, PESC results, and PESC bench sheets containing test tube metadata.
#' @export
#'
#' @examples \dontrun {
#' out <- read_lims("~/Documents/path/to/lims/file.xslx")
#' samples <- read.csv("~/Documents/path/to/sample/metadata.csv")
#' merged_results <- merge_samples(lims_out = out, samples = samples)
#' }
merge_samples <- function(lims_out, samples) {
  stopifnot("`lims_out` must be a list output produced by `read_lims()`." = inherits(lims_out, "list"))
  stopifnot("`lims_out` must be a list output of length three, produced by `read_lims().`" = (length(lims_out) == 3))
  stopifnot("`lims_out` must be a list output produced by `read_lims() with names 'batch', 'ng data', and 'bench sheet'.`." = all(names(lims_out) %in% c("batch", "ng data", "bench sheet")))

  batch <- lims_out$batch
  ng_dat <- lims_out$`ng data`
  benchtop <- lims_out$`bench sheet`

  # Clean up samples
  samples$date_collected <- as.Date(samples$date_collected) # TODO: make this more robust
  samples$site <- toupper(samples$site)
  samples$sample <- stringr::str_trim(gsub("-", "", samples$sample))
  samples$sample <- stringr::str_trim(gsub("A$", "", samples$sample))
  samples$sample <- stringr::str_trim(gsub("2021|2022|2023", "", samples$sample))
  samples$sample <- sub("^0+", "", samples$sample) # Remove leading zeroes

  # Merge batch and samples
  s <- merge(batch, samples, by = c("sample", "date_collected"), all.x = TRUE)
  s <- dplyr::select(s, -site.x)
  names(s) <- gsub("site.y", "site", names(s))
  s <- dplyr::arrange(s, lims_ref, pesc_id)
  s <- dplyr::select(s, pesc_id, lims_ref, bag, sample, site, date_collected, time_collected, date_to_pesc, dplyr::everything())

  # Rearrange some columns and spit results out
  benchtop <- dplyr::select(benchtop, pesc_id, dplyr::everything())

  out <- list(s, benchtop, ng_dat)
  names(out) <- c("samples", "bench sheet", "ng data")
  return(out)
}
