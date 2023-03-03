#' Read and clean up LIMS data sheets
#'
#' @param path Path to an Excel file containing LIMS data.
#'
#' @return A list containing two dataframes.
#' @export
#'
#' @examples
#' \dontrun {
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

  # Batch tab
  # Pull out data from batch header row onward and from "location" col to "date to PESC" col
  loc_col <- grep("site|location", x$batch, ignore.case = TRUE)
  pesc_col <- grep("PESC", x$batch, ignore.case = TRUE)
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
  x$batch$date_collected <- janitor::excel_numeric_to_date(as.numeric(x$batch$date_collected))
  x$batch$date_to_pesc <- janitor::excel_numeric_to_date(as.numeric(x$batch$date_to_pesc))
  x$batch$bag <- as.numeric(x$batch$bag)
  # Clean batch sample names
  x$batch$sample <- stringr::str_trim(gsub("#|-A|-1", "", x$batch$sample))
  x$batch[["site"]][grep("JB", x$batch$sample)] <- "JENSEN'S BAY"
  x$batch[["site"]][grep("MS", x$batch$sample)] <- "MALTBY SLOUGH"
  x$batch$sample <- stringr::str_trim(gsub("JB", "", x$batch$sample))
  x$batch$sample <- stringr::str_trim(gsub("MS", "", x$batch$sample))
  x$batch$sample <- stringr::str_trim(gsub("BN", "", x$batch$sample))
  # Final clean
  x$batch$sample <- stringr::str_trim(gsub("-", "", x$batch$sample))
  x$batch$sample <- stringr::str_trim(gsub("A$", "", x$batch$sample))
  x$batch$sample <- stringr::str_trim(gsub("2021|2022|2023", "", x$batch$sample))
  x$batch$sample <- sub("^0+", "", x$batch$sample) # Remove leading zeroes
  # Add LIMS ref column so we can easily reference the original file if needed
  x$batch$lims_ref <- lims_ref

  # Data tabs
  names(x$dry)[2] <- "lims_sample"
  names(x$whole)[2] <- "lims_sample"

  # Remove rows without sample info
  x$dry <- x$dry[!is.na(x$dry$Sample),]
  x$whole <- x$whole[!is.na(x$whole$Sample),]

  # Coerce numeric columns to numeric
  x$dry[,-c(1,2)] <- dplyr::mutate_all(x$dry[,-c(1,2)], as.numeric)
  x$whole[,-c(1,2)] <- dplyr::mutate_all(x$whole[,-c(1,2)], as.numeric)

  # Pivot longer
  x$dry <- as.data.frame(tidyr::pivot_longer(x$dry, !c(Sample, lims_sample), names_to = "assay", values_to = "ng_per_dry"))
  x$whole <- as.data.frame(tidyr::pivot_longer(x$whole, !c(Sample, lims_sample), names_to = "assay", values_to = "ng_whole"))

  x$dry$dry_batch_id <- dry_lims_id
  x$whole$whole_batch_id <- whole_lims_id

  # Merge dry and whole sheets
  ng_dat <- merge(x$dry, x$whole, by = c("Sample", "lims_sample", "assay"), all = TRUE)
  names(ng_dat)[1] <- "pesc_id"
  ng_dat$lims_ref <- lims_ref

  # Cleanup the sample name so we can match it to the batch tab
  # Extract bag
  ng_dat$bag <- stringr::str_extract(tolower(ng_dat$lims_sample), "bag\\d+|bag \\d+")
  ng_dat$bag <- as.numeric(gsub("[^0-9.-]", "", ng_dat$bag))
  # Extract cleaned sample name
  ng_dat$sample <- stringr::str_trim(gsub("bag\\d+|bag \\d+", "", ng_dat$lims_sample, ignore.case = TRUE))
  ng_dat$sample <- stringr::str_trim(gsub("#|-A|-1", "", ng_dat$sample))
  # Extract duplicate/replicates
  ng_dat$replicate <- ifelse(grepl("dup|dp", ng_dat$sample, ignore.case = TRUE), 2, 1)
  ng_dat$sample <- stringr::str_trim(gsub("dup|dp", "", ng_dat$sample, ignore.case = TRUE))
  ng_dat$pesc_id <- stringr::str_trim(gsub("dup|dp", "", ng_dat$pesc_id, ignore.case = TRUE))
  # Extract site
  ng_dat$site <- stringr::str_extract(toupper(ng_dat$sample), "TOFINO|BOUNDARY BAY|IONA|ROBERTS BANK|ROBERT'S BANK|JENSENS BAY|JENSEN'S BAY|BRUNSWICK POINT|COWICHAN")
  ng_dat$sample <- stringr::str_trim(gsub("TOFINO|BOUNDARY BAY|IONA|IONA NORTH|IONA SOUTH|ROBERTS BANK|ROBERT'S BANK|JENSENS BAY|JENSEN'S BAY|BRUNSWICK POINT|COWICHAN", "", ng_dat$sample))
  ng_dat[["site"]][grep("JB", ng_dat$sample)] <- "JENSEN'S BAY"
  ng_dat[["site"]][grep("MS", ng_dat$sample)] <- "MALTBY SLOUGH"
  ng_dat[["site"]][grep("BN", ng_dat$sample)] <- "BOUNDARY BAY"
  ng_dat$sample <- stringr::str_trim(gsub("JB", "", ng_dat$sample))
  ng_dat$sample <- stringr::str_trim(gsub("MS", "", ng_dat$sample))
  ng_dat$sample <- stringr::str_trim(gsub("BN", "", ng_dat$sample))
  # Final clean
  ng_dat$sample <- stringr::str_trim(gsub("-", "", ng_dat$sample))
  ng_dat$sample <- stringr::str_trim(gsub("A$", "", ng_dat$sample))
  ng_dat$sample <- stringr::str_trim(gsub("2021|2022|2023", "", ng_dat$sample))
  ng_dat$sample <- sub("^0+", "", ng_dat$sample) # Remove leading zeroes

  # Merge PESC sample ID into the samples tab
  # This won't be perfect but it should match most of them
  x$batch <- unique(merge(x$batch, ng_dat[,c("pesc_id", "bag", "sample", "lims_ref")], by = c("sample", "bag", "lims_ref"), all.x = TRUE))
  # Throw warning if any PESC IDs are duplicated
  if (any(plyr::count(x$batch[["pesc_id"]][!is.na(x$batch$pesc_id)])[["freq"]] > 1)) {
    warning("Duplicated PESC IDs: ", paste(plyr::count(x$batch$pesc_id)[plyr::count(x$batch$pesc_id)[2] > 1,"x"], collapse = ", "))
  }

  # Clean benchtop tab
  names(x$benchtop) <- c("bag", "pesc_id", "tube_g", "tube_sample_wet_g", "wet_g", "wet_extracted_g",
                         "dilution_solution_ml", "tube_sample_dry_g", "dry_g_per_ml", "dried_g",
                         "prct_dry_weight", "start_date", "analyst_initials", "notes")
  x$benchtop <- x$benchtop[!is.na(x$benchtop$pesc_id),]
  x$benchtop[,-c(2,12,13,14)] <- dplyr::mutate_all(x$benchtop[,-c(2,12,13,14)], as.numeric)

  out <- list(x$batch, ng_dat, x$benchtop)
  names(out) <- c("samples", "ng data", "bench sheet")

  return(out)
}
