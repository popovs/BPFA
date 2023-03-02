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

  # If there's any NA rows, grab data up to first empty row
  # otherwise just return the original df untouched
  x <- lapply(x, function(j) {
    if (any(rowSums(is.na(j)) == ncol(j))) {
      j[1:(as.numeric(rownames(j)[rowSums(is.na(j)) == ncol(j)][1])-1),]
    } else {
        j
      }
    }
    )

  # Save various LIMS IDs
  lims_ref <- names(x$batch)[!grepl("\\...", names(x$batch))][2]

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
  # Clean data
  x$batch$date_collected <- janitor::excel_numeric_to_date(as.numeric(x$batch$date_collected))
  x$batch$date_to_pesc <- janitor::excel_numeric_to_date(as.numeric(x$batch$date_to_pesc))
  x$batch$bag <- as.numeric(x$batch$bag)

  # Data tabs
  names(x$dry)[2] <- "lims_id"
  names(x$whole)[2] <- "lims_id"

  x$dry[,-c(1,2)] <- dplyr::mutate_all(x$dry[,-c(1,2)], as.numeric)
  x$whole[,-c(1,2)] <- dplyr::mutate_all(x$whole[,-c(1,2)], as.numeric)

  x$dry <- as.data.frame(tidyr::pivot_longer(x$dry, !c(Sample, lims_id), names_to = "assay", values_to = "ng_per_dry"))
  x$whole <- as.data.frame(tidyr::pivot_longer(x$whole, !c(Sample, lims_id), names_to = "assay", values_to = "ng_whole"))

  x$dry$dry_batch_id <- dry_lims_id
  x$whole$whole_batch_id <- whole_lims_id

  ng_dat <- merge(x$dry, x$whole, by = c("Sample", "lims_id", "assay"), all = TRUE)
  names(ng_dat)[1] <- "pesc_id"

  # Benchtop tab
  names(x$benchtop) <- c("bag", "pesc_id", "tube_g", "tube_sample_wet_g", "wet_g", "wet_extracted_g",
                         "dilution_solution_ml", "tube_sample_dry_g", "dry_g_per_ml", "dried_g",
                         "prct_dry_weight", "start_date", "analyst_initials", "notes")

  out <- list(x$batch, ng_dat, x$benchtop)
  names(out) <- c("samples", "ng data", "bench sheet")

  return(out)
}
