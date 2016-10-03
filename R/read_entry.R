#' Read in a csv entry file
#'
#' This function reads in the csv file and arranges it for consistency.
#'
#' @param file A csv file
#' @return An arranged data.frame
#' @import magrittr
#' @export
read_entry = function(file) {
  entry <- read.csv(file,
                    colClasses = "character",      # Due to bin_start_incl "none"
                    stringsAsFactors = FALSE)

  names(entry) <- tolower(names(entry))

  entry <- dplyr::mutate(entry, value = as.numeric(value))

  entry %>% arrange_entry
}

#' Arrange an entry for consistency
#'
#' @param entry A data.frame
#' @return An arranged data.frame
#' @import dplyr
#' @export
arrange_entry = function(entry) {

  verify_columns(entry)

  # Arrange entry by type, location, target, bin
  entry %>%
    dplyr::arrange(type, location, target, bin_start_incl) %>%
    dplyr::select_("location", "target", "type", "unit",
                   "bin_start_incl", "bin_end_notincl", "value")
}




#' Check columns of an entry
#'
#' Check to make sure the columns are location, target, type, unit,
#' bin_start_incl, bin_end_notincl, and value. If any of these are missing,
#' return an error indicating the column that is missing. If there are any
#' extra columns, return a warning indicating the extra columns.
#'
#' @param entry A data.frame
#' @return Invisibly returns TRUE if the column names check out
#'
verify_columns <- function(entry) {
  necessary_columns <- c("location", "target", "type", "unit",
                         "bin_start_incl", "bin_end_notincl", "value")

  cnames = names(entry)

  missing_columns <- base::setdiff(necessary_columns, cnames)
  if (length(missing_columns)>0) {
    stop("Entry needs these columns: ", paste(missing_columns))
  }

  extra_columns <- base::setdiff(cnames, necessary_columns)
  if (length(extra_columns)>0) {
    warning("Ignoring these extra columns:", paste(extra_columns))
  }

  return(invisible(TRUE))
}
