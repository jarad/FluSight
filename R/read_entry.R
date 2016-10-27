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
  
  forecast_week <- as.numeric(gsub("EW", "", 
                              regmatches(file, regexpr("(?:EW)[0-9]{2}", file))))

  entry <- dplyr::mutate(entry, value = as.numeric(value),
                         forecast_week  = forecast_week)

  entry %>% arrange_entry
}

#' Arrange an entry for consistency
#'
#' @param entry A data.frame
#' @return An arranged data.frame
#' @import dplyr
#' @export
#' @keywords internal
arrange_entry = function(entry) {

  verify_colnames(entry)

  # Arrange entry by type, location, target, bin
  entry %>%
    dplyr::arrange(type, location, target) %>% 
    dplyr::select_("location", "target", "type", "unit",
                   "bin_start_incl", "bin_end_notincl", "value",
                   "forecast_week")
}

?regmatches
