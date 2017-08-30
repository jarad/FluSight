#' Read in a csv entry file
#'
#' This function reads in the csv file and arranges it for consistency.
#'
#' @param file A csv file path
#' @param challenge one of "ilinet", "hospital", or "state_ili", indicating which 
#'   forecasting challenge the entry is for. Default is "ilinet"
#' @return An arranged data.frame
#' @import dplyr
#' @export
read_entry = function(file, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("Challenge must be one of ilinet, hospital, or state_ili")
  }
  
  entry <- read.csv(file,
                    colClasses = "character",      # Due to bin_start_incl "none"
                    stringsAsFactors = FALSE)

  names(entry) <- tolower(names(entry))
  
  
  entry <- entry %>%
            mutate(value = as.numeric(value),
                   bin_start_incl = trimws(replace(bin_start_incl,
                                            !is.na(bin_start_incl) & bin_start_incl != "none",
                                            format(round(as.numeric(
                                              bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                              , 1), nsmall = 1))),
                   bin_end_notincl = trimws(replace(bin_end_notincl,
                                            !is.na(bin_end_notincl) & bin_end_notincl != "none",
                                            format(round(as.numeric(
                                              bin_end_notincl[!is.na(bin_end_notincl) & bin_end_notincl != "none"])
                                              , 1), nsmall = 1))))

  # Add forecast week to imported data
  forecast_week <- as.numeric(gsub("EW", "", 
                                   regmatches(file, regexpr("(?:EW)[0-9]{2}", file))))
  
  
  if (length(forecast_week > 0))
     entry <- dplyr::mutate(entry, forecast_week  = forecast_week)


  arrange_entry(entry = entry, challenge = challenge)
}

#' Arrange an entry for consistency
#'
#' @param entry A data.frame
#' @param challenge one of "ilinet", "hospital", or "state_ili", indicating which
#'   forecasting challenge the entry is for
#' @return An arranged data.frame
#' @import dplyr
#' @export
#' @keywords internal
arrange_entry <- function(entry, challenge = "ilinet") {

  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("Challenge must be one of ilinet, hospital, or state_ili")
  }
  
  verify_colnames(entry, challenge)

  # Arrange entry by type, location, target, bin
  if (challenge %in% c("ilinet", "state_ili")) {
    entry %>%
      dplyr::arrange(type, location, target) %>% 
      dplyr::select(location, target, type, unit, bin_start_incl,
                    bin_end_notincl, value, everything())
  } else {
    entry %>%
      dplyr::arrange(type, age_grp, target) %>% 
      dplyr::select(age_grp, target, type, unit, bin_start_incl,
                    bin_end_notincl, value, everything())
  }
}

