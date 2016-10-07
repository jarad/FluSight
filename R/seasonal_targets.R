#' Creates observed truth for seasonal targets
#'
#' Determines observed true values for onset week, seasonal peak percentage, 
#' and seasonal peak week
#'
#' @param fluview A logical value (default TRUE) indicating whether to download ILINet from 
#' Fluview
#' @param week_flu A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. Must be NULL when downloading data using 
#' fluview == TRUE. Required if fluview == FALSE.
#' @import dplyr
#' @import cdcfluview
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' 
seasonal_targets <- function(week_flu) {
  
}
  
  
  
  
  
  
  
 
  
# Create baselines  
baselines <- data.frame(location = unique(week_flu$location),
                          value = c(2.1, 1.3, 2.3, 1.8, 1.6, 1.9, 3.6, 1.7,
                                    1.4, 2.6, 1.1))