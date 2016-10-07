#' Creates observed truth to score entry against
#'
#' Determines observed true values for each target 
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
create_truth <- function(fluview = TRUE, week_flu = NULL) {

  warning("Work in progress. Values need to be updated for 2016/2017 season")

    # Return an error if fluview == FALSE and no data frame provided
  if (fluview == FALSE & is.null(week_flu)) {
    stop("ILINet data required if not fetching data from FluView")
  }
  
  if (fluview == TRUE & !is.null(week_flu)) {
    stop("Do not provide data if fetching data from ILINet")
  }
  
  if (!is.null(week_flu)) {
    verify_ILI() #Need to create this program still
  }
  
  # Date first forecasts received
  start.date <- as.Date("2015-11-02")
  start.wk <- 42    #First week of ILINet data used for forecasts
  end.wk <- 18      #Last week of ILINet data used for forecasts
  
  # Read in ILINet results
  if (fluview == TRUE) {
    # Read in ILINet data and rename locations to match template
    usflu <- get_flu_data("national", "ilinet", years = 2015:2016) %>%
      select(
        location = REGION.TYPE,
        week = WEEK,
        wILI = X..WEIGHTED.ILI) %>%
      mutate(
        location = "US National",
        wILI = round(wILI,1)) %>%
      filter(
        week >= start.wk | week <= end.wk)
    
    regionflu <- get_flu_data("HHS", sub_region = 1:10,
                              "ilinet", years = 2015:2016) %>%
      select(
        location = REGION,
        week = WEEK,
        wILI = X..WEIGHTED.ILI) %>%
      mutate(
        location = paste("HHS", location),
        wILI = round(wILI,1)) %>%
      filter(
        week >= start.wk | week <= end.wk)
    
    # Join national and HHs regional flu data
    week_flu <- rbind(usflu, regionflu)
  }
  
  #Create baselines  
  baselines <- data.frame(location = unique(week_flu$location),
                          value = c(2.1, 1.3, 2.3, 1.8, 1.6, 1.9, 3.6, 1.7,
                                    1.4, 2.6, 1.1))
  
  # Add 52 to weeks in new year to keep weeks in order
  week_flu$week[week_flu$week < 40] <-
    as.integer(week_flu$week[week_flu$week < 40] + 52)
  
  # Create data shell to add targets to
  truth <- data.frame(target = character(),
                      location = character(),
                      forecast.wk = numeric(),
                      bin_start_incl = numeric()) 
  
  # Calculate targets if reached ----------------------------------
  for (this_location in levels(as.factor(week_flu$location))) {
    filter(week_flu, location == this_location) %>%
      seasonal_targets()
    
    filter(week_flu, location == this_location) %>%
      weekly_targets()
      # Need to write functions for creating onset, peak, week ahead forecasts
    
  }
  return(truth)
}
