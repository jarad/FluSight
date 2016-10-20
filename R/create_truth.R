#' Creates observed truth to score entry against
#'
#' Determines observed true values for each target 
#'
#' @param fluview A logical value (default TRUE) indicating whether to download ILINet from 
#' Fluview
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. Must be NULL when downloading data using 
#' fluview == TRUE. Required if fluview == FALSE.
#' @import dplyr
#' @import cdcfluview
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' 
create_truth <- function(fluview = TRUE, weekILI = NULL) {

  warning("Work in progress. Values need to be updated for 2016/2017 season")

    # Return an error if fluview == FALSE and no data frame provided
  if (fluview == FALSE & is.null(weekILI)) {
    stop("ILINet data required if not fetching data from FluView")
  }
  
  if (fluview == TRUE & !is.null(weekILI)) {
    stop("Do not provide data if fetching data from ILINet")
  }
  
  # Verify user-submitted ILI data
  if (!is.null(weekILI)) {
    verify_ILI(weekILI) 
  }
  
  # Date first forecasts received
  start_week <- 42    #First week of ILINet data used for forecasts
  end_week <- 18      #Last week of ILINet data used for forecasts
  
  # Read in ILINet results
  if (fluview == TRUE) {
    # Read in ILINet data and rename locations to match template
    usflu <- get_flu_data("national", "ilinet", years = 2015) %>%
      select(
        location = REGION.TYPE,
        week = WEEK,
        wILI = X..WEIGHTED.ILI) %>%
      mutate(
        location = "US National",
        wILI = round(wILI, 1)) %>%
      filter(
        week >= start_wk | week <= end_wk + 4)
    
    
    regionflu <- get_flu_data("HHS", sub_region = 1:10,
                              "ilinet", years = 2015) %>%
      select(
        location = REGION,
        week = WEEK,
        wILI = X..WEIGHTED.ILI) %>%
      mutate(
        location = paste("HHS", location),
        wILI = round(wILI, 1)) %>%
      filter(
        week >= start_wk | week <= end_wk + 4)
    
    # Join national and HHs regional flu data
    weekILI <- rbind(usflu, regionflu)
  }

  # Add 52 to weeks in new year to keep weeks in order
  weekILI$week[weekILI$week < 40] <-
    as.integer(weekILI$week[weekILI$week < 40] + 52)
  
  # Create data shell to add targets to
  truth <- data.frame(target = character(),
                      location = character(),
                      forecast.wk = numeric(),
                      bin_start_incl = numeric()) 
 
  # Calculate targets if reached ----------------------------------
  for (this_location in levels(as.factor(weekILI$location))) {
    truth <- filter(weekILI, location == this_location) %>%
              create_seasonal(
                              this_location) %>%
              rbind(truth, .)
  }
  
  truth <- rbind(truth,
                 create_week(weekILI, start_week, end_week),
                 fill = TRUE)
  
  return(truth)
}



