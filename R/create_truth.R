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
create_targets <- function(fluview = TRUE, week_flu = NULL) {

  warning("Work in progress. Values need to be updated for 2016/2017 season")
  # 	week_flu: data frame of weekly ILINet values from this season to use for
  #							calculating targets. Must be provided if fluview == FALSE.
  #							Must contain the following columns:
  #								- location: 		character with 11 values; "US National" and
  #																"HHS Region 1" to "HHS Region 10"
  #								- week:					MMWR week
  # 							- observation:	weighted ILI value for given
  #																location/week/season combination

  
  # Return an error if fluview == FALSE and no data frame provided
  if (fluview == FALSE & is.null(week_flu)) {
    stop("ILINet data required if not fetching data from FluView")
  }
  
  # Return an error if fluview == TRUE and data frame provided
  if (fluview == TRUE & !is.null(week_flu)) {
    stop("Do not provide data if fetching data from ILINet")
  }
  
  # Return an error if provided data frame not in proper format
  if (!is.null(week_flu)) {
    verify_ILI() #Need to create this program still
  }
  
   
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
        wILI = round(wILI,1))
    
    regionflu <- get_flu_data("HHS", sub_region = 1:10,
                              "ilinet", years = 2015:2016) %>%
      select(
        location = REGION,
        week = WEEK,
        wILI = X..WEIGHTED.ILI) %>%
      mutate(
        location = paste("HHS", location),
        wILI = round(wILI,1))
    
    # Join national and HHs regional flu data
    week_flu <- rbind(usflu, regionflu)
  }
    
  

  
  return(targets)
}

