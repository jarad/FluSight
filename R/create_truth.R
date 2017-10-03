#' Creates observed truth to score entry against
#'
#' Determines observed true values for each target 
#'
#' @param fluview A logical value (default \code{TRUE}) indicating whether to
#'   download ILINet from Fluview. Only applicable if 
#'   \code{challenge = "ilinet"}.
#' @param weekILI A data.frame of observed values (default \code{NULL}). Must be 
#'   \code{NULL} when downloading data using \code{fluview = TRUE}. Required 
#'   if \code{fluview = FALSE}.
#'   
#'   For \code{challege = "ilinet"} or \code{challenge = "state_ili"}, must
#'   contain columns location, week, and wILI. For \code{challenge = "hospital"},
#'   must contain columns age_grp, week, and weeklyrate.
#' @param year Calendar year during which the flu season of interest begins. For
#'   the 2015/2016 flu season, \code{year = 2015}. Required whether downloading
#'   data from fluview or providing observed data
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for (default \code{"ilinet"})
#' @import dplyr
#' @import cdcfluview
#' @export
#' @return A data.frame with columns location, target, and bin_start_incl or
#'   columns age_grp, target, and bin_start_incl if \code{challenge = "hospital"}
#' @examples 
#' truth <- create_truth(year = 2015)
#' truth <- create_truth(fluview = TRUE, year = 2015, challenge = "ilinet")
#' truth <- create_truth(fluview = FALSE, weekILI = valid_ILI)
#' 
#' \dontrun{
#' truth <- create_truth(weekILI = valid_ILI)
#' truth <- create_truth(fluview = FALSE)
#' truth <- create_truth(fluview = TRUE, year = 2015, challenge = "hospital")
#' }
#' 
create_truth <- function(fluview = TRUE, year = NULL, weekILI = NULL,
                         challenge = "ilinet") {

  # Return informative errors if invalid parameter combinations provided
  if (is.null(year)) {
    stop("Year is required")
  }
  
  if (fluview == FALSE & is.null(weekILI)) {
    stop("ILINet data required if not fetching data from FluView")
  }
  
  if (fluview == TRUE & !is.null(weekILI)) {
    stop("Do not provide data if fetching data from ILINet")
  }
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  if (fluview == TRUE & challenge != "ilinet") {
    stop("Data can only be fetched from FluView for national ILINet forecasting")
  }
  
  # Verify user-submitted ILI data
  if (!is.null(weekILI) && challenge %in% c("ilinet", "state_ili")) {
    FluSight::verify_ILI(weekILI, challenge) 
  } else FluSight::verify_hosp(weekILI)
  
  # Date first forecasts - varies depending on forecast year and challenge
  if (challenge %in% c("ilinet", "state_ili")) {
    if (year <= 2014) {
      start_wk <- 41    #First week of ILINet data used for forecasts
      end_wk <- 19      #Last week of ILINet data used for forecasts
    }
    if (year == 2015) {
      start_wk <- 42    
      end_wk <- 18      
    }
    if (year == 2016) {
      start_wk <- 43
      end_wk <- 18
    }
    if (year == 2017) {
      start_wk <- 43
      end_wk <- 18
    }
  } else {
    if (year == 2016) {
      start_wk <- 48
      end_wk <- 17
    }
    if (year == 2017) {
      start_wk <- 48
      end_wk <- 17
    }
  }
  
  
  # Read in ILINet results
  if (fluview == TRUE) {
    
    # Check cdcfluview version number - output is different depending on version
    if (as.numeric(substr(packageVersion("cdcfluview"), 1, 3)) >= 0.5) {
      # Read in ILINet data and rename locations to match template
      usflu <- get_flu_data(region = "national", data_source = "ilinet",
                            years = year) %>%
        select(
          week = WEEK,
          wILI = `% WEIGHTED ILI`) %>%
        mutate(
          location = "US National",
          wILI = round(wILI, 1)) %>%
        filter(
          week >= start_wk | week <= end_wk + 4)
      
      
      regionflu <- get_flu_data(region = "HHS", sub_region = 1:10,
                                data_source = "ilinet", years = year) %>%
        select(
          location = REGION,
          week = WEEK,
          wILI = `% WEIGHTED ILI`) %>%
        mutate(
          location = paste("HHS", location),
          wILI = round(wILI, 1)) %>%
        filter(
          week >= start_wk | week <= end_wk + 4)
    } else {
          # Read in ILINet data and rename locations to match template
      usflu <- get_flu_data(region = "national", data_source = "ilinet",
                            years = year) %>%
        select(
          location = REGION.TYPE,
          week = WEEK,
          wILI = X..WEIGHTED.ILI) %>%
        mutate(
          location = "US National",
          wILI = round(wILI, 1)) %>%
        filter(
          week >= start_wk | week <= end_wk + 4)
      
      
      regionflu <- get_flu_data(region = "HHS", sub_region = 1:10,
                                data_source = "ilinet", years = year) %>%
        select(
          location = REGION,
          week = WEEK,
          wILI = X..WEIGHTED.ILI) %>%
        mutate(
          location = paste("HHS", location),
          wILI = round(wILI, 1)) %>%
        filter(
          week >= start_wk | week <= end_wk + 4)
    }
    # Join national and HHs regional flu data
    weekILI <- rbind(usflu, regionflu) 
  }

  # Create data shell to add targets to
  truth <- data.frame()
  
  if (challenge %in% c("ilinet", "state_ili")) {
    
    # Calculate targets if reached ----------------------------------
    for (this_location in levels(as.factor(weekILI$location))) {
      truth <- filter(weekILI, location == this_location) %>%
                FluSight::create_seasonal(this_location, year, challenge) %>%
                bind_rows(truth, .)
    }
    
  } else {
    
    # Calculate targets if reached ----------------------------------
    for (this_age in levels(as.factor(weekILI$age_grp))) {
      truth <- filter(weekILI, age_grp == this_age) %>%
        FluSight::create_seasonal(this_age, year, challenge) %>%
        bind_rows(truth, .)
    }
    
  }
  truth <- bind_rows(truth,
                 FluSight::create_week(weekILI, start_wk, end_wk, challenge)) #%>%

  
  return(truth)
}