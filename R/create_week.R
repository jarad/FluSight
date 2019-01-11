#' Creates observed truth for 1-4 week ahead prediction
#'
#' Determines observed true values for 1 to 4 week ahead weighted ILINet predictions
#'
#' @param weekILI A data.frame of weighted ILI values (default \code{NULL}). Must 
#' contain columns location, week, and wILI. 
#' @param start_wk The first MMWR week used for creating forecasts 
#' @param end_wk The last MMWR week used for creating forecasts
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for (default \code{"ilinet"})
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' @examples 
#' week_targets <- create_week(valid_ILI, 42, 18)
#' 
create_week <- function(weekILI, start_wk, end_wk, 
                        challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  # Rename submitted file to have same column names to work with following code
  if (challenge == "hospital") weekILI <- rename(weekILI, ILI = weeklyrate)

  # Save maximum MMWR week in season being analyzed
  maxMMWR <- max(weekILI$week)
  
  # Add 52/53 to weeks in new year to keep weeks in order
  weekILI$week[weekILI$week < 40] <-
    as.integer(weekILI$week[weekILI$week < 40] + maxMMWR)
  
  # Ensure ILI rounded to 1 decimal place
  weekILI$ILI <- round(weekILI$ILI, 1)
  
  end_wk <- end_wk + maxMMWR
  
  these_targets <- c("1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  
  week_target <- data.frame() 
  
  for (this_target in these_targets) {
    wk <- as.integer(substr(this_target, 1, 1))
    for (this_week in start_wk:end_wk) {
      #Set forecast week
      forecast_week <- this_week
      #Set forecast location
      this_point <- weekILI %>%
        filter(week == this_week + wk) %>%
        mutate(target = this_target,
          forecast_week = as.integer(forecast_week)) %>%
        select(target, location, forecast_week, bin_start_incl = ILI)
      
      week_target <- rbind(week_target, this_point)
    }
  }
  
  # If determine maximum bin value and reset if above that value
  if (challenge == "ilinet") {
    
    max_per <- FluSight::full_entry %>%
      dplyr::filter(target == "Season peak percentage", type == "Bin") %>%
      dplyr::group_by(location) %>%
      dplyr::mutate(bin_start_incl = as.numeric(bin_start_incl)) %>%
      dplyr::filter(bin_start_incl == max(bin_start_incl, na.rm = TRUE)) %>%
      dplyr::select(location, max_per = bin_start_incl)
    
  } else if (challenge == "state_ili") {
    
    max_per <- FluSight::full_entry_state %>%
      dplyr::filter(target == "Season peak percentage", type == "Bin") %>%
      dplyr::group_by(location) %>%
      dplyr::mutate(bin_start_incl = as.numeric(bin_start_incl)) %>%
      dplyr::filter(bin_start_incl == max(bin_start_incl, na.rm = TRUE))%>%
      dplyr::select(location, max_per = bin_start_incl)
    
  } else {
    
    max_per <- FluSight::full_entry_hosp %>%
      dplyr::filter(target == "Season peak percentage", type == "Bin") %>%
      dplyr::group_by(location) %>%
      dplyr::mutate(bin_start_incl = as.numeric(bin_start_incl)) %>%
      dplyr::filter(bin_start_incl == max(bin_start_incl, na.rm = TRUE)) %>%
      dplyr::select(location, max_per = bin_start_incl)
    
  }
  
  week_target <- dplyr::left_join(week_target, max_per, by = "location") %>%
    dplyr::mutate(bin_start_incl = ifelse(bin_start_incl > max_per,
                                          max_per, bin_start_incl)) %>%
    dplyr::select(-max_per)
  
  # Subtract 52/53 from all weeks in new year to realign with MMWR week
  week_target$forecast_week[week_target$forecast_week > maxMMWR] <-
    week_target$forecast_week[week_target$forecast_week > maxMMWR] - maxMMWR

  week_target$bin_start_incl <- format(round(week_target$bin_start_incl, 1),
                                       trim = T, nsmall = 1)
  
  return(week_target)
} 

