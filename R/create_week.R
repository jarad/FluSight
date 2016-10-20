#' Creates observed truth for 1-4 week ahead prediction
#'
#' Determines observed true values for 1 to 4 week ahead weighted ILINet predictions
#'
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. 
#' @param start_week The first MMWR week used for creating forecasts 
#' @param end_week The last MMWR week used for creating forecasts
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' 
create_week <- function(weekILI, start_week, end_week) {
  
  end_week <- end_week + 52
  
  these_targets <- c("1 wk ahead", "2 wk ahead", "3 wk ahead", "4 wk ahead")
  
  week_target <- data.frame(target = character(),
                            location = character(),
                            forecast_wk = numeric(),
                            bin_start_incl = numeric()) 
  
  for (this_target in these_targets) {
    wk <- as.numeric(substr(this_target, 1, 1))
    for (this_week in start_week:end_week) {
      #Set forecast week
      forecast_wk <- this_week
      #Set forecast location
      this_point <- filter(weekILI, location == location &
                             week == this_week + wk) %>%
        mutate(
          target = this_target,
          forecast_wk = forecast_wk) %>%
        select(
          -week,
          bin_start_incl = wILI)
      
      week_target <- rbind(week_target, this_point)
    }
  }
  
  # Subtract 52 from all weeks in new year to realign with MMWR week
  week_target$forecast_wk[week_target$forecast_wk > 52] <-
    week_target$forecast_wk[week_target$forecast_wk > 52] - 52
  
  return(week_target)
} 

