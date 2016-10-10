#' Creates observed truth for seasonal and weekly targets
#'
#' Determines observed true values for onset week, seasonal peak percentage, 
#' seasonal peak week, and 1 to 4 week ahead influenza forecasting targets
#'
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI.
#' @param location A character string specifying the target location - either US National 
#' or one of HHS Region 1-10
#' @import dplyr
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' 
create_targets <- function(weekILI, location) {
  truth <- data.frame(target = character(),
                      location = character(),
                      forecast.wk = numeric(),
                      bin_start_incl = numeric())
  
  truth <- rbind(truth, 
                 create_onset(weekILI, location),
                 create_pkwk(weekILI, location),
                 create_pkper(weekILI, location),
                 create_wk(weekILI, location))
  
  return(truth)
}
  
#' Creates observed truth for seasonal onset
#'
#' Determines observed true values for onset week
#'
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. 
#' @param location A character string specifying the target location - either US National 
#' or one of HHS Region 1-10
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#'   
create_onset <- function(weekILI, location) {
  
  # Create baselines  
  warning("Baselines need to be updated for 2016/2017 season")
  baselines <- data.frame(location = c("US National", "HHS Region 1", "HHS Region 2", 
                                       "HHS Region 3", "HHS Region 4", "HHS Region 5",
                                       "HHS Region 6", "HHS Region 7", "HHS Region 8",
                                       "HHS Region 9", "HHS Region 10"),
                          value = c(2.1, 1.3, 2.3, 1.8, 1.6, 1.9, 3.6, 1.7,
                                    1.4, 2.6, 1.1))
  
  # Check to see if 3 weeks above baseline have passed
  j <- 0  # Counter for weeks above peak
  for (i in head(weekILI$week, n = 1):tail(weekILI$week, n = 1)) {
    if (weekILI$observation[weekILI$week == i] >=
        baselines$value[baselines$location == location]) {
      j <- j + 1
    } else {
      j <- 0
    }
    if (j == 3) {
      onset <- i - 2
      break
    }
    if (i == tail(weekILI$week, n = 1)) {
      onset <- NA
    }
  }
  
  # If onset week > 52, reset to MMWR week
  if (onset > 52) {
    onset <- onset - 52
  }
  
  onset_truth <- data.frame(target = "Season onset",
                            location = location,
                            forecast.wk = NA,
                            bin_start_incl = onset)
  return(onset_truth)
}  

#' Creates observed truth for seasonal peak values
#'
#' Determines observed true values for peak week and peak weighted ILINet
#' percentage for an influenza season
#'
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. 
#' @param location A character string specifying the target location - either US National 
#' or one of HHS Region 1-10
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' 
create_peak <- function(weekILI, location) {
  
  pkwk  <- weekILI$week[weekILI$wILI == max(weekILI$wILI)]
  pkper <- max(weekILI$wILI)
  
  # Only create peak if after MMWR week 4 in new year (56 in ordered coding)
  if (tail(weekILI$week, n = 1) < 56) {
    pkwk  <- NA
    pkper <- NA
  }
  
  # If peak week > 52, reset to MMWR week
  for (i in 1:length(pkwk)) {
    if (pkwk[i] > 52) {
      pkwk[i] <- pkwk[i] - 52
    }
  }

  peak.truth <- data.frame(target = c("Season peak week", 
                                      "Season peak percentage"),
                          location = location,
                          forecast.wk = NA,
                          bin_start_incl = c(pkwk[1], pkper))
  for (i in 2:length(pkwk)) {
    extra.obs <- data.frame(target = "Season peak week",
                            location = location,
                            forecast.wk = NA,
                            bin_start_incl = pkwk[i])
    peak.truth <- rbind(peak.truth, extra.obs)
  }
  
  return(peak_truth)
}  
  
  
  
  
 
  
