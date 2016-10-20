#' Creates observed truth for seasonal targets
#'
#' Determines observed true values for onset week, seasonal peak, and seasonal
#' peak percentage
#'
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. 
#' @param region A character string specifying the target location - either US National 
#' or one of HHS Region 1-10
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#'   
create_seasonal <- function(weekILI, region) {
  season_truth <- rbind(create_onset(weekILI, region),
                        create_peak(weekILI, region))
  return(season_truth)
}  


#' Creates observed truth for seasonal onset
#'
#' Determines observed true values for onset week
#'
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. 
#' @param region A character string specifying the target location - either US National 
#' or one of HHS Region 1-10
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#'   
create_onset <- function(weekILI, region) {
  
  # Create baselines  
  warning("Baselines need to be updated for 2016/2017 season")
  baselines <- data.frame(region = c("US National", "HHS Region 1", "HHS Region 2", 
                                     "HHS Region 3", "HHS Region 4", "HHS Region 5",
                                     "HHS Region 6", "HHS Region 7", "HHS Region 8",
                                     "HHS Region 9", "HHS Region 10"),
                          value = c(2.1, 1.3, 2.3, 1.8, 1.6, 1.9, 3.6, 1.7,
                                    1.4, 2.6, 1.1))

  # Check to see if 3 weeks above baseline have passed
  j <- 0  # Counter for weeks above peak
  for (i in head(weekILI$week, n = 1):tail(weekILI$week, n = 1)) {
    if (weekILI$wILI[weekILI$week == i] >=
        baselines$value[baselines$region == region]) {
      j <- j + 1
    } else {
      j <- 0
    }
    if (j == 3) {
      onset <- i - 2
      break
    }
    if (i == tail(weekILI$week, n = 1)) {
      onset <- "none"
    }
  }
    
  # If onset week > 52, reset to MMWR week
  if (onset > 52) {
    onset <- onset - 52
  }
    
  onset_truth <- data.frame(target = "Season onset",
                            location = region,
                            forecast_wk = NA,
                            bin_start_incl = onset,
                            stringsAsFactors = FALSE)
  
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
create_peak <- function(weekILI, region) {
  
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

  peak_truth <- data.frame(target = c("Season peak week", 
                                      "Season peak percentage"),
                          location = region,
                          forecast_wk = NA,
                          bin_start_incl = c(pkwk[1], pkper))
  if (length(pkwk) > 1) {  
    for (i in 2:length(pkwk)) {
      extra_obs <- data.frame(target = "Season peak week",
                              location = region,
                              forecast_wk = NA,
                              bin_start_incl = pkwk[i],
                              stringsAsFactors = FALSE)
      peak_truth <- rbind(peak_truth, extra_obs)
    }
  }
  
  return(peak_truth)
}  


