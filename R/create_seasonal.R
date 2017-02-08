#' Creates observed truth for seasonal targets
#'
#' Determines observed true values for onset week, seasonal peak, and seasonal
#' peak percentage
#'
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. 
#' @param region A character string specifying the target location - either US National 
#' or one of HHS Region 1-10
#' @param year Calendar year during which the flu season of interest begins. 
#' For the 2015/2016 flu season, \code{year = 2015}
#' @import dplyr
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' @examples 
#' season_targets <- create_seasonal(valid_ILI, "US National")
#' season_targets <- create_seasonal(valid_ILI, "HHS Region 4")
#'   
create_seasonal <- function(weekILI, region, year) {
  season_truth <- rbind(create_onset(weekILI, region, year),
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
#' @param year Calendar year during which the flu season of interest begins. 
#' For the 2015/2016 flu season, \code{year = 2015}
#' @import dplyr
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' @keywords internal
#'   
create_onset <- function(weekILI, region, year) {
  
  # Add 52 to weeks in new year to keep weeks in order
  weekILI$week[weekILI$week < 40] <-
    as.integer(weekILI$week[weekILI$week < 40] + 52)
  
  # Create baselines  
  if (year == 2015){
    baselines <- data.frame(region = c("US National", "HHS Region 1", "HHS Region 2", 
                                       "HHS Region 3", "HHS Region 4", "HHS Region 5",
                                       "HHS Region 6", "HHS Region 7", "HHS Region 8",
                                       "HHS Region 9", "HHS Region 10"),
                            value = c(2.1, 1.3, 2.3, 1.8, 1.6, 1.9, 3.6, 1.7,
                                      1.4, 2.6, 1.1))
  }
  
  if (year == 2016){
    baselines <- data.frame(region = c("US National", "HHS Region 1", "HHS Region 2",
                                       "HHS Region 3", "HHS Region 4", "HHS Region 5",
                                       "HHS Region 6", "HHS Region 7", "HHS Region 8",
                                       "HHS Region 9", "HHS Region 10"),
                            value = c(2.2, 1.4, 3.0, 2.2, 1.7, 1.9, 4.1, 1.8,
                                      1.4, 2.5, 1.1))
  }
  
  # Check to see if 3 weeks above baseline have passed
  j <- 0  # Counter for weeks above peak
  for (i in head(weekILI$week, n = 1):tail(weekILI$week, n = 1)) {
    if (weekILI$wILI[weekILI$week == i & weekILI$location == region] >=
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
  if (is.numeric(onset) && onset > 52) {
    onset <- onset - 52
  }
    
  onset_truth <- data.frame(target = "Season onset",
                            location = region,
                            forecast_week = as.integer(NA),
                            bin_start_incl = as.character(onset),
                            stringsAsFactors = FALSE) %>%
    mutate(bin_start_incl = trimws(replace(bin_start_incl,
                                           !is.na(bin_start_incl) & bin_start_incl != "none",
                                           format(round(as.numeric(
                                             bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                             , 1), nsmall = 1, trim = T))))
  
  
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
#' @import dplyr
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' @keywords internal
#' 
create_peak <- function(weekILI, region) {
 
  # Add 52 to weeks in new year to keep weeks in order
  weekILI$week[weekILI$week < 40] <-
    as.integer(weekILI$week[weekILI$week < 40] + 52)
  
  pkwk  <- weekILI$week[weekILI$location == region &
                          weekILI$wILI == max(weekILI$wILI[weekILI$location == 
                                                           region])]
  pkper <- max(weekILI$wILI[weekILI$location == region])
  
  # Only create peak if at least three weeks of decline following last peak
  if (tail(weekILI$week, n = 1) - tail(pkwk, n = 1) < 3) {
    pkwk <- NA
    pkper <- NA
  }
  
  # Only create peak if after MMWR week 4 in new year (56 in ordered coding)
  if (tail(weekILI$week, n = 1) < 56) {
    pkwk  <- NA
    pkper <- NA
  }
  
  # If peak week > 52, reset to MMWR week
  for (i in 1:length(pkwk)) {
    if (!(is.na(pkwk[i])) && pkwk[i] > 52) {
      pkwk[i] <- pkwk[i] - 52
    }
  }

  peak_truth <- data.frame(target = c("Season peak week", 
                                      "Season peak percentage"),
                          location = region,
                          forecast_week = as.integer(NA),
                          bin_start_incl = c(pkwk[1], pkper),
                          stringsAsFactors = FALSE)
  if (length(pkwk) > 1) {  
    for (i in 2:length(pkwk)) {
      extra_obs <- data.frame(target = "Season peak week",
                              location = region,
                              forecast_week = as.integer(NA),
                              bin_start_incl = pkwk[i],
                              stringsAsFactors = FALSE)
      peak_truth <- rbind(peak_truth, extra_obs)
    }
  }
  
  peak_truth$bin_start_incl <- ifelse(is.na(peak_truth$bin_start_incl),
                                      peak_truth$bin_start_incl,
                                      format(round(peak_truth$bin_start_incl, 1), trim = T, nsmall = 1))
  
  

  return(peak_truth)
}  
