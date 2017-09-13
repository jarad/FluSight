#' Creates observed truth for seasonal targets
#'
#' Determines observed true values for onset week, seasonal peak, and seasonal
#' peak percentage
#'
#' @param weekILI A data.frame of weighted ILI values (default NULL). Must contain columns
#' location, week, and wILI. 
#' @param location A character string specifying the target location or age group
#' @param year Calendar year during which the flu season of interest begins. 
#' For the 2015/2016 flu season, \code{year = 2015}
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for (default \code{"ilinet"})
#' @import dplyr
#' @return A data.frame with columns location, target, and bin_start_incl
#' @export
#' @examples 
#' season_targets <- create_seasonal(valid_ILI, "US National")
#' season_targets <- create_seasonal(valid_ILI, "HHS Region 4")
#'   
create_seasonal <- function(weekILI, location, year, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  # Round weekILI or rate values to one decimal place for calculating targets
  if (challenge %in% c("ilinet", "state_ili")) {
    weekILI$ILI <- round(weekILI$ILI, 1)
  } else weekILI$weeklyrate <- round(weekILI$weeklyrate, 1)
  
  # Create truth for seasonal targets
  if (challenge == "ilinet") {
    season_truth <- bind_rows(create_onset(weekILI, location, year),
                              create_peak(weekILI, location, challenge))
  } else season_truth <- create_peak(weekILI, location, challenge)
  
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
  
  # Save maximum MMWR week in season being analyzed
  maxMMWR <- max(weekILI$week)
  
  # Add 52/53 to weeks in new year to keep weeks in order
  weekILI$week[weekILI$week < 40] <-
    as.integer(weekILI$week[weekILI$week < 40] + maxMMWR)
  
  # Check to see if 3 weeks above baseline have passed
  j <- 0  # Counter for weeks above peak
  for (i in head(weekILI$week, n = 1):tail(weekILI$week, n = 1)) {
    if (weekILI$ILI[weekILI$week == i & weekILI$location == region] >=
        past_baselines$value[past_baselines$location == region & 
                             past_baselines$year == year]) {
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
  if (is.numeric(onset) && onset > maxMMWR) {
    onset <- onset - maxMMWR
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
#' @param weekILI A data.frame of weighted ILI values (default NULL). 
#' Must contain columns location (or age_grp), week, and wILI. 
#' @param location A character string specifying the target location or age group
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for (default \code{"ilinet"})
#' @import dplyr
#' @return A data.frame with columns location (or age_grp), target, and bin_start_incl
#' @export
#' @keywords internal
#' 
create_peak <- function(weekILI, location, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  # Rename submitted file to have same column names to work with following code
  if (challenge == "hospital") weekILI <- rename(weekILI, location = age_grp,
                                                 ILI = weeklyrate)
  
  # Save maximum MMWR week in season being analyzed
  maxMMWR <- max(weekILI$week)
  
  # Add 52/53 to weeks in new year to keep weeks in order
  weekILI$week[weekILI$week < 40] <-
    as.integer(weekILI$week[weekILI$week < 40] + maxMMWR)
  
  pkwk  <- weekILI$week[weekILI$location == location &
                          weekILI$ILI == max(weekILI$ILI[weekILI$location == 
                                                             location])]
  pkper <- max(weekILI$ILI[weekILI$location == location])
  
  # If peak percentage > max bin, set to max bin size
  if (challenge == "ilinet") {
    max_pkper <- max(as.numeric(
      full_entry$bin_start_incl[full_entry$target == "Season peak percentage" &
                                  full_entry$type == "Bin" &
                                  full_entry$location == location]))
  } else if (challenge == "state_ili") {
    max_pkper <- max(as.numeric(
      full_entry_state$bin_start_incl[full_entry_state$target == "Season peak percentage" &
                                        full_entry_state$type == "Bin" &
                                        full_entry_state$location == location]))
  } else {
    max_pkper <- max(as.numeric(
      full_entry_hosp$bin_start_incl[full_entry_hosp$target == "Season peak rate" &
                                       full_entry_hosp$type == "Bin" &
                                       full_entry_hosp$age_grp == location]))
  }
 
  if (pkper > max_pkper) pkper <- max_pkper
  
  
  # Only create peak if at least three weeks of decline following last peak
  if (tail(weekILI$week, n = 1) - tail(pkwk, n = 1) < 3) {
    pkwk <- NA
    pkper <- NA
  }
  
  # Only create peak if data from after MMWR week 4 in new year 
  # (56/57 in ordered coding)
  if (tail(weekILI$week, n = 1) < (maxMMWR + 4)) {
    pkwk  <- NA
    pkper <- NA
  }
  
  # If peak week > 52/53, reset to MMWR week
  for (i in 1:length(pkwk)) {
    if (!(is.na(pkwk[i])) && pkwk[i] > maxMMWR) {
      pkwk[i] <- pkwk[i] - maxMMWR
    }
  }

  peak_truth <- data.frame(target = c("Season peak week", 
                                      "Season peak percentage"),
                          location = location,
                          forecast_week = as.integer(NA),
                          bin_start_incl = c(pkwk[1], pkper),
                          stringsAsFactors = FALSE)
  if (length(pkwk) > 1) {  
    for (i in 2:length(pkwk)) {
      extra_obs <- data.frame(target = "Season peak week",
                              location = location,
                              forecast_week = as.integer(NA),
                              bin_start_incl = pkwk[i],
                              stringsAsFactors = FALSE)
      peak_truth <- rbind(peak_truth, extra_obs)
    }
  }
  
  peak_truth$bin_start_incl <- ifelse(is.na(peak_truth$bin_start_incl),
                                      peak_truth$bin_start_incl,
                                      format(round(peak_truth$bin_start_incl, 1), trim = T, nsmall = 1))
  
  # Rename returned file to have correct column names for hospitalizations
  if (challenge == "hospital") {
    peak_truth <- peak_truth %>%
      rename(age_grp = location) %>%
      mutate(target = ifelse(target == "Season peak percentage", "Season peak rate", 
                             target))
  }

  return(peak_truth)
}  
