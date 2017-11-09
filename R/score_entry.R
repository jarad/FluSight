#' Scores an entry
#' 
#' Scores an entry based on the observed truth provided. \code{\link{verify_entry}}
#' should be run prior to scoring, along with \code{\link{remove_invalid}} if 
#' invalid probabilities are detected.
#'
#' @param entry A valid entry data.frame with columns location, target,
#' bin_start_incl, value, and forecast week
#' @param truth A data.frame containing all true values with columns 
#' location, target, forecast_wk, and bin_start_incl. If multiple bins are 
#' considered correct for a given target, all correct bins must be included
#' here.
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating
#' which challenge the submission is for (default \code{"ilinet"}).
#' @seealso \code{\link{expand_truth}} \code{\link{verify_entry}} \code{\link{remove_invalid}}
#' @import dplyr
#' @return A data.frame of scores for each target
#' @export
#' @examples 
#' scores <- score_entry(full_entry_week, truth_1516)
#' scores <- score_entry(full_entry_hosp_score, hosp_truth_1617, challenge = "hospital")
#' scores <- score_entry(full_entry_state_score, state_truth_1617, challenge = "state_ili")
#' 
score_entry <- function(entry, truth, challenge = "ilinet") {

  names(entry) <- tolower(names(entry))
  names(truth) <- tolower(names(truth))
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }

  # Check for missing forecast week
  if (!("forecast_week" %in% names(entry)))
    stop("Column forecast_week needed in entry - 
         use read_entry() with your submission CSV")
 
  # Rename hospitalization entry for consistency with later code
  if (challenge == "hospital") {
    entry <- rename(entry, location = age_grp)
    truth <- rename(truth, location = age_grp)
  }
  
  seasonal <- entry %>%
    filter(type == "Bin", target %in% c("Season onset", "Season peak week",
                                        "Season peak percentage",
                                        "Season peak rate")) %>%
    right_join(truth, by=c("location", "target",
                           "bin_start_incl")) %>%
    filter(target %in% c("Season onset", "Season peak week",
                         "Season peak percentage",
                         "Season peak rate")) %>%
    select(-forecast_week.x, -forecast_week.y) %>%
    mutate(forecast_week = entry$forecast_week[1])
  
  weekly <- entry %>%
    filter(type == "Bin", target %in% c("1 wk ahead", "2 wk ahead",
                                        "3 wk ahead", "4 wk ahead")) %>%
    right_join(truth, by=c("location", "target",
                           "bin_start_incl", "forecast_week")) %>%
    filter(target %in% c("1 wk ahead", "2 wk ahead",
                         "3 wk ahead", "4 wk ahead"),
           forecast_week == entry$forecast_week[1])
               
  scores <- bind_rows(seasonal, weekly) %>%             
              group_by(location, target, forecast_week) %>%
              mutate(score = log(sum(value))) %>%
              select(location, target, score, forecast_week) %>%
              unique() %>%
              ungroup() %>%
      # If score < -10 or forecast is missing, set to -10
              mutate(score = ifelse(score < -10 | is.na(score), -10, score)) 
    
  
  # Rename hospitalization entry for consistency with later code
  if (challenge == "hospital") scores <- rename(scores, age_grp = location)
  
  
  return(scores)
}
