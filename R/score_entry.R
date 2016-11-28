#' Scores an entry
#'
#' @param entry An entry data.frame with columns location, target,
#' bin_start_incl, value, and forecast week
#' @param truth A data.frame containing all true values with columns 
#' location, target, forecast_wk, and bin_start_incl. If multiple bins are 
#' considered correct for a given target, all correct bins must be included
#' here.
#' @seealso \code{\link{expand_truth}}
#' @import dplyr
#' @return A data.frame of scores for each target
#' @export
#' @examples 
#' scores < - score_entry(full_entry_week, truth_1516)
#' 
score_entry <- function(entry, truth) {

  names(entry) <- tolower(names(entry))
  names(truth) <- tolower(names(truth))
  

  # Check for missing forecast week
  if (!("forecast_week" %in% names(entry)))
    stop("Forecast week needed in entry - 
         use read_entry() with your submission CSV")
  
  seasonal <- entry %>%
    filter(type == "Bin", target %in% c("Season onset", "Season peak week",
                                        "Season peak percentage")) %>%
    right_join(truth, by=c("location", "target",
                           "bin_start_incl")) %>%
    filter(target %in% c("Season onset", "Season peak week",
                         "Season peak percentage")) %>%
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
              unique()
  
  # If score < -10 or forecast is missing, set to -10
  scores$score[scores$score < -10 | is.na(scores$score)] <- -10
  
  return(scores)
}
