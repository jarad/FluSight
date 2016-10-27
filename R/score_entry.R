#' Scores an entry
#'
#' @param entry An entry data.frame with columns location, target,
#' bin_start_incl, and value
#' @param truth A data.frame containing all true values with columns 
#' location, target, forecast_wk, and bin_start_incl. If multiple bins are 
#' considered correct for a given target, all correct bins must be included
#' here.
#' @import dplyr
#' @return A data.frame of scores for each target
score_entry <- function(entry, truth) {

  seasonal <- entry %>%
    filter(type == "Bin", target %in% c("Season onset", "Season peak week",
                                        "Season peak percentage")) %>%
    inner_join(truth, by=c("location", "target",
                           "bin_start_incl")) %>%
    select(-forecast_week.y,
           forecast_week = forecast_week.x)
  
  weekly <- entry %>%
    filter(type == "Bin", target %in% c("1 wk ahead", "2 wk ahead",
                                        "3 wk ahead", "4 wk ahead")) %>%
    inner_join(truth, by=c("location", "target",
                           "bin_start_incl", "forecast_week"))           
               
  scores <- bind_rows(seasonal, weekly) %>%             
              group_by(location, target, forecast_week) %>%
              mutate(score = log(sum(value))) %>%
              select(location, target, score, forecast_week) %>%
              unique()
  
  return(scores)
}
