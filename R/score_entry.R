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

  entry %>%
    filter(type == "Bin") %>%
    right_join(truth, by=c("location", "target",
                           "bin_start_incl")) %>%  # Will need to add forecast_wk here before bin_start_incl
    group_by(location, target, forecast_wk) %>%
    mutate(score = log(sum(value))) %>%
    select(location, target, score, forecast_wk) %>%
    unique()
}


