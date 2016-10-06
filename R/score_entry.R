#' Scores an entry
#'
#' @param entry An entry data.frame with columns location, target,
#' bin_start_incl, and value
#' @param truth A data.frame with columns location, target, and bin_start_incl
#' @import dplyr
#' @return A data.frame of scores for each target
score_entry <- function(entry, truth) {
  entry %>%
    filter(type == "Bin") %>%
    right_join(truth, by=c("location","target","bin_start_incl"))
}
