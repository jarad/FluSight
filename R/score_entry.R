#' Scores an entry
#'
#' @param entry An entry data.frame
#' @param truth A data.frame of true values
#' @param percent_bin_width An integer (default 5) indicating the number of
#' surrounding bins in each direction to sum for percent targets.
#' @param week_bin_width An integer (default 1) indicating the number of
#' surounding bins in each direction to sum for week targets.
#' @import dplyr
#' @return A data.frame of scores for each target
score_entry <- function(entry, truth, percent_bin_width = 5,
                        week_bin_width = 1) {

}
