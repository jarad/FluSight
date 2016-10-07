#' Creates a range of bins to score
#'
#' Expands the observed truth across a range of bins
#'
#' @param truth A data.frame including columns unit and bin_start_incl
#' @param week_expand A scalar numeric (default 1) indicating the number of
#' bins to include around the observed value for week targets.
#' @param percent_expand A scalar numeric (default 5) indicating the number
#' of bins to include around the observed value for percent targets.
#' @import magrittr
#' @return A data.frame with expanded truth
#' @export
expand_truth <- function(truth, week_expand=1, percent_expand=5) {
  week_targets <- truth %>%
    dplyr::filter(target == "week") %>%
    rowwise %>%
    expand_week(., expand = week_expand)

  percent_targets <- truth %>%
    dplyr::filter(target == "percent") %>%
    rowwise %>%
    expand_percent(., expand = percent_expand)

  dplyr::bind_rows(week_targets, percent_targets_)
}


#' Expands truth for weeks
#'
#' The week target is peculiar because MMWR weeks run from 1-52 (or 53
#' depending on the year) and week 1 is sequentially, immediately after week 52
#' (or 53).
#'
#' @param truth A data.frame with a single row and, at minimum, columns unit
#' and value
#' @param expand The number of weeks to expand around the observed truth.
#' @return A data.frame with the same columns as `truth` but additional rows
#' around the observed value.
#' @export
#' @keywords internal
expand_week <- function(truth, expand) {
  # Do not expand Season onset if truth is "none"
  if (identical(truth$value, "none")) return(truth)

  warning("Work in progress...")

  return(truth)
}



#' Expands truth for percents
#'
#' @param truth A data.frame with a single row and, at minimum, columns unit
#' and value
#' @param expand The number of percent bins to expand around the observed truth.
#' @return A data.frame with the same columns as `truth` but additional rows
#' around the observed value.
#' @export
#' @keywords internal
expand_percent <- function(truth, expand) {


  return(truth)
}
