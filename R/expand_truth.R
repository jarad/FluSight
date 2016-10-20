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
    dplyr::filter(target %in% c("Season onset", "Season peak week")) %>%
    rowwise %>%
    expand_week(., expand = week_expand)

  percent_targets <- truth %>%
    dplyr::filter(!(target %in% c("Season onset", "Season peak week"))) %>%
    rowwise %>%
    expand_percent(., expand = percent_expand)

  dplyr::bind_rows(week_targets, percent_targets)
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
  
  # Remove regions with no onset
  no_onset <- filter(truth, bin_start_incl == "none")

  truth <- filter(truth, bin_start_incl != "none") %>%
            mutate(bin_start_incl = as.numeric(bin_start_incl))
  
  # Expand known truth  
  expand_week <- data.frame()
  
  for(i in 1:nrow(truth)) {
    lower <- truth$bin_start_incl[i] - expand
    upper <- truth$bin_start_incl[i] + expand
    for(j in seq(lower, upper, 1)) {
      new_truth <- mutate(truth[i, ], bin_start_incl = j)
      expand_week <- rbind(expand_week, new_truth)
    }
  }
  
  # Remove any repeated values in the case of multiple peaks
  expand_week <- unique(expand_week)
  
  # Delete any edge cases
  expand_week <- filter(expand_week, 
                        (bin_start_incl >= 42 | bin_start_incl <= 18) &
                          !is.na(bin_start_incl))
  
  # Deal with week 52/week 1 being sequential
  expand_week$bin_start_incl[expand_week$bin_start_incl > 52] <-
    expand_week$bin_start_incl[expand_week$bin_start_incl > 52] - 52
  expand_week$bin_start_incl[expand_week$bin_start_incl < 1] <-
    expand_week$bin_start_incl[expand_week$bin_start_incl < 1] + 52
  
  # Add back in any regions with no onset
  expand_week <- rbind(expand_week, no_onset)
  
  return(expand_week)
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

  expand_percent <- data.frame()
 
  for(i in 1:nrow(truth)) {
    lower <- truth$bin_start_incl[i] - expand*0.1
    upper <- truth$bin_start_incl[i] + expand*0.1
    for(j in seq(lower, upper, 0.1)) {
      new_truth <- mutate(truth[i, ], bin_start_incl = j)
      expand_percent <- rbind(expand_percent, new_truth)
    }
  }
  
  # Delete any edge cases
  expand_percent <- filter(expand_percent, 
                        bin_start_incl >= 0 & bin_start_incl <= 13)
  
  return(expand_percent)
}

test <- expand_truth(full_truth)
