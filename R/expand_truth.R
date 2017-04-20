#' Creates a range of bins to score
#'
#' Expands the observed truth across a range of bins
#'
#' @param truth A data.frame including columns unit and bin_start_incl
#' @param week_expand A scalar numeric (default 1) indicating the number of
#' bins to include around the observed value for week targets.
#' @param percent_expand A scalar numeric (default 5) indicating the number
#' of bins to include around the observed value for percent targets.
#' @param week53 A logical value (default `FALSE`) indicating whether to include
#' MMWR week 53 in the expanded truth
#' @import dplyr
#' @return A data.frame with expanded truth
#' @export
expand_truth <- function(truth, week_expand=1, percent_expand=5, week53 = F) {

  names(truth) <- tolower(names(truth))

  # Pull out targets that have no observed truth yet
  na_rows <- truth %>%
    filter(is.na(bin_start_incl))
  truth <- truth %>%
    filter(!(is.na(bin_start_incl)))
  
  # Weekly targets
  week_targets <- truth %>%
    filter(target %in% c("Season onset", "Season peak week")) %>%
    rowwise %>%
    expand_week(., expand = week_expand, week53 = week53)

  # Percentage targets
  percent_targets <- truth %>%
    filter(!(target %in% c("Season onset", "Season peak week"))) %>%
    rowwise %>%
    expand_percent(., expand = percent_expand)

  # Combine weekly targets, percentage targets, and unobserved truth
  bind_rows(week_targets, percent_targets, na_rows) %>%
    mutate(bin_start_incl = trimws(replace(bin_start_incl,
                                  !is.na(bin_start_incl) & bin_start_incl != "none",
                                  format(round(as.numeric(
                                  bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                  , 1), nsmall = 1))))
  
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
#' @param week53 A logical value (default `FALSE`) indicating whether to include
#' MMWR week 53 in the expanded truth
#' @return A data.frame with the same columns as `truth` but additional rows
#' around the observed value.
#' @import dplyr
#' @export
#' @keywords internal
expand_week <- function(truth, expand, week53 = F) {
  
  # Set max MMWR week
  if (week53 == TRUE) maxMMWR <- 53
  if (week53 == FALSE) maxMMWR <- 52
  
  # Remove regions with no onset
  no_onset <- filter(truth, bin_start_incl == "none")

  truth <- truth %>%
            filter(bin_start_incl != "none") %>%
            mutate(bin_start_incl = as.numeric(bin_start_incl))
  
  # If no onset anywhere and all peak NA, then return df of no_onset
  if (nrow(truth) == 0) return(no_onset)
  
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
  expand_week <- unique(expand_week) %>%
                  filter((bin_start_incl >= 42 | bin_start_incl <= 18) &
                          !is.na(bin_start_incl))
  
  # Deal with week 52/week 1 being sequential
  expand_week$bin_start_incl[expand_week$bin_start_incl > maxMMWR] <-
    expand_week$bin_start_incl[expand_week$bin_start_incl > maxMMWR] - maxMMWR
  expand_week$bin_start_incl[expand_week$bin_start_incl < 1] <-
    expand_week$bin_start_incl[expand_week$bin_start_incl < 1] + maxMMWR
  
  # Add back in any regions with no onset
  expand_week <- rbind(expand_week, no_onset) %>%
    arrange(location, target) %>%
    mutate(bin_start_incl = replace(bin_start_incl,
                                    !is.na(bin_start_incl) & bin_start_incl != "none",
                                    format(round(as.numeric(
                                      bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                      , 1), nsmall = 1, trim = T)))

  return(expand_week)
}


#' Expands truth for percents
#'
#' @param truth A data.frame with a single row and, at minimum, columns unit
#' and value
#' @param expand The number of percent bins to expand around the observed truth.
#' @return A data.frame with the same columns as `truth` but additional rows
#' around the observed value.
#' @import dplyr
#' @export
#' @keywords internal
expand_percent <- function(truth, expand) {

  truth <- mutate(truth, bin_start_incl = as.numeric(bin_start_incl))
  
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
                        bin_start_incl >= 0 & bin_start_incl <= 13) %>%
                  mutate(bin_start_incl = format(round(bin_start_incl, 1), 
                                                 trim = T, nsmall = 1))
  
  return(expand_percent)
}
