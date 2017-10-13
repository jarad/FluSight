#' Creates a range of bins to score
#'
#' Expands the observed truth across a range of bins
#'
#' @param truth A data.frame including columns unit and bin_start_incl
#' @param week_expand A scalar numeric (default 1) indicating the number of
#' bins to include around the observed value for week targets.
#' @param percent_expand A scalar numeric (default 5) indicating the number
#' of bins to include around the observed value for percent or rate targets.
#' If \code{expand_by_percent = TRUE} then this value is ignored.
#' @param week53 A logical value (default `FALSE`) indicating whether to 
#' include MMWR week 53 in the expanded truth.
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating
#' which challenge the submission is for (default \code{"ilinet"}).
#' @param expand_by_percent A logical value (default `FALSE`) indicating 
#' whether to expand truth around percent or ratetargets by a percentage 
#' of the observed value. 
#' @param percent_observed A scalar numeric between 0 and 1 (default 0.05),
#' indicating the one-sided percentage of the observed percent or rate target
#' that will be included. If \code{expand_by_percent = FALSE} then this
#' value is ignored.
#' @import dplyr
#' @return A data.frame with expanded truth
#' @export
expand_truth <- function(truth, week_expand=1, percent_expand=5, week53 = F,
                         challenge = "ilinet", expand_by_percent = F,
                         percent_observed = 0.05) {

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
    FluSight::expand_week(., expand = week_expand, week53 = week53)

  # Percentage or rate targets
  percent_targets <- truth %>%
    filter(!(target %in% c("Season onset", "Season peak week"))) %>%
    rowwise %>%
    FluSight::expand_percent(., challenge = challenge, 
                   percent_expand = percent_expand,
                   expand_by_percent = expand_by_percent,
                   percent_observed = percent_observed)

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
  expand_week <- mutate(expand_week, bin_start_incl = replace(bin_start_incl,
                                    !is.na(bin_start_incl) & bin_start_incl != "none",
                                    format(round(as.numeric(
                                      bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                      , 1), nsmall = 1, trim = T))) %>%
    bind_rows(no_onset)

  return(expand_week)
}


#' Expands truth for percents
#'
#' @param truth A data.frame with a single row and, at minimum, columns unit
#' and value
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating
#' which challenge the submission is for.
#' @param expand_by_percent A logical value indicating whether to expand truth
#' around percent or rate targets by a percentage of the observed value. 
#' @param percent_expand The number of percent bins to expand around the 
#' observed truth. If \code{expand_by_percent = TRUE} then this value is 
#' ignored.
#' @param percent_observed A scalar numeric between 0 and 1, indicating the 
#' one-sided percentage of the observed percent or rate target that will be 
#' included. If \code{expand_by_percent = FALSE} then this value is ignored.
#' @return A data.frame with the same columns as `truth` but additional rows
#' around the observed value.
#' @import dplyr
#' @export
#' @keywords internal
expand_percent <- function(truth, percent_expand, challenge = "ilinet", 
                           expand_by_percent = F, percent_observed = 0.05) {

  truth <- mutate(truth, bin_start_incl = as.numeric(bin_start_incl))
  
  expand_percent <- data.frame()
  
  # Expand with fixed number of bins
  if (!isTRUE(expand_by_percent)) {
    for (i in 1:nrow(truth)) {
      lower <- truth$bin_start_incl[i] - percent_expand*0.1
      upper <- truth$bin_start_incl[i] + percent_expand*0.1
      for(j in seq(lower, upper, 0.1)) {
        expand_percent <- bind_rows(expand_percent, 
                                    mutate(truth[i, ], bin_start_incl = j))
      }
    }
  }
  
  
  # Expand with flexible number of bins - ensure at least one bin
  if (isTRUE(expand_by_percent)) {
    for (i in 1:nrow(truth)) {
      lower <- truth$bin_start_incl[i] - 
        max(round(percent_observed*truth$bin_start_incl[i], 1), 0.1)
      upper <- truth$bin_start_incl[i] + 
        max(round(percent_observed*truth$bin_start_incl[i], 1), 0.1)
      for(j in seq(lower, upper, 0.1)) {
        expand_percent <- bind_rows(expand_percent, 
                                    mutate(truth[i, ], bin_start_incl = j))
      }
    }
  }

  
  
  # Delete any edge cases
  if (challenge == "hospital") {
    expand_percent <- expand_percent %>%
      filter(ifelse(age_grp == "65+ yr",
                    bin_start_incl >= 0 & bin_start_incl <= 60,
                    bin_start_incl >= 0 & bin_start_incl <= 13)) %>%
      mutate(bin_start_incl = format(round(bin_start_incl, 1),
                                     trim = T, nsmall = 1))
  } else {
    expand_percent <- expand_percent %>%
      filter(bin_start_incl >= 0, bin_start_incl <= 13) %>%
      mutate(bin_start_incl = format(round(bin_start_incl, 1),
                                     trim = T, nsmall = 1))
  }
  return(expand_percent)
}
