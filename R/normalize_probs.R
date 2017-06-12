#' Normalize probabilistic predictions
#'
#' Normalizes all probabilistic predictions so that the cumulative probability 
#' for a target sums to 1.
#'
#' @param entry A verified entry data.frame with columns location, target, type, unit, 
#'        bin_start_incl, bin_end_incl, and value.
#' @param ignore_invalid logical; if \code{FALSE} all location/target combinations
#'        have probabilities normalized: if \code{TRUE} location/target
#'        combinations with probabilities summing to < 0.9 or > 1.1 are not
#'        normalized. Should be \code{TRUE} and run in combination with 
#'        \code{\link{remove_invalid}} if run on an entry prior to scoring. 
#' 
#' @return An entry data.frame with normalized probabilities
#' @import dplyr
#' @export
#' @seealso \code{\link{verify_entry}} \code{\link{remove_invalid}}
#' @examples
#' norm_entry <- normalize_probs(full_entry)

normalize_probs <- function(entry, ignore_invalid = FALSE) {
  
  names(entry) <- tolower(names(entry))
  
  if (ignore_invalid == FALSE) {
    # Verify entry first to find implausible values
    # verify_entry(entry)
    
    # Identify targets where cumulative probability is not = 1
    to_normal <- entry %>%
      filter(type == "Bin") %>%
      group_by(location, target) %>%
      summarize(total = sum(value)) %>%
      filter(abs(total - 1) > 1e-8)
  } else {
    # Identify targets where cumulative probability is not = 1
     to_normal <- entry %>%
      filter(type == "Bin") %>%
      group_by(location, target) %>%
      summarize(total = sum(value)) %>%
      filter(abs(total - 1) > 1e-8 & total < 1.1 & total > 0.9)
  } 
  
  # Return entry if no rows need to be normalized
  if (nrow(to_normal) == 0) return(entry)
  else {
    # Loop through groups identified and normalize probabilities
    for(i in 1:nrow(to_normal)) {
      entry$value[entry$location == to_normal$location[i] & 
                   entry$target == to_normal$target[i] &
                   entry$type == "Bin"] <-
        entry$value[entry$location == to_normal$location[i] & 
                      entry$target == to_normal$target[i] &
                      entry$type == "Bin"] /
        sum(entry$value[entry$location == to_normal$location[i] & 
                          entry$target == to_normal$target[i] &
                          entry$type == "Bin"])
      
    }
  
    return(entry) 
  }
}
