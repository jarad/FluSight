#' Normalize probabilistic predictions
#'
#' Checks an entry for validity and, if valid, normalizes all probabilistic
#' predictions so that the cumulative probability for a target sums to 1.
#'
#' @param entry An entry data.frame with columns location, target, type, unit, 
#'        bin_start_incl, bin_end_incl, and value.
#' @return An entry data.frame with normalized probabilities
#' @import dplyr
#' @export
#' @seealso \code{\link{verify_entry}}
#' @examples
#' norm_entry <- normalize_probs(full_entry)

normalize_probs <- function(entry) {
  
  # Verify entry first to find implausible values
  verify_entry(entry)
  
  # Identify targets where cumulative probability is not = 1
  to_normal <- entry %>%
                filter(type == "Bin") %>%
                group_by(location, target) %>%
                summarize(total = sum(value)) %>%
                filter(abs(total[1] - 1) > 1e-8)

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
