#' Verify the entry probabilities
#'
#' @param entry An entry data.frame
#' @import dplyr
#' @return Invisibly returns \code{TRUE} or a descriptive error message
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples 
#' verify_probabilities(minimal_entry)
verify_probabilities <- function(entry) {

  names(entry) <- tolower(names(entry))
  
  probabilities = entry %>%
    filter(type=="Bin") %>%
    group_by(location,target) %>%
    summarize(miss     = any(is.na(value)),
              sum      = sum(value, na.rm = TRUE),
              negative = any(!is.na(value) & value < 0))

  errors <- character()

  # Report message for missing probabilities
  if (any(probabilities$miss)) {
    tmp <- probabilities %>%
      filter(miss)

    errors <- c(errors, paste0("ERROR: Missing probabilities detected in ",
                               paste(tmp$location, tmp$target), ".\n"))
  }

  # Report message for negative probabilities
  if (any(probabilities$negative)) {
    tmp <- probabilities %>%
      filter(negative)

    errors <- c(errors, paste0("ERROR: Negative probabilities detected in ",
                               paste(tmp$location, tmp$target), ".\n"))
  }

  # Report message for sum of target probabilities outside of 0.9 and 1.1
  if (any(probabilities$sum < 0.9 | probabilities$sum > 1.1)) {
    tmp <- probabilities %>%
      filter(sum<0.9 | sum>1.1)

    errors <- c(errors, paste0("ERROR: In ", tmp$location, "-", tmp$target, ", probabilities sum to ",
                               tmp$sum, ". \n"))
  }

  #Output probability related errors
  if (length(errors) != 0) {
    stop(errors)
  }
  
  return(invisible(TRUE))
}
