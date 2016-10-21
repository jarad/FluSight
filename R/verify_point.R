#' Verify validity of point predictions
#'
#' @param entry An entry data.frame
#' @import dplyr
#' @return Invisibly returns \code{TRUE} or a descriptive warning/error 
#' message
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples 
#' verify_point(minimal_entry)
verify_point <- function(entry) {

  point = entry %>%
    filter(type == "Point") %>%
    mutate(miss     = is.na(value),
           negative = (!is.na(value) & value < 0))


  # Report warning for missing point predictions
  if (any(point$miss)) {
    tmp <- point %>%
      filter(miss)

    warning(paste0("WARNING: Missing point predictions detected in ",
                   paste(tmp$location, tmp$target), ". \n",
                   "Please take a look at the generate_point_forecasts function.\n"))
  }

  # Report error for negative point predictions
  if (any(point$negative)) {
    tmp <- point %>%
      filter(negative)

    stop(paste0("ERROR: Negative point predictions detected in ",
                paste(tmp$location, tmp$target), ". \n",
                "Please take a look at the generate_point_forecasts function.\n"))
  }
  
  return(invisible(TRUE))
}
