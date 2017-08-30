#' Verify validity of point predictions
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for
#' @import dplyr
#' @return Invisibly returns \code{TRUE} or a descriptive warning/error 
#' message
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples 
#' verify_point(minimal_entry)
#' verify_point(full_entry_hosp, challenge = "hosptial")
verify_point <- function(entry, challenge = "ilinet") {

  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  names(entry) <- tolower(names(entry))
  
  point <- entry %>%
    filter(type == "Point") %>%
    mutate(miss     = is.na(value),
           negative = (!is.na(value) & value < 0))
  
  # Rename age_grp in hospital to integrate with later code
  if (challenge == "hospital") {
    point <- rename(point, location = age_grp)
  }

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
