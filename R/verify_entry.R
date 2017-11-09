#' Verify an entry file
#'
#' This function will check to make sure the structure is correct and that
#' the forecast probabilities are non-negative and sum to a value between
#' 0.9 and 1.1.
#'
#' @param file A csv entry file
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @seealso \code{\link{verify_entry}}
#' @examples
#' file <- system.file("extdata", "valid-test.csv", package="FluSight")
#' verify_entry_file(file) # TRUE
verify_entry_file <- function(file, challenge = "ilinet") {
	entry <- FluSight::read_entry(file, challenge)
	FluSight::verify_entry(entry, challenge, check_week = F)
}



#' Verify entry stored as an R data.frame
#'
#' @param entry A data.frame
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for
#' @param check_week A logical value (default `TRUE`) indicating whether to check
#'   for the column forecast_week. Should be `TRUE` if evaluating entry prior to 
#'   scoring, can be `FALSE` if evaluating entry prior to writing to disk.
#' @return Invisibly returns \code{TRUE} if successful
#' @import dplyr
#' @export
#' @seealso \code{\link{verify_entry_file}}
#' @examples
#' verify_entry(minimal_entry)
#' verify_entry(full_entry)
verify_entry <- function(entry, challenge = "ilinet", check_week = T) {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  names(entry) <- tolower(names(entry))

  FluSight::verify_colnames(entry, challenge, check_week)

  # Verify column contents
  if (challenge %in% c("ilinet", "state_ili")) {
    FluSight::verify_locations(entry, challenge)
  } else {
    FluSight::verify_agegrp(entry)
  }
  FluSight::verify_targets(entry, challenge)
  FluSight::verify_types(entry, challenge)
  FluSight::verify_units(entry, challenge)

  FluSight::verify_bins(entry, challenge)
  FluSight::verify_probabilities(entry, challenge)
  FluSight::verify_point(entry, challenge)

	return(invisible(TRUE))
}

