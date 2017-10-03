#' Verify supplied weighted ILI data
#' 
#' Verifies user-supplied wILI data to be used to generate forecasting targets
#'
#' @param weekILI A data.frame of weighted ILI values. 
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for (default \code{"ilinet"})
#' @return \code{NULL} or a descriptive error or warning message
#' @export
#' @examples 
#' verify_ILI(valid_ILI)
#' 
verify_ILI <- function(weekILI, challenge = "ilinet") {

  if (!(challenge %in% c("ilinet", "state_ili"))) {
    stop("challenge must be one of ilinet or state_ili")
  }
  
  # Verify column names
  FluSight::verify_ILI_colnames(weekILI)
  
  # Verify locations
  FluSight::verify_ILI_location(weekILI, challenge)

}

#' Verify supplied weighted ILI data columns
#' 
#' Verifies columns in user-supplied wILI data 
#'
#' @param weekILI A data.frame of weighted ILI values. 
#' @return \code{NULL} or a descriptive error or warning message
#' @keywords internal
#' @export
#' 
verify_ILI_colnames <- function(weekILI) {
  sub_names <- colnames(weekILI)
  valid_names <- c("location", "week", "ILI")
  
  missing_names <- setdiff(valid_names, sub_names)
  extra_names   <- setdiff(sub_names, valid_names)
  
  if (length(missing_names)>0)
    stop("Missing these columns: ", paste(missing_names))
  
  if (length(extra_names)>0)
    warning("These extra columns are ignored: ", paste(extra_names))

}

#' Verify supplied weighted ILI regions
#' 
#' Verifies regions in user-supplied wILI data 
#'
#' @param weekILI A data.frame of weighted ILI values.
#' @param challenge one of "ilinet" or "state_ili", indicating which
#'   challenge the submission is for (default \code{"ilinet"}) 
#' @return \code{NULL} or a descriptive error or warning message
#' @export
#' @keywords internal
#' 
verify_ILI_location <- function(weekILI, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "state_ili"))) {
    stop("challenge must be one of ilinet or state_ili")
  }
  
  sub_locations <- unique(weekILI$location)
  
  if (challenge == "ilinet") {
    valid_locations <- unique(FluSight::full_entry$location)
  } else {
    valid_locations <- unique(FluSight::full_entry_state$location)
  }
  
  missing_names <- setdiff(valid_locations, sub_locations)
  extra_names   <- setdiff(sub_locations, valid_locations)
  
  if (length(missing_names)>0)
    warning("Missing these locations: ", paste(missing_names), "; no targets
            will be generated for these locations")
  
  if (length(extra_names)>0)
    stop("These location names are invalid: ", paste(extra_names))
  
}