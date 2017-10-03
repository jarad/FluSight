#' Verify units are correct
#'
#' The necessary units depend on the target, so this will verify units are correct
#' for all units
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for
#' @return Invisibly returns TRUE if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_units(minimal_entry)
#' verify_units(full_entry_hosp, challenge = "hospital")
verify_units <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  names(entry) <- tolower(names(entry))
  
  # ILINet challenge
  if (challenge == "ilinet") {
    valid_units <- unique(FluSight::minimal_entry$unit)
  } else if (challenge == "hospital") {
    valid_units <- unique(FluSight::full_entry_hosp$unit)
  } else if (challenge == "state_ili") {
    valid_units <- unique(FluSight::full_entry_state$unit)
  } 
  entry_units <- unique(entry$unit)
  
  missing_units <- setdiff(valid_units, entry_units)
  extra_units   <- setdiff(entry_units, valid_units)

  if (length(missing_units)>0)
    stop("Missing these units: ", paste(missing_units))

  if (length(extra_units)>0)
    warning("These extra units are ignored: ", paste(extra_units))
  
  return(invisible(TRUE))
}
