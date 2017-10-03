#' Verify types are correct
#'
#' The necessary types depend on the type, so this will verify types are correct
#' for all types
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for
#' @return Invisibly returns TRUE if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_types(minimal_entry)
verify_types <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  names(entry) <- tolower(names(entry))
  
  # ILINet challenge
  if (challenge == "ilinet") {
    valid_types <- unique(FluSight::minimal_entry$type)
  } else if (challenge == "hospital") {
    valid_types <- unique(FluSight::full_entry_hosp$type)
  } else if (challenge == "state_ili") {
    valid_types <- unique(FluSight::full_entry_state$type)
  } 
  entry_types <- unique(entry$type)
  
  missing_types <- setdiff(valid_types, entry_types)
  extra_types   <- setdiff(entry_types, valid_types)

  if (length(missing_types)>0)
    stop("Missing these types: ", paste(missing_types))

  if (length(extra_types)>0 && extra_types != "point")
    warning("These extra types are ignored: ", paste(extra_types))
  
  return(invisible(TRUE))
}
