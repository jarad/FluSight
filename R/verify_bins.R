#' Verify bins are correct
#'
#' The necessary bins depend on the target, so this will verify bins are correct
#' for all targets
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating which
#'   challenge the submission is for
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_bins(minimal_entry)
verify_bins <- function(entry, challenge = "ilinet") {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("challenge must be one of ilinet, hospital, or state_ili")
  }
  
  if (challenge == "ilinet") {
    valid <- FluSight::minimal_entry
  } else if (challenge == "hospital") {
    valid <- FluSight::full_entry_hosp
  } else {
    valid <- FluSight::full_entry_state
  }
  
  names(entry) <- tolower(names(entry))
  
  entry_targets <- unique(entry$target)  
  
  errors <- character()
  warnings <- character()
  
  for(i in seq_along(entry_targets)) {
    entry_bins <- unique(entry$bin_start_incl[entry$target == entry_targets[i] & 
                                                entry$type == "Bin"])
    valid_bins <- unique(valid$bin_start_incl[valid$target ==
                                                entry_targets[i] &
                                                valid$type ==
                                                "Bin"])
    missing_bins <- setdiff(valid_bins, entry_bins)
    extra_bins <- setdiff(entry_bins, valid_bins)
    
    if (length(missing_bins) > 0)
      errors <- c(errors, paste0("Missing these bins for ", 
                                     entry_targets[i], ": ", missing_bins, "\n"))
    
    if (length(extra_bins) > 0)
      warnings <- c(warnings, paste0("These extra bins for ",
                                      entry_targets[i], " are ignored: ",
                                      extra_bins, "\n"))
  }
  
  if (length(errors) > 0)
    stop(errors)
  
  if (length(warnings) > 0)
    warning(warnings)
  
  return(invisible(TRUE))
}

