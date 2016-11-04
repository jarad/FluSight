#' Verify bins are correct
#'
#' The necessary bins depend on the target, so this will verify bins are correct
#' for all targets
#'
#' @param entry An entry data.frame
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_bins(minimal_entry)
verify_bins <- function(entry) {
  
  names(entry) <- tolower(names(entry))
  
  entry_targets <- unique(entry$target)  
  
  errors <- character()
  warnings <- character()
  
  for(i in seq_along(entry_targets)) {
    entry_bins <- unique(entry$bin_start_incl[entry$target == entry_targets[i] & 
                                                entry$type == "Bin"])
    valid_bins <- unique(minimal_entry$bin_start_incl[minimal_entry$target ==
                                                        entry_targets[i] &
                                                      minimal_entry$type ==
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

