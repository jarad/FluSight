#' Verify targets are correct
#'
#' The necessary targets depend on the target, so this will verify targets are correct
#' for all targets
#'
#' @param entry An entry data.frame
#' @return Invisibly returns TRUE if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_targets(minimal_entry)
verify_targets <- function(entry) {
  valid_targets <- unique(minimal_entry$target)
  entry_targets <- unique(entry$target)
  
  missing_targets <- setdiff(valid_targets, entry_targets)
  extra_targets   <- setdiff(entry_targets, valid_targets)

  if (length(missing_targets)>0)
    stop("Missing these targets: ", paste(missing_targets))

  if (length(extra_targets)>0)
    warning("These extra targets are ignored: ", paste(extra_targets))
  
  return(invisible(TRUE))
}
