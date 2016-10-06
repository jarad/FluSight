#' Verify units are correct
#'
#' The necessary units depend on the target, so this will verify units are correct
#' for all units
#'
#' @param entry An entry data.frame
#' @return Invisibly returns TRUE if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_units(minimal_entry)
verify_units <- function(entry) {
  valid_units <- unique(minimal_entry$unit)
  entry_units <- unique(entry$unit)
  
  missing_units <- setdiff(valid_units, entry_units)
  extra_units   <- setdiff(entry_units, valid_units)

  if (length(missing_units)>0)
    stop("Missing these units: ", paste(missing_units))

  if (length(extra_units)>0)
    warning("These extra units are ignored: ", paste(extra_units))
  
  return(invisible(TRUE))
}
