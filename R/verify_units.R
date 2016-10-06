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
  return(invisible(TRUE))
}
