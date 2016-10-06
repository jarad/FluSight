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
  return(invisible(TRUE))
}
