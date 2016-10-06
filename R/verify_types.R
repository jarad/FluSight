#' Verify types are correct
#'
#' The necessary types depend on the type, so this will verify types are correct
#' for all types
#'
#' @param entry An entry data.frame
#' @return Invisibly returns TRUE if successful
#' @export
#' @keywords internal
#' @seealso \code{\link{verify_entry}}
#' @examples
#' verify_types(minimal_entry)
verify_types <- function(entry) {
  valid_types <- unique(minimal_entry$type)
  entry_types <- unique(entry$type)
  
  missing_types <- setdiff(valid_types, entry_types)
  extra_types   <- setdiff(entry_types, valid_types)

  if (length(missing_types)>0)
    stop("Missing these types: ", paste(missing_types))

  if (length(extra_types)>0)
    warning("These extra types are ignored: ", paste(extra_types))
  
  return(invisible(TRUE))
}
