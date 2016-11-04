#' Verify the column names of an entry
#'
#' Compares column names to a valid entry and provides an error if any names
#' are missing and a warning if there are any extra names.
#'
#' @param entry An entry data.frame
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
verify_colnames <- function(entry) {
  
  names(entry) <- tolower(names(entry))
  
  entry_names <- colnames(entry)
  valid_names <- colnames(minimal_entry)

  missing_names <- setdiff(valid_names, entry_names)
  extra_names   <- setdiff(entry_names, valid_names)

  if (length(missing_names)>0)
    stop("Missing these columns: ", paste(missing_names))

  if (length(extra_names)>0)
    warning("These extra columns are ignored: ", paste(extra_names))

  return(invisible(TRUE))
}
