#' Verify the column names of an entry
#'
#' Compares column names to a valid entry and provides an error if any required names
#' are missing and a warning if there are any extra names.
#'
#' @param entry An entry data.frame
#' @param check_week A logical value (default `TRUE`) indicating whether to check
#'   for the column forecast_week. Should be `TRUE` if evaluating entry prior to 
#'   scoring, can be `FALSE` if evaluating entry prior to writing to disk.
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
verify_colnames <- function(entry, check_week = T) {
  
  names(entry) <- tolower(names(entry))
  
  entry_names <- colnames(entry)
  valid_names <- colnames(FluSight::full_entry_score)

  missing_names <- setdiff(valid_names, entry_names)
  extra_names   <- setdiff(entry_names, valid_names)

  if (length(missing_names) > 0) {
    if (length(missing_names) == 1 && missing_names == "forecast_week") {
      if (check_week == T) {
        warning("Missing forecast_week - verification will proceed but forecast cannot be scored")
      }
    } else {
      stop("Missing these columns: ", paste(missing_names))
    }
  }
  if (length(extra_names)>0)
    warning("These extra columns are ignored: ", paste(extra_names))

  return(invisible(TRUE))
}


