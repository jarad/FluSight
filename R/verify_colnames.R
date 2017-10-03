#' Verify the column names of an entry
#'
#' Compares column names to a valid entry and provides an error if any required names
#' are missing and a warning if there are any extra names.
#'
#' @param entry An entry data.frame
#' @param challenge one of "ilinet", "hospital", or "state_ili", indicating which
#'   forecasting challenge the entry is for (default "ilinet")
#' @param check_week A logical value (default `TRUE`) indicating whether to check
#'   for the column forecast_week. Should be `TRUE` if evaluating entry prior to 
#'   scoring, can be `FALSE` if evaluating entry prior to writing to disk.
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
verify_colnames <- function(entry, challenge = "ilinet", check_week = T) {
  
  if (!(challenge %in% c("ilinet", "hospital", "state_ili"))) {
    stop("Challenge must be one of ilinet, hospital, or state_ili")
  }
  
  names(entry) <- tolower(names(entry))
  
  entry_names <- colnames(entry)
  if (challenge %in% c("ilinet", "state_ili")) {
    valid_names <- colnames(FluSight::full_entry_score)
  } else {
    valid_names <- colnames(FluSight::full_entry_hosp_score)
  }

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


