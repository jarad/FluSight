#' Verify the locations
#'
#' Compares locations to minimal and full entries and provides an error if any
#' locations are missing, a warning if there are any extra locations, and a
#' message to suggest forecasting for additional locations.
#'
#' @param entry An entry data.frame
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
verify_locations <- function(entry) {
  
  names(entry) <- tolower(names(entry))
  
  entry_locations    <- unique(entry$location)
  required_locations <- unique(minimal_entry$location)
  valid_locations    <- unique(full_entry$location)

  missing_locations <- setdiff(required_locations, entry_locations)
  extra_locations   <- setdiff(entry_locations, valid_locations)
  possible_locations <- setdiff(valid_locations, entry_locations)

  if (length(missing_locations)>0)
    stop("Missing these locations: ", paste(missing_locations))

  if (length(extra_locations)>0)
    warning("These extra locations are ignored: ", paste(extra_locations))

  if (length(possible_locations)>0)
    message("Consider forecasting for these locations: ", paste(possible_locations))

  return(invisible(TRUE))
}
