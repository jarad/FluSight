#' Verify the age groups in hospitalization forecasts
#'
#' Compares age groups to full entries and provides a warning if any
#' age groups are missing, an error if there are any extra age groups, and a
#' message to suggest forecasting for additional locations.
#'
#' @param entry An entry data.frame
#' @return Invisibly returns \code{TRUE} if successful
#' @export
#' @keywords internal
verify_agegrp <- function(entry) {
  
  names(entry) <- tolower(names(entry))
  
  entry_agegrp    <- unique(entry$age_grp)
  required_agegrp <- "Overall"
  valid_agegrp    <- unique(FluSight::full_entry_hosp$age_grp)

  missing_agegrp <- setdiff(required_agegrp, entry_agegrp)
  extra_agegrp   <- setdiff(entry_agegrp, valid_agegrp)
  possible_agegrp <- setdiff(valid_agegrp, entry_agegrp)

  if (length(missing_agegrp)>0)
    stop("Missing these age groups: ", paste(missing_agegrp))

  if (length(extra_agegrp)>0)
    stop("These extra age groups are invalid: ", paste(extra_agegrp))

  if (length(possible_agegrp)>0)
    message("Consider forecasting for these age groups: ", paste(possible_agegrp))

  return(invisible(TRUE))
}
