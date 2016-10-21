#' Verify supplied weighted ILI data
#' 
#' Verifies user-supplied wILI data to be used to generate forecasting targets
#'
#' @param weekILI A data.frame of weighted ILI values. 
#' @return \code{NULL} or a descriptive error or warning message
#' @export
#' @examples 
#' verify_ILI(valid_ILI)
#' 
verify_ILI <- function(weekILI) {
  
  # Verify column names
  verify_ILI_colnames(weekILI)
  
  # Verify locations
  verify_ILI_location(weekILI)

}

#' Verify supplied weighted ILI data columns
#' 
#' Verifies columns in user-supplied wILI data 
#'
#' @param weekILI A data.frame of weighted ILI values. 
#' @return \code{NULL} or a descriptive error or warning message
#' @keywords internal
#' @export
#' 
verify_ILI_colnames <- function(weekILI) {
  sub_names <- colnames(weekILI)
  valid_names <- c("location", "week", "wILI")
  
  missing_names <- setdiff(valid_names, sub_names)
  extra_names   <- setdiff(sub_names, valid_names)
  
  if (length(missing_names)>0)
    stop("Missing these columns: ", paste(missing_names))
  
  if (length(extra_names)>0)
    warning("These extra columns are ignored: ", paste(extra_names))

}

#' Verify supplied weighted ILI regions
#' 
#' Verifies regions in user-supplied wILI data 
#'
#' @param weekILI A data.frame of weighted ILI values. 
#' @return \code{NULL} or a descriptive error or warning message
#' @export
#' @keywords internal
#' 
verify_ILI_location <- function(weekILI) {
  sub_locations <- unique(weekILI$location)
  valid_locations <- c("US National", "HHS Region 1", "HHS Region 2", 
                       "HHS Region 3", "HHS Region 4", "HHS Region 5", 
                       "HHS Region 6", "HHS Region 7", "HHS Region 8", 
                       "HHS Region 9", "HHS Region 10")
  
  missing_names <- setdiff(valid_locations, sub_locations)
  extra_names   <- setdiff(sub_locations, valid_locations)
  
  if (length(missing_names)>0)
    warning("Missing these locations: ", paste(missing_names), "; no targets
            will be generated for these locations")
  
  if (length(extra_names)>0)
    stop("These location names are invalid: ", paste(extra_names))
  
}