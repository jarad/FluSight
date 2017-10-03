#' Verify an observed hospitalizations file
#'
#' Verifies user-supplied FluSurv-NET hospitalization data to create
#' observed truth
#'
#' @param observe_hosp A data.frame of observed FluSurv-NET values
#' @return \code{NULL} or a descriptive error or warning message
#' @export
#' @examples
#' verify_hosp(valid_observe_hosp)
 

verify_hosp <- function(observe_hosp) {
 
  # Verify column names
  sub_names <- colnames(observe_hosp)
  valid_names <- colnames(FluSight::valid_observe_hosp)
  
  missing_names <- setdiff(valid_names, sub_names)
  extra_names   <- setdiff(sub_names, valid_names)
  
  if (length(missing_names)>0)
    stop("Missing these columns: ", paste(missing_names))
  
  if (length(extra_names)>0)
    warning("These extra columns are ignored: ", paste(extra_names))
  
  
  # Verify age groups
  sub_agegrp <- unique(observe_hosp$age_grp)
  valid_agegrp <- unique(FluSight::valid_observe_hosp$age_grp)
  
  missing_names <- setdiff(valid_agegrp, sub_agegrp)
  extra_names   <- setdiff(sub_agegrp, valid_agegrp)
  
  if (length(missing_names)>0)
    warning("Missing these age groups: ", paste(missing_names), "; no targets
            will be generated for these age groups")
  
  if (length(extra_names)>0)
    stop("These age groups are invalid: ", paste(extra_names))
}