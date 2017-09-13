#' Generate point forecasts for all locations and targets
#'
#' @param entry An entry data.frame
#' @param method The method to be used to generate the point forecasts. 
#'   \code{"Median"} (the default) uses the median value, \code{"Expected
#'   Value"} generates the expected value from the provided probabilities,and
#'   \code{"Mode"} returns the individual bin with the largest probability
#' @param challenge one of "ilinet", "hospital" or "state_ili", indicating
#' which challenge the submission is for (default \code{"ilinet"}).
#' @return A data.frame of point forecasts for all locations and targets.
#' @seealso \code{\link{generate_point_forecast}}, \code{\link{verify_entry}}
#' @import magrittr
#' @export
generate_point_forecasts <- function(entry, method = 
                                       c("Median", "Expected Value", "Mode"),
                                     challenge = "ilinet") {
  
  method <- match.arg(method)

  names(entry) <- tolower(names(entry))
    
  if (sum(entry$type == "Point") > 0) {
  	warning("It appears point forecasts already exist.")
  }
  
  #Rename columns if hospital forecast for use with later code
  if (challenge == "hospital") {
    entry <- rename(entry, location = age_grp)
  }

  # Generate point forecasts
  entry <- entry %>%
      dplyr::filter(type == "Bin") %>%
      dplyr::group_by(location,target) %>%
      generate_point_forecast(., method)
  
  # Rename columns back
  if (challenge == "hospital") {
    entry <- rename(entry, age_grp = location)
  }
  
  return(entry)
  
}


#' Generate a point forecast from probabilistic forecast
#'
#' The point forecast is taken to be either the expected value, median, 
#' or mode of the probabilistic forecasts.
#'
#' @param d A data.frame with columns `location`, `target`, bin_start_incl`,
#'   and `value`
#' @param method The method to be used to generate the point forecasts. 
#'   \code{"Median"} (the default) uses the median value, \code{"Expected
#'   Value"} generates the expected value from the provided probabilities,and
#'   \code{"Mode"} returns the individual bin with the largest probability 
#' @return A data.frame with columns `type` and `value`
#' @seealso \code{\link{generate_point_forecasts}}
#' @import magrittr
#' @export
#' @keywords internal
generate_point_forecast <- function(d, method = 
                                     c("Median", "Expected Value", "Mode")) {
  
  if (packageVersion("dplyr") < "0.7.0") {
    stop("dplyr >= 0.7.0 needed for this function.", call. = FALSE)
  }
  
  method <- match.arg(method)
  
  names(d) <- tolower(names(d))
  
  # Find max MMWR week in submitted entry
  maxMMWR <- d %>%
    dplyr::filter(target %in% c("Season peak week", "Season onset")) %>%
    dplyr::mutate(bin_start_incl = 
                    suppressWarnings(as.numeric(bin_start_incl))) %>%
    stats::na.omit() %>%
    dplyr::pull(bin_start_incl) %>%
    max()
  
  d <- d %>%
#    filter(bin_start_incl != "none") %>%
          # Season onset has `none` as a possible bin_start_incl thus we
          # exclude it from the point forecast by turning bin_start_incl
          # into numeric 
    dplyr::mutate(bin_start_incl = 
                          suppressWarnings(as.numeric(bin_start_incl)),
                  bin_start_incl = ifelse(!(is.na(bin_start_incl)) & 
                                            unit == "week" & 
                                            bin_start_incl < 40,
                                          bin_start_incl + maxMMWR,
                                          bin_start_incl)) %>%
    dplyr::arrange(location, target, bin_start_incl)
  
	# Expected Value method
  if (method == "Expected Value") {
    temp <- d %>%
      stats::na.omit() %>% # Remove the NA for no onset to calculate mean
      dplyr::mutate(probability = value/sum(value),
                    value       = bin_start_incl) %>%
  	  dplyr::summarize(value = sum(value*probability)) %>%
  		dplyr::mutate(type = "Point",
  		              value = ifelse(target %in% c("Season onset", "Season peak week"),
  		                             round(value, 0),
  		                             round(value, 1)))
  }
  
  # Median method
  if (method == "Median") {
    temp <- d %>%
      dplyr::mutate(cumulative = cumsum(value),
                    type = "Point") %>%
      dplyr::filter(row_number() == min(which(cumulative >= 0.5))) %>%
      dplyr::select(location, target, value = bin_start_incl, type) 
  }
  
  # Mode method
  if (method == "Mode") {
    temp <- d %>%
      dplyr::filter(value == max(value)) %>%
      dplyr::select(location, target, value = bin_start_incl, type) %>%
      dplyr::mutate(type = "Point")
  }
  
	# Reset weeks back to MMWR format
	temp <- temp %>%
	          dplyr::mutate(value = ifelse(target %in% c("Season onset", "Season peak week") & 
	                                         value > maxMMWR & !(is.na(value)),
	                                       value - maxMMWR,
	                                       value))

	return(temp)
}
