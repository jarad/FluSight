#' Generate point forecasts for all locations and targets
#'
#' @param entry An entry data.frame
#' @param method The method to be used to generate the point forecasts. 
#'   \code{"Median"} (the default) uses the median value, \code{"Expected
#'   Value"} generates the expected value from the provided probabilities,and
#'   \code{"Mode"} returns the individual bin with the largest probability
#' @return A data.frame of point forecasts for all locations and targets.
#' @seealso \code{\link{generate_point_forecast}}, \code{\link{verify_entry}}
#' @import magrittr
#' @export
generate_point_forecasts <- function(entry, method = 
                                       c("Median", "Expected Value", "Mode")) {
  
  method <- match.arg(method)
  
  if (sum(entry$type == "Point") > 0)
  	warning("It appears point forecasts already exist.")

  d<-entry %>%
      dplyr::filter(type == "Bin") %>%
      dplyr::group_by(location,target) %>%
      generate_point_forecast(., method)
}


#' Generate point forecasts from probabilistic forecasts
#'
#' The point forecast is taken to be either the expected value, median, 
#' or mode of the
#' probabilistic forecasts.
#'
#' @param d A data.frame with columns `bin_start_incl` and `value`
#' @param method The method to be used to generate the point forecasts. 
#'   \code{"Median"} (the default) uses the median value, \code{"Expected
#'   Value"} generates the expected value from the provided probabilities,and
#'   \code{"Mode"} returns the individual bin with the largest probability 
#' @return A data.frame with columns `type` and `value`
#' @seealso generate_point_forecasts
#' @import magrittr
#' @export
#' @keywords internal
generate_point_forecast <- function(d, method = 
                                     c("Median", "Expected Value", "Mode")) {
  method <- match.arg(method)
  
  d <- d %>%
          # Season onset has `none` as a possible bin_start_incl thus we
          # exclude it from the point forecast by turning bin_start_incl
          # into numeric 
    dplyr::mutate(bin_start_incl = 
                          suppressWarnings(as.numeric(bin_start_incl)))
  
	# Add 52 to weeks in new year to keep in order
	d$bin_start_incl[!(is.na(d$bin_start_incl)) & d$unit == "week" & 
	             d$bin_start_incl < 40] <- 
	  d$bin_start_incl[!(is.na(d$bin_start_incl)) & d$unit == "week" & 
	               d$bin_start_incl < 40] + 52
	
	d <- dplyr::arrange(d, location, target, bin_start_incl)

	# Expected Value method
  if (method == "Expected Value") {
    temp <- d %>%
      stats::na.omit() %>% # Remove the NA for no onset to calculate mean
      dplyr::mutate(probability = value/sum(value),
                    value       = bin_start_incl) %>%
  	  dplyr::summarize(value = sum(value*probability)) %>%
  		dplyr::mutate(type = "Point")
  	
  	# Round off results to needed precision
  	temp$value[temp$target %in% c("Season onset", "Season peak week")] <- 
  	  round(temp$value[temp$target %in% c("Season onset", "Season peak week")], 0)
  	temp$value[!(temp$target %in% c("Season onset", "Season peak week"))] <- 
  	  round(temp$value[!(temp$target %in% c("Season onset", "Season peak week"))], 1)
  }
  
  # Median method
  if (method == "Median") {
    temp <- d %>%
      dplyr::arrange(bin_start_incl) %>%
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
	temp$value[temp$target %in% c("Season onset", "Season peak week") & 
	             temp$value > 52 & !(is.na(temp$value))] <-
	  temp$value[temp$target %in% c("Season onset", "Season peak week") & 
	               temp$value > 52 & !(is.na(temp$value))] - 52
	
	return(temp)
}
