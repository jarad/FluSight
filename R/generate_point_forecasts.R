#' Generate point forecasts for all locations and targets
#'
#' @param entry An entry data.frame
#' @return A data.frame of point forecasts for all locations and targets.
#' @seealso \code{\link{generate_point_forecast}}, \code{\link{verify_entry}}
#' @import magrittr
#' @export
generate_point_forecasts <- function(entry) {
  if (sum(entry$type == "Point") > 0)
  	warning("It appears point forecasts already exist.")

  entry %>%
      dplyr::filter(type == "Bin") %>%
      dplyr::group_by(location,target) %>%
      generate_point_forecast(.)
}





#' Generate point forecasts from probabilistic forecasts
#'
#' The point forecast is taken to be the expected value of the
#' probabilistic forecasts.
#'
#' @param d A data.frame with columns `bin_start_incl` and `value`
#' @return A data.frame with columns `type` and `value`
#' @seealso generate_point_forecasts
#' @import magrittr
#' @export
#' @keywords internal
generate_point_forecast <- function(d) {
	temp <- d %>%
      		# Season onset has `none` as a possible bin_start_incl thus we
      		# exclude it from the point forecast by turning bin_start_incl
      		# into numeric and removing the none with na.omit
      		dplyr::mutate(probability = value/sum(value),
      									value       = suppressWarnings(as.numeric(bin_start_incl))) %>%
      		stats::na.omit() 
	
	# Add 52 to weeks in new year to keep in order
	temp$value[temp$unit == "week" & temp$value < 40] <- 
	  temp$value[temp$unit == "week" & temp$value < 40] + 52

	temp <- temp %>%
	  dplyr::summarize(value = sum(value*probability)) %>%
		dplyr::mutate(type = "Point")
	
	# Round off results to needed precision
	temp$value[temp$target %in% c("Season onset", "Season peak week")] <- 
	  round(temp$value[temp$target %in% c("Season onset", "Season peak week")], 0)
	temp$value[!(temp$target %in% c("Season onset", "Season peak week"))] <- 
	  round(temp$value[!(temp$target %in% c("Season onset", "Season peak week"))], 1)
	
	# Reset weeks back to MMWR format
	temp$value[temp$target %in% c("Season onset", "Season peak week") & 
	             temp$value > 52] <-
	  temp$value[temp$target %in% c("Season onset", "Season peak week") & 
	               temp$value > 52] - 52
	
	return(temp)
}
