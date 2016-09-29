#' A function to read in an Entry from a csv file
#'
#' @param file A csv file
#' @return An Entry
#' @import dplyr
read_entry = function(file) {
	# Expected format of the file is teamName-week.csv

  # Need to extract teamName from filename
	teamName = "defaultName"

	# Need to extract week from filename
	week = 1

	tmp = read.csv(file, stringsAsFactors = FALSE)
	names(tmp) = tolower(names(tmp))

	# Organize the file
	forecast = tmp %>%
		arrange(type, location, target, bin_start_incl)

	# Check for point forecasts
  if (any(forecast$type == "Point")) {
    point_forecast = forecast %>%
    	filter(type == "Point") %>%
    	dplyr::select(-type, -bin_start_incl, - bin_end_notincl)

    forecast = forecast %>%
    	filter(type == "Bin") %>%
    	dplyr::select(-type)

    new("Entry", teamName = teamName, week = week, forecast = forecast, point_forecast = point_forecast)
  } else {
  	new("Entry", teamName = teamName, week = week, forecast = forecast) # point forecasts are automatically generated
  }
}
