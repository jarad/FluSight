#' Verifies a csv submission file
#'
#' This function will run through a variety of tests to check a submission file.
#' It will return TRUE if the entry is valid and FALSE if not. In both cases, it might
#' print warnings.
#'
#' @param object An Entry object
#' @return TRUE if the Entry is compatible
#' @examples
#' entry <- read_entry("extdata", "valid-test.csv", package="FluSight")
#' verify_entry(entry)
verify_entry <- function(object) {
	errors <- character()

  errors <- c(errors, verify_forecast(object@forecast, valid_entry@forecast))

#	errors <- c(errors, verify_colnames(object))


	if (length(errors) == 0) TRUE else errors
}



#' An S4 class to represent an entry into the FluSight competition
#'
#' @slot teamName A character vector indicating the teamname
#' @slot week A scalar numeric indicating the submission week
#' @slot forecast A data.frame of probabilistic forecasts
#' @slot point_forecast A data.frame of point forecasts
setClass("Entry",
	representation(teamName       = "character",
							 week           = "numeric",
							 forecast       = "data.frame",
							 point_forecast = "data.frame"),

	prototype(teamName = NA_character_,
						week = NA_real_,
						forecast = data.frame(location=character(),
																	target = character(),
																	unit = character(),
																	bin_start_incl = numeric(),
																	bin_end_notincl = numeric(),
																	value = numeric()
																	),
						point_forecast = data.frame(location = character(),
																				target = character(),
																				unit = character(),
																				value = numeric())),
	validity = verify_entry
)

Entry <- function(teamName, week, forecast, point_forecasts = NULL) {
	if (missing(teamName)) stop("Need a team name.")
	if (missing(week))     stop("Need a prediction week.")
	if (missing(forecast)) stop("Need forecast.")

	if (missing(point_forecast))
		point_forecast = generate_point_forecast(forecast)

	new("Entry", teamName = teamName, week = week, forecast = forecast, point_forecast = point_forecast)
}







####################################################################################
# The functions below all take a data.frame as input and
# return NULL or a character string of errors.
####################################################################################
#' import dplyr
verify_forecast <- function(forecast, valid_forecast) {
	errors <- character()

	# Check structure
	msg <- all.equal(forecast       %>% select(-value),
									 valid_forecast %>% select(-value))
	if (!isTRUE(msg))
		errors <- c(errors, msg)

# 	# Check probabilities
# 	probabilities = forecast %>%
# 		group_by(location,target) %>%
# 		summarize(sum      = sum(value),
# 							negative = any(value<0))
#
# 	# Report message for negative probabilities
# 	if (any(probabilities$negative)) {
# 		tmp <- probabilities %>%
# 			filter(negative)
#
# 		msg <- paste0("Negative probabilities detected in ", paste(tmp$location, tmp$target), ".")
# 	}
#   errors <- c(errors, msg)
#
#   # Report message for probabilities that do not sum to 1
# 	if (any(probabilities$sum!=1)) {
# 		tmp <- probabilities %>%
# 			filter(sum!=1)
#
# 		msg <- paste0("Probabilities do not sum to 1 in ", paste(tmp$location, tmp$target), ".")
# 	}
#   errors <- c(errors, msg)

  if(length(errors) == 0) TRUE else errors
}



verify_colnames <- function(entry) {
	correct_colnames <- c("location",
												"target",
												"type",
												"unit",
												"bin_start_incl",
												"bin_end_notincl",
												"value")

	entry_colnames <- colnames(entry)

	# Check for extra column names
	extra_colnames <- setdiff(entry_colnames, correct_colnames)
	if (length(extra_colnames)>0)
		warning("Extra column name(s) found: ",paste0(extra_colnames,sep=" "), call.=FALSE)

	# Check to make sure all necessary column names are included
	if (all(correct_colnames %in% entry_colnames)) {
		return(NULL)
	} else {
		# If necessary columns are missing, then determine which ones are missing.
		extra_colnames <- setdiff(correct_colnames, entry_colnames)
		return(paste0("These column(s) are needed in the entry file: ", paste0(extra_colnames,collapse=" ")))
	}
}


