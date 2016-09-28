#' An S4 class to represent an entry into the FluSight competition
#'
#' @slot teamName A character vector indicating the teamname
#' @slot week A scalar numeric indicating the submission week
#' @slot forecast A data.frame of probabilistic forecasts
#' @slot point_forecast A data.frame of point forecasts
setClass("Entry",
	slots = list(teamName       = "character",
							 week           = "numeric",
							 forecast       = "data.frame",
							 point_forecast = "data.frame")
)

Entry <- function(teamName, week, forecast, point_forecasts = NULL) {
	if (missing(teamName)) stop("Need a team name.")
	if (missing(week))     stop("Need a prediction week.")
	if (missing(forecast)) stop("Need forecast.")

	if (missing(point_forecast))
		point_forecast = generate_point_forecast(forecast)

	new("Entry", teamName = teamName, week = week, forecast = forecast, point_forecast = point_forecast)
}

