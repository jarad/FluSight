#' A full valid entry for national ILI forecasting
#'
#' A data.frame containing a valid entry for national ILI forecasting
#'
#' @format A dta frame with 8019 rows and 7 variables:
#' \describe{
#'   \item{location}{character vector of locations}
#'   \item{target}{character vector of targets}
#'   \item{type}{character vector with elements "Bin" or "Point"}
#'   \item{unit}{character vector with elements "percent" or "week"}
#'   \item{bin_start_incl}{character vector with forecast bin start value (inclusive)}
#'   \item{bin_end_notincl}{character vector the forecast bin end value}
#'   \item{value}{numeric vector with probabilities or point forecasts}
#' }
"full_entry"


#' A minimal valid entry for national ILI forecasting
#'
#' A data.frame containing a valid entry for national ILI forecasting
#'
#' @format A dta frame with 729 rows and 7 variables:
#' \describe{
#'   \item{location}{character vector of locations}
#'   \item{target}{character vector of targets}
#'   \item{type}{character vector with elements "Bin" or "Point"}
#'   \item{unit}{character vector with elements "percent" or "week"}
#'   \item{bin_start_incl}{character vector with forecast bin start value (inclusive)}
#'   \item{bin_end_notincl}{character vector the forecast bin end value}
#'   \item{value}{numeric vector with probabilities or point forecasts}
#' }
"minimal_entry"


#' A valid ILINet submission
#'
#' A data.frame containing a valid submission of national ILINet data
#'
#' @format A dta frame with 363 rows and 3 variables:
#' \describe{
#'   \item{location}{character vector of locations}
#'   \item{week}{numeric vector of MMWR weeks}
#'   \item{wILI}{numeric vector of weighted ILI percentage}
#' }
"valid_ILI"


#' Observed national ILINet truth for 2015/2016 influenza season
#'
#' A data.frame containing the observed national ILI forecasting truth for the
#' 2015/2016 influenza season
#'
#' @format A dta frame with 1310 rows and 4 variables:
#' \describe{
#'   \item{target}{character vector of targets}
#'   \item{location}{character vector of locations}
#'   \item{forecast_week}{numeric vector of MMWR week forecasted value is based on}
#'   \item{bin_start_incl}{numeric vector of observed truth}
#' }
"truth_1516"

#' Expanded national ILINet truth for 2015/2016 influenza season
#'
#' A data.frame containing the observed national ILI forecasting truth for the 
#' 2015/2016 influenza season, expanded for one bin around week targets and 
#' five bins around percent targets
#'
#' @format A dta frame with 14212 rows and 4 variables:
#' \describe{
#'   \item{target}{character vector of targets}
#'   \item{location}{character vector of locations}
#'   \item{forecast_week}{numeric vector of MMWR week forecasted value is based on}
#'   \item{bin_start_incl}{numeric vector of observed truth}
#' }
"valid_exp_truth"

#' A full valid national ILINet entry containing forecast week for scoring
#'
#' A data.frame containing a scorable valid entry for national ILI forecasting
#'
#' @format A dta frame with 8019 rows and 8 variables:
#' \describe{
#'   \item{location}{character vector of locations}
#'   \item{target}{character vector of targets}
#'   \item{type}{character vector with elements "Bin" or "Point"}
#'   \item{unit}{character vector with elements "percent" or "week"}
#'   \item{bin_start_incl}{character vector with forecast bin start value (inclusive)}
#'   \item{bin_end_notincl}{character vector the forecast bin end value}
#'   \item{value}{numeric vector with probabilities or point forecasts}
#'   \item{forecast_week}{numeric vector of MMWR week forecasted value is based on}
#' }
"full_entry_score"

#' A full valid hospitalization forecast entry containing forecast week for 
#' scoring
#'
#' A data.frame containing a scorable valid entry for hospitalization 
#' forecasting
#'
#' @format A dta frame with 6466 rows and 8 variables:
#' \describe{
#'   \item{age_grp}{character vector of age group}
#'   \item{target}{character vector of targets}
#'   \item{type}{character vector with elements "Bin" or "Point"}
#'   \item{unit}{character vector with elements "percent" or "week"}
#'   \item{bin_start_incl}{character vector with forecast bin start value (inclusive)}
#'   \item{bin_end_notincl}{character vector the forecast bin end value}
#'   \item{value}{numeric vector with probabilities or point forecasts}
#'   \item{forecast_week}{numeric vector of MMWR week forecasted value is based on}
#' }
"full_entry_hosp_score"

#' A full valid entry for hospitalization forecasting
#'
#' A data.frame containing a valid entry for hospitalization forecasting
#'
#' @format A dta frame with 6466 rows and 7 variables:
#' \describe{
#'   \item{age_grp}{character vector of age group}
#'   \item{target}{character vector of targets}
#'   \item{type}{character vector with elements "Bin" or "Point"}
#'   \item{unit}{character vector with elements "percent" or "week"}
#'   \item{bin_start_incl}{character vector with forecast bin start value (inclusive)}
#'   \item{bin_end_notincl}{character vector the forecast bin end value}
#'   \item{value}{numeric vector with probabilities or point forecasts}
#' }
"full_entry_hosp"
