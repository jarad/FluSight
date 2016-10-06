#' A full valid entry
#'
#' A data.frame containing a valid entry
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


#' A minimal valid entry
#'
#' A data.frame containing a valid entry
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
