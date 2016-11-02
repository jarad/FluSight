#' FluSight: a package for CDC Flu Competition functions
#'
#' The FluSight package provides functions to assist with creating, verifying, 
#' and scoring submissions to the CDC Influenza Forecasting Competition.
#' 
#' @section Creation functions:
#' The functions \code{\link{generate_point_forecasts}}, 
#' \code{\link{normalize_probs}}, and \code{\link{write_entry}} help create,
#' format, and label submission files.
#' 
#' @section Verification functions:
#' The functions \code{\link{verify_entry_file}} and \code{\link{verify_entry}}
#' verify that a submission is in the correct format and can be scored.
#' 
#' @section Scoring functions:
#' The functions \code{\link{create_truth}} and \code{\link{expand_truth}} 
#' create the observed truth that submissions will be scored against. The
#' function \code{\link{score_entry}} uses that observed truth to score an
#' entry
#'
#' @docType package
#' @name FluSight
NULL