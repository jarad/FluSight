#' Verify an entry file
#'
#' This function will check to make sure the structure is correct and that
#' the forecast probabilities are non-negative and sum to a value between
#' 0.9 and 1.1.
#'
#' @param file A csv entry file
#' @return Invisibly returns TRUE if successful
#' @export
#' @seealso \code{\link{verify_entry}}
#' @examples
#' file <- system.file("extdata", "valid-test.csv", package="FluSight")
#' verify_entry_file(file) # TRUE
verify_entry_file <- function(file) {
	entry <- read_entry(file)
  verify_entry(entry)
}



#' Verify entry stored as an R data.frame
#'
#' @param entry A data.frame
#' @return Invisibly returns TRUE if successful
#' @import dplyr
#' @export
#' @seealso \code{\link{verify_entry_file}}
#' @examples
#' verify_entry(minimal_entry)
#' verify_entry(full_entry)
verify_entry = function(entry) {

  verify_colnames(entry)

  # Verify column contents
  verify_locations(entry)
  verify_targets(entry)
  verify_types(entry)
  verify_units(entry)

  verify_bins(entry)
	verify_probabilities(entry)
	verify_point(entry)

	return(invisible(TRUE))
}

