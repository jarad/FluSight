#' Read in a csv entry file
#'
#' This function reads in the csv file and arranges it for consistency.
#'
#' @param file A csv file
#' @return An arranged data.frame
#' @import dplyr
#' @export
read_entry = function(file) {
	read.csv(file,
					 colClasses = "character",      # Due to bin_start_incl "none"
					 stringsAsFactors = FALSE) %>%
		mutate(value = as.numeric(value)) %>%
		arrange_entry
}

#' Arrange an entry for consistency
#'
#' @param entry A data.frame
#' @return An arranged data.frame
#' @import dplyr
#' @export
arrange_entry = function(entry) {
	entry %>%	dplyr::arrange(type, location, target, bin_start_incl)
}
