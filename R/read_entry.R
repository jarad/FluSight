#' Read in a csv entry file
#'
#' This function reads in the csv file and arranges it for consistency.
#'
#' @param file A csv file
#' @return An arranged data.frame
#' @import magrittr
#' @export
read_entry = function(file) {
	entry = read.csv(file,
					 colClasses = "character",      # Due to bin_start_incl "none"
					 stringsAsFactors = FALSE)

	names(entry) = tolower(names(entry))

	entry <- mutate(entry, value = as.numeric(value))

	entry %>% arrange_entry
}

#' Arrange an entry for consistency
#'
#' @param entry A data.frame
#' @return An arranged data.frame
#' @import dplyr
#' @export
arrange_entry = function(entry) {

	# Create vector of valid column names
	valid_columns <- c("location", "target", "type", "unit",
										 "bin_start_incl", "bin_end_notincl", "value")

	# Return error if column names don't match
	msg <- all.equal(names(entry),valid_columns)

	if (!isTRUE(msg)) {
		stop (c(paste("ERROR: Column name error,", msg,"\n"),
					"NOTE: Please take a look at the write_entry() function."))
	}

	# Arrange entry by type, location, target, bin
	entry %>%
		dplyr::arrange(type, location, target, bin_start_incl) %>%
		dplyr::select_("location", "target", "type", "unit",
									"bin_start_incl", "bin_end_notincl", "value")
}
