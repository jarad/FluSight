#' A function to read in a csv entry file
#'
#' This function reads in the csv file and arranges it for consistency.
#'
#' @param file A csv file
#' @return An arranged data.frame
#' @import dplyr
read_entry = function(file) {
	read.csv(file, stringsAsFactors = FALSE) %>% arrange_entry
}

#' A function to arrange an entry for consistency
#'
#' @param entry A data.frame
#' @return An arranged data.frame
#' @import dplyr
arrange_entry = function(entry) {
	entry %>%	arrange(type, location, target, bin_start_incl)
}
