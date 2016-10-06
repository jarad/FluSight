#' Verify an entry file
#'
#' This function will check to make sure the structure is correct and that
#' the forecast probabilities are non-negative and sum to a value between
#' 0.9 and 1.1.
#'
#' @param file A csv entry file
#' @return TRUE or a character vector of errors
#' @export
#' @seealso verify_entry
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
#' @return TRUE or a descriptive error message
#' @import dplyr
#' @export
#' @seealso verify_entry_file
#' @examples
#' file <- system.file("extdata", "valid-test.csv", package="FluSight")
#' entry <- read.csv(file, stringsAsFactors = FALSE)
#' verify_entry(entry) # TRUE
verify_entry = function(entry) {

  verify_colnames(entry)
	verify_probabilities(entry)
	verify_point(entry)

	# Return success message
	return(TRUE)

}



#' Verify the column names of an entry
verify_colnames <- function(entry) {


}




#' Verify the entry probabilities
#'
#' @param entry An entry data.frame
#' @import dplyr
#' @return NULL or a descriptive error message
verify_probabilities <- function(entry) {

	probabilities = entry %>%
		filter(type=="Bin") %>%
		group_by(location,target) %>%
		summarize(miss     = any(is.na(value)),
							sum      = sum(value, na.rm = TRUE),
							negative = any(!is.na(value) & value < 0))

	errors <- character()

	# Report message for missing probabilities
	if (any(probabilities$miss)) {
		tmp <- probabilities %>%
			filter(miss)

		errors <- c(errors, paste0("ERROR: Missing probabilities detected in ",
												paste(tmp$location, tmp$target), ".\n"))
	}

	# Report message for negative probabilities
	if (any(probabilities$negative)) {
		tmp <- probabilities %>%
			filter(negative)

		errors <- c(errors, paste0("ERROR: Negative probabilities detected in ",
								paste(tmp$location, tmp$target), ".\n"))
	}

	# Report message for sum of target probabilities outside of 0.9 and 1.1
	if (any(probabilities$sum < 0.9 | probabilities$sum > 1.1)) {
		tmp <- probabilities %>%
			filter(sum<0.9 | sum>1.1)

		errors <- c(errors, paste0("ERROR: In ", tmp$location, "-", tmp$target, ", probabilities sum to ",
								tmp$sum, ". \n"))
	}

	#Output probability related errors
	if (length(errors) != 0) {
		stop(errors)
	}

}


#' Verify validity of point predictions
#'
#' @param entry An entry data.frame
#' @import dplyr
#' @return NULL or a descriptive warning/error message
verify_point <- function(entry) {

	point = entry %>%
		filter(type == "Point") %>%
		mutate(miss     = is.na(value),
							negative = (!is.na(value) & value < 0))


	# Report warning for missing point predictions
	if (any(point$miss)) {
		tmp <- point %>%
			filter(miss)

		warning(paste0("WARNING: Missing point predictions detected in ",
									 paste(tmp$location, tmp$target), ". \n",
									 "Please take a look at the generate_point_forecasts function.\n"))
	}

	# Report error for missing point predictions
	if (any(point$negative)) {
		tmp <- point %>%
			filter(negative)

		stop(paste0("ERROR: Negative point predictions detected in ",
									 paste(tmp$location, tmp$target), ". \n",
									 "Please take a look at the generate_point_forecasts function.\n"))
	}

}



# # #Test files
# #  valid.test <- read.csv("data-raw/valid-test.csv")
# #  template <- read.csv("data-raw/template.csv")
# #  bad.test <- read.csv("data-raw/bad-test.csv")
#
# verify_entry <- function(this.sub, templ) {
# 	# Verifies that submissions to the CDC Flu Forecasting Challenge are formatted
# 	# correctly and that submission values are valid
# 	#
# 	# Args:
# 	# 	entry - data frame of team submission file read from csv
# 	# 	templ - data frame of template file read from csv
# 	#
# 	#	Returns:
# 	# 	sub - data frame of team submission verified and adjusted as needed
#
# 	names(this.sub) <- tolower(names(this.sub))
# 	names(templ)    <- tolower(names(templ))
#
# 	if (is.null(templ)) stop("Template is null.")
#
# 	### match row and column names
# 	if (any(row.mismatch <- rownames(this.sub) != rownames(templ))) {
# 		stop(paste0("Row names don't match: ",
# 				paste(which(row.mismatch), collapse=", ")))
# 	}
# 	if (any(col.mismatch <- colnames(this.sub) != colnames(templ))) {
# 		stop(paste0("Column names don't match: ",
# 				paste(which(col.mismatch), collapse=", ")))
# 	}
#
#   ### Check to make sure descriptive columns match.
#   for(i in 1:6){
#     if(!identical(this.sub[,i],templ[,i])){
#       stop(paste("Info in column",names(this.sub[i]),"does not match template"))
#     }
#   }
#
# 	### check for negative predictions
# 	# if a point prediction is negative, leave and report
# 	# if a probability prediction is negative, convert to zero
# 	neg.pp <- filter(this.sub, type=="Point" & value < 0) %>%
# 	  select(location, target) %>%
# 	  mutate_if(is.factor, as.character)
# 	if (nrow(neg.pp) != 0) {
# 	  warning(paste("!!!Row(s) with a negative point prediction: ",
# 	                 neg.pp$location, neg.pp$target,"\n",sep=" "), call.=F, immediate.=T)
# 	}
#
# 	neg.prob <- filter(this.sub, type=="Bin" & value <0) %>%
# 	  select(location,target,bin_start_incl) %>%
# 	  mutate_if(is.factor, as.character)
# 	if (nrow(neg.prob) !=0) {
# 	  for(i in 1:nrow(neg.prob)){
# 	  	this.sub$value[this.sub$value<0 & this.sub$location == neg.prob$location[i] & this.sub$target==neg.prob$target[i]] <- 0
# 	  }
# 	  warning(paste("!!!Row(s) with a negative probability prediction replaced with zeros: ",
# 	                neg.prob$location, neg.prob$target, neg.prob$bin_start_incl,"\n", sep=" "), call.=F, immediate.=T)
# 	}
#
# 	### check for NAs in the point predictions
# 	# if a point prediction is NA, leave as NA and report
# 	no.pp <- filter(this.sub, type=="Point" & is.na(value)) %>%
# 	  select(location, target) %>%
# 	  mutate_if(is.factor, as.character)
# 	if (nrow(no.pp) != 0) {
# 		warning(paste("Row(s) with no point prediction: ",
# 				no.pp$location, no.pp$target,"\n", sep=" "), call.=F, immediate.=T)
# 	}
#
# 	### check for NAs in the probabilities
# 	# if a whole target is NA, leave as NA and report
# 	# if only some entries are NAs, convert them to zeros
# 	# NOTE - if some negative entries converted to NA earlier, now set to 0
# 	na.in.probs <- filter(this.sub, type=="Bin" & is.na(value)) %>%
# 	  select(location, target) %>%
# 	  mutate_if(is.factor, as.character)
#
# 	if (nrow(na.in.probs) != 0) {
# 		num.na <- group_by(na.in.probs,location,target) %>% count()
# 		empty.probs <- num.na[(num.na$n == 131 |
# 		                         (num.na$n == 34 & num.na$target == "Season onset") |
# 		                         (num.na$n == 33 & num.na$target == "Season peak week")), ]
# 		partial.na <- setdiff(num.na,empty.probs)
#
#   	if (nrow(empty.probs) != 0) {
#   		warning(paste("Target(s) with no probability predictions: ",
#     			empty.probs$location, empty.probs$target,"\n", sep=" "), call.=F, immediate.=T)
#   	}
#
# 		if (nrow(partial.na) != 0) {
# 		  for(i in 1:nrow(partial.na)){
#   		  sub$value[is.na(this.sub$value) & this.sub$location == partial.na$location[i] & this.sub$target==partial.na$target[i]] <- 0
# 		  }
# 		  warning(paste(partial.na$n, "NAs replaced with zeros: ",
# 		                    partial.na$location, partial.na$target, "\n", sep=" "))
# 		}
# 	}
#
# 	### check probability column sums
# 	# if 1.0 < sum(probs) < 1.1 or 0.9 < sum(probs) < 1.0,
# 	# divide by sum to normalize to 1.0
# 	# if the sum is less than 0.9 or greater than 1.1, convert all to NAs and report
# 	prob.sums <- filter(this.sub,type=="Bin") %>%
# 	                group_by(location,target) %>%
# 	                summarize(sum=sum(value)) %>%
# 	                filter(round(sum,6) != 1)  #Round off sum to 6 decimal places to deal with floating point math
#
# 	within.one <- filter(prob.sums, sum < 1.1 & sum > 0.9)
# 	below.one <- filter(prob.sums, sum <= 0.9)
# 	above.one <- filter(prob.sums, sum >= 1.1)
#
#   for(i in 1:nrow(within.one)){
#     #Normalize predictions between 0.9 and 1.1
#   	this.sub$value[this.sub$location == within.one$location[i] & this.sub$target == within.one$target[i] & this.sub$type == "Bin"] <-
#   		this.sub$value[this.sub$location == within.one$location[i] & this.sub$target == within.one$target[i] & this.sub$type == "Bin"]/prob.sums$sum[i]
#   }
# 	writeLines(paste(nrow(within.one), "prediction(s) with probability predictions normalized to 1.0."))
#
# 	for(i in 1:nrow(below.one)){
# 		this.sub$value[this.sub$location == below.one$location[i] & this.sub$target == below.one$target[i] & this.sub$type == "Bin"] <- NA
# 	}
# 	if(nrow(below.one)>0) warning(paste("Target(s) with probability predictions summing to < 0.9 set to NA: ",
# 	              below.one$location, below.one$target,"\n", sep=" "), call.=F, immediate.=T)
#
# 	for(i in 1:nrow(above.one)){
# 		this.sub$value[this.sub$location == above.one$location[i] & this.sub$target == above.one$target[i] & this.sub$type == "Bin"] <- NA
# 	}
# 	if(nrow(above.one)>0) warning(paste("Target(s) with probability predictions summing to >1.1 set to NA: ",
# 	                                    above.one$location, above.one$target,"\n", sep=" "), call.=F, immediate.=T)
#
#
# 	writeLines("Verification complete.\n")
# 	return(this.sub)
# }

