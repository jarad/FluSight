# This essentially duplicates read_entry and then writes to an rda file.
library(dplyr)

d = read.csv("valid-test.csv", stringsAsFactors = TRUE)

names(d) = tolower(names(d))

# Organize the data.frame
forecast = d %>%
	arrange(type, location, target, bin_start_incl)


setClass("validEntry",
				 representation(teamName = "character",
				 							 week = "numeric",
				 							 forecast = "data.frame",
				 							 point_forecast = "data.frame"))

valid_entry = new("validEntry",
									teamName       = "validEntry",
									week           = 1,
									forecast       = forecast %>%	filter(type=="Bin"),
									point_forecast = forecast %>% filter(type=="Point"))

devtools::use_data(valid_entry)
