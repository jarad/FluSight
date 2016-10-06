library(magrittr)


full_entry <- FluSight::read_entry("../inst/extdata/valid-test.csv") # no idea why this isn't working


# full_entry <- utils::read.csv("../inst/extdata/valid-test.csv",
#                               stringsAsFactors = FALSE) %>%


minimal_entry <- full_entry %>%
  dplyr::filter(location == "US National")


devtools::use_data(full_entry, overwrite=TRUE)
devtools::use_data(minimal_entry, overwrite=TRUE)
