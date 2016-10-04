# valid_entry <- FluSight::read_entry("../inst/extdata/valid-test.csv") # no idea why this isn't working

valid_entry <- utils::read.csv("../inst/extdata/valid-test.csv",
                        stringsAsFactors = FALSE)

devtools::use_data(valid_entry, overwrite=TRUE)
