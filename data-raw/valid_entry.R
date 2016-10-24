library(magrittr)


full_entry <- FluSight::read_entry("../inst/extdata/valid-test.csv") # no idea why this isn't working


# full_entry <- utils::read.csv("../inst/extdata/valid-test.csv",
#                               stringsAsFactors = FALSE) %>%


minimal_entry <- full_entry %>%
  dplyr::filter(location == "US National")

valid_ILI <- read.csv("../FluSight/inst/extdata/valid_ILI.csv",
                      stringsAsFactors = FALSE)

truth_1516 <- read.csv("../FluSight/inst/extdata/truth_1516.csv",
                       stringsAsFactors = FALSE)

valid_exp_truth <- read.csv("../FluSight/inst/extdata/valid_exp_truth.csv",
                            stringsAsFactors = FALSE)
valid_exp_truth$bin_start_incl <- as.character(valid_exp_truth$bin_start_incl)


devtools::use_data(full_entry, overwrite=TRUE)
devtools::use_data(minimal_entry, overwrite=TRUE)
devtools::use_data(valid_ILI, overwrite = TRUE)
devtools::use_data(truth_1516, overwrite = TRUE)
devtools::use_data(valid_exp_truth, overwrite = TRUE)
