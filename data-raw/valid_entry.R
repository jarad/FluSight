library(magrittr)



full_entry_score <- FluSight::read_entry("inst/extdata/EW44_ValidTest_2016-11-07.csv") # no idea why this isn't working


# full_entry <- utils::read.csv("../inst/extdata/valid-test.csv",
#                               stringsAsFactors = FALSE) %>%

full_entry <- full_entry_score %>%
  dplyr::select(-forecast_week)

minimal_entry <- full_entry %>%
  dplyr::filter(location == "US National") 

valid_ILI <- read.csv("inst/extdata/valid_ILI.csv",
                      stringsAsFactors = FALSE)

truth_1516 <- read.csv("inst/extdata/truth_1516.csv",
                       stringsAsFactors = FALSE) %>%
  dplyr::mutate(bin_start_incl = trimws(replace(bin_start_incl,
                                                !is.na(bin_start_incl) & bin_start_incl != "none",
                                                format(round(as.numeric(
                                                  bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                                  , 1), nsmall = 1))))

valid_exp_truth <- read.csv("inst/extdata/valid_exp_truth.csv",
                            stringsAsFactors = FALSE) %>%
  dplyr::mutate(bin_start_incl = trimws(replace(bin_start_incl,
                                                !is.na(bin_start_incl) & bin_start_incl != "none",
                                                format(round(as.numeric(
                                                  bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                                  , 1), nsmall = 1))))


devtools::use_data(full_entry, overwrite=TRUE)
devtools::use_data(minimal_entry, overwrite=TRUE)
devtools::use_data(full_entry_score, overwrite = TRUE)
devtools::use_data(valid_ILI, overwrite = TRUE)
devtools::use_data(truth_1516, overwrite = TRUE)
devtools::use_data(valid_exp_truth, overwrite = TRUE)
