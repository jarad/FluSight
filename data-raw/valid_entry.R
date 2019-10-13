library(magrittr)

full_entry_score <- FluSight::read_entry("inst/extdata/EW44_ValidTest_2016-11-07.csv") %>%
  FluSight::normalize_probs()# no idea why this isn't working

full_entry <- full_entry_score %>%
  dplyr::select(-forecast_week)

minimal_entry <- full_entry %>%
  dplyr::filter(location == "US National") 

valid_ILI <- read.csv("inst/extdata/valid_ILI.csv",
                      stringsAsFactors = FALSE) %>%
  dplyr::rename(ILI = wILI)

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

full_entry_hosp_score <- read.csv("inst/extdata/EW48_ValidHospTest_2017-12-04.csv",
                                  stringsAsFactors = FALSE) %>%
  setNames(tolower(names(.))) %>%
  dplyr::mutate(value = as.numeric(value),
                bin_start_incl = trimws(replace(bin_start_incl,
                                         !is.na(bin_start_incl) & bin_start_incl != "none",
                                         format(round(as.numeric(
                                           bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                           , 1), nsmall = 1))),
                bin_end_notincl = trimws(replace(bin_end_notincl,
                                          !is.na(bin_end_notincl) & bin_end_notincl != "none",
                                          format(round(as.numeric(
                                            bin_end_notincl[!is.na(bin_end_notincl) & bin_end_notincl != "none"]), 1),
                                            nsmall = 1))),
                forecast_week = 48) %>%
  FluSight::normalize_probs()

full_entry_hosp <- full_entry_hosp_score %>%
  dplyr::select(-forecast_week)


full_entry_state_score <- read.csv("inst/extdata/EW44_ValidStateTest_2017-11-07.csv",
                                  stringsAsFactors = FALSE) %>%
  setNames(tolower(names(.))) %>%
  dplyr::mutate(value = as.numeric(value),
                bin_start_incl = trimws(replace(bin_start_incl,
                                                !is.na(bin_start_incl) & bin_start_incl != "none",
                                                format(round(as.numeric(
                                                  bin_start_incl[!is.na(bin_start_incl) & bin_start_incl != "none"])
                                                  , 1), nsmall = 1))),
                bin_end_notincl = trimws(replace(bin_end_notincl,
                                                 !is.na(bin_end_notincl) & bin_end_notincl != "none",
                                                 format(round(as.numeric(
                                                   bin_end_notincl[!is.na(bin_end_notincl) & bin_end_notincl != "none"])
                                                   , 1), nsmall = 1))),
                forecast_week = 44) %>%
  FluSight::normalize_probs()

full_entry_state <- full_entry_state_score %>%
  dplyr::select(-forecast_week)


valid_observe_hosp <- read.csv("inst/extdata/valid_observe_hosp.csv", 
                               stringsAsFactors = FALSE)

valid_ILI_state <- read.csv("inst/extdata/valid_ILI_state.csv",
                            stringsAsFactors = FALSE)

state_truth_1617 <- read.csv("inst/extdata/state_truth_1617.csv",
                             stringsAsFactors = FALSE) %>%
  dplyr::mutate(bin_start_incl = trimws(replace(bin_start_incl,!is.na(bin_start_incl),
                                                format(round(as.numeric(
                                                  bin_start_incl[!is.na(bin_start_incl)])
                                                  , 1), nsmall = 1))))

valid_state_expand_1617 <- read.csv("inst/extdata/valid_state_expand_1617.csv",
                             stringsAsFactors = FALSE) %>%
  dplyr::mutate(bin_start_incl = trimws(replace(bin_start_incl,!is.na(bin_start_incl),
                                                format(round(as.numeric(
                                                  bin_start_incl[!is.na(bin_start_incl)])
                                                  , 1), nsmall = 1))))

hosp_truth_1617 <- read.csv("inst/extdata/hosp_truth_1617.csv",
                             stringsAsFactors = FALSE) %>%
  dplyr::mutate(bin_start_incl = trimws(replace(bin_start_incl,!is.na(bin_start_incl),
                                                format(round(as.numeric(
                                                  bin_start_incl[!is.na(bin_start_incl)])
                                                  , 1), nsmall = 1))))

valid_hosp_expand_1617 <- read.csv("inst/extdata/valid_hosp_expand_1617.csv",
                            stringsAsFactors = FALSE) %>%
  dplyr::mutate(bin_start_incl = trimws(replace(bin_start_incl,!is.na(bin_start_incl),
                                                format(round(as.numeric(
                                                  bin_start_incl[!is.na(bin_start_incl)])
                                                  , 1), nsmall = 1))))

past_baselines <- read.csv("inst/extdata/wILI_Baseline.csv",
                           stringsAsFactors = F)


usethis::use_data(full_entry, overwrite=TRUE)
usethis::use_data(minimal_entry, overwrite=TRUE)
usethis::use_data(full_entry_score, overwrite = TRUE)
usethis::use_data(valid_ILI, overwrite = TRUE)
usethis::use_data(truth_1516, overwrite = TRUE)
usethis::use_data(valid_exp_truth, overwrite = TRUE)
usethis::use_data(full_entry_hosp_score, overwrite = TRUE)
usethis::use_data(full_entry_hosp, overwrite = TRUE)
usethis::use_data(full_entry_state_score, overwrite = TRUE)
usethis::use_data(full_entry_state, overwrite = TRUE)
usethis::use_data(valid_observe_hosp, overwrite = TRUE)
usethis::use_data(valid_ILI_state, overwrite = TRUE)
usethis::use_data(state_truth_1617, overwrite = TRUE)
usethis::use_data(hosp_truth_1617, overwrite = TRUE)
usethis::use_data(valid_state_expand_1617, overwrite = TRUE)
usethis::use_data(valid_hosp_expand_1617, overwrite = TRUE)
usethis::use_data(past_baselines, overwrite = TRUE)
