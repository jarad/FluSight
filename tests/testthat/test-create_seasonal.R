context("create_seasonal")

test_that("Onset creation works", {
  rand_loc <- sample(valid_ILI$location, 1)

  tmp_onset <- create_onset(valid_ILI, rand_loc, 2015)
  tmp_truth <- truth_1516[truth_1516$location == rand_loc &
                            truth_1516$target == "Season onset", ]
  expect_equivalent(tmp_onset, tmp_truth)
  
})

test_that("Peak creation works for ILINet", {
  rand_loc <- sample(valid_ILI$location, 1)
  
  tmp_peak <- create_peak(valid_ILI, rand_loc)
  tmp_truth <- truth_1516[truth_1516$location == rand_loc &
                            truth_1516$target %in% 
                            c("Season peak week", "Season peak percentage"), ]
  expect_equivalent(tmp_peak, tmp_truth)
  
})

test_that("Peak creation works for states", {
  rand_loc <- sample(valid_ILI_state$location, 1)
  tmp_peak <- create_peak(valid_ILI_state, rand_loc, challenge = "state_ili")
  tmp_truth <- state_truth_1617[state_truth_1617$location == rand_loc &
                                  state_truth_1617$target %in%
                                  c("Season peak week", "Season peak percentage"), ]
  expect_equivalent(tmp_peak, tmp_truth)
})

test_that("Peak creation works for hospitalization", {
  rand_age <- sample(valid_observe_hosp$age_grp, 1)

  tmp_peak <- create_peak(valid_observe_hosp, rand_age, challenge = "hospital")
  tmp_truth <- hosp_truth_1617[hosp_truth_1617$age_grp == rand_age &
                                 hosp_truth_1617$target %in%
                            c("Season peak week", "Season peak rate"), ]
  expect_equivalent(tmp_peak, tmp_truth)

})