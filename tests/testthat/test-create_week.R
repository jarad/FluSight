context("create_week")


test_that("create_week works", {

  expect_equivalent(create_week(valid_ILI, head(valid_ILI$week, n = 1), 
                                (tail(valid_ILI$week, n = 1) - 4)),
                    truth_1516[truth_1516$target %in% 
                                 c("1 wk ahead", "2 wk ahead", 
                                   "3 wk ahead", "4 wk ahead"), ])
  
  expect_equivalent(create_week(valid_ILI_state, 43, 18,
                                challenge = "state_ili"),
                    state_truth_1617[state_truth_1617$target %in% 
                                       c("1 wk ahead", "2 wk ahead", 
                                         "3 wk ahead", "4 wk ahead"), ])
  expect_equivalent(create_week(valid_observe_hosp, 45, 17,
                                challenge = "hospital"),
                    hosp_truth_1617[hosp_truth_1617$target %in% 
                                       c("1 wk ahead", "2 wk ahead", 
                                         "3 wk ahead", "4 wk ahead"), ])
})

