context("create_week")


test_that("create_week works", {
  
  locations <- unique(valid_ILI$location)
  
  tmp_weeks <- create_week(valid_ILI, head(valid_ILI$week, n = 1), 
                           (tail(valid_ILI$week, n = 1) - 4))
  tmp_truth <- truth_1516[truth_1516$target %in% 
                            c("1 wk ahead", "2 wk ahead", 
                              "3 wk ahead", "4 wk ahead"), ]
  expect_equivalent(tmp_weeks, tmp_truth)
  
})

