context("create_truth")

test_that("create_truth works", {
  
  tmp_fluview <- create_truth(fluview = TRUE, year = 2015)
  tmp_ILI <- create_truth(fluview = FALSE, weekILI = valid_ILI)
  
  expect_equivalent(tmp_fluview, truth_1516)
  expect_equivalent(tmp_ILI, truth_1516)
  
})

