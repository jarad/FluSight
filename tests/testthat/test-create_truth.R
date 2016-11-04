context("create_truth")

test_that("create_truth works", {
  
  # Removed first test as backfilled data means it no longer matches saved file
    # tmp_fluview <- create_truth(fluview = TRUE, year = 2015)
    # expect_equivalent(tmp_fluview, truth_1516)
  
  tmp_ILI <- create_truth(fluview = FALSE, weekILI = valid_ILI, year = 2015)
  expect_equivalent(tmp_ILI, truth_1516)
  
})

