context("score_entry")

test_that("score_entry returns error if missing forecast_week", {
  expect_error(score_entry(full_entry, truth_1516))
})


test_that("Missing location scored as -10", {
  rand_location <- sample(unique(full_entry_score$location), 1)
  
  tmp <- full_entry_score[full_entry_score$location != rand_location, ]
  
  tmp_score <- score_entry(tmp, truth_1516)

  expect_equal(tmp_score$score[tmp_score$location == rand_location],
               rep(-10, length(tmp_score$score[tmp_score$location == rand_location])))  
  
})

test_that("Missing target scored as -10", {
  rand_target <- sample(unique(full_entry_score$target), 1)
  
  tmp <- full_entry_score[full_entry_score$target != rand_target, ]
  
  tmp_score <- score_entry(tmp, truth_1516)
  
  expect_equal(tmp_score$score[tmp_score$target == rand_target],
               rep(-10, length(tmp_score$score[tmp_score$target == rand_target])))  
  
})