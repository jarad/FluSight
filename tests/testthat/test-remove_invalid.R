context("remove_invalid")

test_that("Ignores valid entry.",{
  expect_equivalent(full_entry, remove_invalid(full_entry))
  expect_equivalent(full_entry_score, remove_invalid(full_entry_score))
})

test_that("Invalid high probs set to zero.", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_min <- minimal_entry
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "Bin"] <- 1
  invalid_full <- full_entry
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "Bin"] <- 1
  
  invalid_min <- remove_invalid(invalid_min)
  invalid_full <- remove_invalid(invalid_full)
  
  expect_true(is.na(sum(invalid_min$value[invalid_min$target == rand_target &
                                            invalid_min$type == "Bin"])))
  expect_true(is.na(sum(invalid_full$value[invalid_full$location == rand_location &
                                                invalid_full$target == rand_target &
                                                invalid_full$type == "Bin"])))
  
})

test_that("Invalid high probs set to zero.", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_min <- minimal_entry
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "Bin"] <- 0.001
  invalid_full <- full_entry
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "Bin"] <- 0.001
  
  invalid_min <- remove_invalid(invalid_min)
  invalid_full <- remove_invalid(invalid_full)
  
  expect_true(is.na(sum(invalid_min$value[invalid_min$target == rand_target &
                                               invalid_min$type == "Bin"])))
  expect_true(is.na(sum(invalid_full$value[invalid_full$location == rand_location &
                                                invalid_full$target == rand_target &
                                                invalid_full$type == "Bin"])))
  
})

