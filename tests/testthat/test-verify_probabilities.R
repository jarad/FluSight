context("verify_probabilities")


test_that("Valid entry passes", {
  expect_true(verify_probabilities(minimal_entry))
  expect_true(verify_probabilities(full_entry))
  expect_true(verify_probabilities(full_entry_hosp, challenge = "hospital"))
  expect_true(verify_probabilities(full_entry_state, challenge = "state_ili"))
})

test_that("Invalid challenge throws errors", {
  expect_error(verify_probabilities(minimal_entry, challenge = "hosp"))
})

test_that("Missing probabilities throw errors", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_min <- minimal_entry
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "Bin"] <- NA
  invalid_full <- full_entry
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "Bin"] <- NA
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})

test_that("Negative probabilities throw errors", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_min <- minimal_entry
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "Bin"] <- -0.1
  invalid_full <- full_entry
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "Bin"] <- -0.1
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})

test_that("Probabilities summing to < 0.9 throw errors", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_min <- minimal_entry
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "Bin"] <- 0.01
  invalid_full <- full_entry
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "Bin"] <- 0.01
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})

test_that("Probabilities summing to > 1.1 throw errors", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_min <- minimal_entry
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "Bin"] <- 0.1
  invalid_full <- full_entry
  invalid_full$value[invalid_full$location == rand_location &
                       invalid_full$target == rand_target &
                       invalid_full$type == "Bin"] <- 0.1
  
  expect_error(verify_probabilities(invalid_min))
  expect_error(verify_probabilities(invalid_full))
  
})