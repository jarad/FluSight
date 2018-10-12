context("verify_point")

test_that("Valid entry passes", {
  expect_true(verify_point(minimal_entry))
  expect_true(verify_point(full_entry))
  expect_true(verify_point(full_entry_hosp))
  expect_true(verify_point(full_entry_state))
})

test_that("Invalid challenge throws error", {
  expect_error(verify_point(minimal_entry, challenge = "hosp"))
})

test_that("Missing point predictions return warning", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_min <- minimal_entry
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "Point"] <- NA
  
  invalid_full <- full_entry
  invalid_full$value[invalid_full$location == rand_location & 
                       invalid_full$target == rand_target &
                       invalid_full$type == "Point"] <- NA
  
  expect_warning(verify_point(invalid_min))
  expect_warning(verify_point(invalid_full))
  
})

test_that("Negative point prediction return error", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_min <- minimal_entry
  invalid_min$value[invalid_min$target == rand_target &
                      invalid_min$type == "Point"] <- -1
  
  invalid_full <- full_entry
  invalid_full$value[invalid_full$location == rand_location & 
                       invalid_full$target == rand_target &
                       invalid_full$type == "Point"] <- -1
  
  expect_error(verify_point(invalid_min))
  expect_error(verify_point(invalid_full))

})