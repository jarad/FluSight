context("verify_point")

test_that("Valid entry passes", {
  expect_null(verify_point(full_entry))
})

test_that("Missing point predictions return warning", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_entry <- full_entry
  invalid_entry$value[invalid_entry$location == rand_location & 
                        invalid_entry$target == rand_target &
                        invalid_entry$type == "Point"] <- NA
  
  expect_warning(verify_point(invalid_entry))
  
})

test_that("Negative point prediction return error", {
  rand_target <- sample(unique(minimal_entry$target), 1)
  rand_location <- sample(unique(full_entry$location), 1)
  
  invalid_entry <- full_entry
  invalid_entry$value[invalid_entry$location == rand_location & 
                        invalid_entry$target == rand_target &
                        invalid_entry$type == "Point"] <- -1
  
  expect_error(verify_point(invalid_entry))

})