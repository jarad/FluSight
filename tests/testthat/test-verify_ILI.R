context("verify_ILI")


test_that("Valid entry is successful", {
  expect_null(verify_ILI(valid_ILI))
  expect_null(verify_ILI_colnames(valid_ILI))
  expect_null(verify_ILI_location(valid_ILI))
  expect_null(verify_ILI_colnames(valid_ILI_state))
  expect_null(verify_ILI_location(valid_ILI_state, challenge = "state_ili"))
})

test_that("Missing columns throw errors", {
  for (i in seq_along(colnames(valid_ILI))) {
    tmp_ILI <- valid_ILI[,-i]
    expect_error(verify_ILI(tmp_ILI))
    expect_error(verify_ILI_colnames(tmp_ILI))
  }
})

test_that("Extra columns throw warnings", {
  tmp_ILI <- valid_ILI
  tmp_ILI$extra <- NA
  expect_warning(verify_ILI(tmp_ILI))
  expect_warning(verify_ILI_colnames(tmp_ILI))
})

test_that("Missing locations throw warnings", {
  rand_loc <- sample(unique(valid_ILI$location), 1)
  tmp_ILI <- valid_ILI[valid_ILI$location != rand_loc, ]
  expect_warning(verify_ILI(tmp_ILI))
  expect_warning(verify_ILI_location(tmp_ILI))
  
  
  rand_loc <- sample(unique(valid_ILI_state$location), 1)
  tmp_ILI <- valid_ILI_state[valid_ILI_state$location != rand_loc, ]
  expect_warning(verify_ILI(tmp_ILI, challenge = "state_ili"))
  expect_warning(verify_ILI_location(tmp_ILI, challenge = "state_ili"))
})

test_that("Extra locations throw errors", {
  tmp_ILI <- valid_ILI
  tmp_ILI$location <- as.character(tmp_ILI$location)
  tmp_ILI$location[1] <- "extra"
  expect_error(verify_ILI(tmp_ILI))
  expect_error(verify_ILI_location(tmp_ILI))
  
  tmp_ILI <- valid_ILI_state
  tmp_ILI$location <- as.character(tmp_ILI$location)
  tmp_ILI$location[1] <- "extra"
  expect_error(verify_ILI(tmp_ILI, challenge = "state_ili"))
  expect_error(verify_ILI_location(tmp_ILI, challenge = "state_ili"))
  expect_error(verify_ILI())
})