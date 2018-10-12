context("verify_hosp")


test_that("Valid entry is successful", {
  expect_null(verify_hosp(valid_observe_hosp))
})

test_that("Missing columns throw errors", {
  rand_col <- sample(names(valid_observe_hosp), 1)
  tmp_hosp <- valid_observe_hosp[,-which(names(valid_observe_hosp) == "randcol")]
  expect_error(verify_hosp(tmp_hosp))
})

test_that("Extra columns throw warnings", {
  tmp_ILI <- valid_observe_hosp
  tmp_ILI$extra <- NA
  expect_warning(verify_hosp(tmp_ILI))
})

test_that("Missing age groups throw warnings", {
  rand_age <- sample(unique(valid_observe_hosp$location), 1)
  tmp_ILI <- valid_observe_hosp[valid_observe_hosp$location != rand_age, ]
  expect_warning(verify_hosp(tmp_ILI))
})

test_that("Extra age group throw errors", {
  tmp_ILI <- valid_observe_hosp
  tmp_ILI$location[1] <- "extra"
  expect_error(verify_hosp(tmp_ILI))
})