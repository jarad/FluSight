context("verify_ILI")


test_that("Valid entry is successful", {
  expect_null(verify_ILI(valid_ILI))
  expect_null(verify_ILI_colnames(valid_ILI))
  expect_null(verify_ILI_location(valid_ILI))
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
  valid_locations <- unique(valid_ILI$location)
  
  for (i in seq_along(valid_locations)) {
    tmp_ILI <- valid_ILI[valid_ILI$location != valid_locations[i], ]
    expect_warning(verify_ILI(tmp_ILI))
    expect_warning(verify_ILI_location(tmp_ILI))
  }
})

test_that("Extra locations throw warnings", {
  tmp_ILI <- valid_ILI
  tmp_ILI$location <- as.character(tmp_ILI$location)
  tmp_ILI$location[1] <- "extra"
  expect_error(verify_ILI(tmp_ILI))
  expect_error(verify_ILI_location(tmp_ILI))
})