context("Check columns of an entry")

valid_file  <- system.file("extdata", "valid-test.csv", package="FluSight")
valid_entry <- read_entry(valid_file)

test_that("Valid entry passes", {
	expect_true(check_columns(valid_entry))
})

test_that("Missing columns returns an error", {
  cnames <- names(valid_entry)
  for (i in seq_along(cnames)) {
    tmp_entry <- valid_entry[,-(names(valid_entry) %in% cnames[i])]
    expect_error(check_columns(tmp_entry))
  }
})

test_that("Extra column return a warning", {
  tmp_entry <- valid_entry
  tmp_entry$extra_column <- 1:nrow(valid_entry)
  expect_warning(check_columns(tmp_entry))
})
