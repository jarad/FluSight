context("Verify entry")

valid_file  <- system.file("extdata", "valid-test.csv", package="FluSight")
valid_entry <- read_entry(valid_file)

test_that("Valid entry passes", {
	expect_true(verify_entry_file(valid_file ))
	expect_true(verify_entry(     valid_entry))
})


test_that("Return error when column name doesn't exist", {
  for (i in seq_along(names(valid_entry))) {
  	invalid_entry <- valid_entry
  	names(invalid_entry)[i] <- "invalidName"
    expect_error(verify_entry(invalid_entry))
  }
})
