context("verify_entry")

valid_file_week  <- system.file("inst/extdata", "EW44_ValidTest_2016-11-07.csv", package="FluSight")
valid_entry_week <- read_entry(valid_file_week)
valid_file <- system.file("inst/extdata", "valid_test.csv", package="FluSight")
valid_entry <- read_entry(valid_file)
valid_hosp_file <- system.file("inst/extdata", "EW44_ValidHospTest_2017-11-07.csv", package="FluSight")
valid_hosp_entry <- read_entry(valid_hosp_file, challenge = "hospital")

test_that("Valid entry passes", {
	expect_true(verify_entry_file(valid_file ))
	expect_true(verify_entry(     valid_entry))
	expect_true(verify_entry_file(valid_file_week))
	expect_true(verify_entry(valid_entry_week))
	expect_true(verify_entry(      full_entry))
	expect_true(verify_entry(  minimal_entry))
	expect_true(verify_entry_file(valid_hosp_file, challenge = "hospital"))
	expect_true(verify_entry(valid_hosp_entry, challenge = "hospital"))
	expect_true(verify_entry(full_entry_hosp, challenge = "hospital"))
})

# test_that("Entry without forecast week generates warnings", {
#   expect_warning(verify_entry_file(valid_file))
#   expect_warning(verify_entry(valid_entry))
# })


test_that("Return error when required column name doesn't exist", {
  for (i in seq_along(names(valid_entry))) {
  	invalid_entry <- valid_entry
  	names(invalid_entry)[i] <- "invalidName"
    expect_error(arrange_entry(invalid_entry))
    expect_error(verify_entry( invalid_entry))
  }
})


test_that("Return error when probabilities are missing", {
	for (this_location in unique(valid_entry$location)) {
		for (this_target in unique(valid_entry$target)) {
			invalid_entry <- valid_entry
			invalid_entry$value[invalid_entry$location == this_location &
														invalid_entry$target == this_target &
														invalid_entry$type == "Bin"] <- NA
			expect_error(verify_probabilities(invalid_entry))
			expect_error(verify_entry(        invalid_entry))
		}
	}
})


test_that("Return error when probabilities are negative", {
	for (this_location in unique(valid_entry$location)) {
		for (this_target in unique(valid_entry$target)) {
			invalid_entry <- valid_entry
			invalid_entry$value[invalid_entry$location == this_location &
														invalid_entry$target == this_target &
														invalid_entry$type == "Bin"] <- -0.5
			expect_error(verify_probabilities(invalid_entry))
			expect_error(verify_entry(        invalid_entry))
		}
	}
})


test_that("Return error when probabilities sum to less than 0.9", {
	for (this_location in unique(valid_entry$location)) {
		for (this_target in unique(valid_entry$target)) {
			invalid_entry <- valid_entry
			invalid_entry$value[invalid_entry$location == this_location &
														invalid_entry$target == this_target &
														invalid_entry$type == "Bin"] <- 0.01
			expect_error(verify_probabilities(invalid_entry))
			expect_error(verify_entry(        invalid_entry))
		}
	}
})


test_that("Return error when probabilities sum to more than 1.1", {
	for (this_location in unique(valid_entry$location)) {
		for (this_target in unique(valid_entry$target)) {
			invalid_entry <- valid_entry
			invalid_entry$value[invalid_entry$location == this_location &
														invalid_entry$target == this_target &
														invalid_entry$type == "Bin"] <- 0.1
			expect_error(verify_probabilities(invalid_entry))
			expect_error(verify_entry(        invalid_entry))
		}
	}
})

test_that("Return warning when point forecast is missing", {
	for (this_location in unique(valid_entry$location)) {
		for (this_target in unique(valid_entry$target)) {
			invalid_entry <- valid_entry
			invalid_entry$value[invalid_entry$location == this_location &
														invalid_entry$target == this_target &
														invalid_entry$type == "Point"] <- NA
			expect_warning(verify_point(invalid_entry))
			expect_warning(verify_entry(invalid_entry))
		}
	}
})

test_that("Return error when point forecast is negative", {
	for (this_location in unique(valid_entry$location)) {
		for (this_target in unique(valid_entry$target)) {
			invalid_entry <- valid_entry
			invalid_entry$value[invalid_entry$location == this_location &
														invalid_entry$target == this_target &
														invalid_entry$type == "Point"] <- -1
			expect_error(verify_point(invalid_entry))
			expect_error(verify_entry(invalid_entry))
		}
	}
})

