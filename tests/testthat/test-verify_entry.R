context("verify_entry")

valid_file_week <- system.file("extdata/EW44_ValidTest_2016-11-07.csv", package = "FluSight")
valid_entry_week <- read_entry(valid_file_week)
valid_file <- system.file("/extdata", "valid_test.csv", package="FluSight")
valid_entry <- read_entry(valid_file)
valid_hosp_file <- system.file("extdata", "EW48_ValidHospTest_2017-12-04.csv", package="FluSight")
valid_hosp_entry <- read_entry(valid_hosp_file)
valid_state_file <- system.file("extdata", "EW44_ValidStateTest_2017-11-07.csv", package = "FluSight")
valid_state_entry <- read_entry(valid_state_file)


test_that("Valid entry passes", {
	expect_true(verify_entry_file(valid_file))
	expect_warning(verify_entry(valid_entry), 
	               "Missing forecast_week - verification will proceed but forecast cannot be scored")
	expect_true(verify_entry_file(valid_file_week))
	expect_true(verify_entry(valid_entry_week))
	expect_warning(verify_entry(      full_entry), 
	               "Missing forecast_week - verification will proceed but forecast cannot be scored")
	expect_warning(verify_entry(  minimal_entry), 
	            "Missing forecast_week - verification will proceed but forecast cannot be scored")
	expect_true(verify_entry_file(valid_hosp_file, challenge = "hospital"))
	expect_true(verify_entry(valid_hosp_entry, challenge = "hospital"))
	expect_warning(verify_entry(full_entry_hosp, challenge = "hospital"), 
	               "Missing forecast_week - verification will proceed but forecast cannot be scored")
	expect_true(verify_entry_file(valid_state_file, challenge = "state_ili"))
	expect_true(verify_entry(valid_state_entry, challenge = "state_ili"))
	expect_warning(verify_entry(full_entry_state, challenge = "state_ili"), 
	               "Missing forecast_week - verification will proceed but forecast cannot be scored")
})

# test_that("Entry without forecast week generates warnings", {
#   expect_warning(verify_entry_file(valid_file))
#   expect_warning(verify_entry(valid_entry))
# })


test_that("Return error when required column name doesn't exist", {
  rand_column <- sample(names(valid_entry), 1)
  invalid_entry <- valid_entry
  names(invalid_entry)[names(invalid_entry) == rand_column] <- "invalidName"
  expect_error(arrange_entry(invalid_entry))
  expect_error(verify_entry( invalid_entry))
})


test_that("Return error when probabilities are missing", {
	rand_location <- sample(unique(valid_entry_week$location), 1)
	rand_target <- sample(unique(valid_entry_week$target), 1)

	invalid_entry <- valid_entry_week
	invalid_entry$value[invalid_entry$location == rand_location &
												invalid_entry$target == rand_target &
												invalid_entry$type == "Bin"] <- NA
	
	expect_error(verify_probabilities(invalid_entry))
	expect_error(verify_entry(invalid_entry))

})


test_that("Return error when probabilities are negative", {
  rand_location <- sample(unique(valid_entry_week$location), 1)
  rand_target <- sample(unique(valid_entry_week$target), 1)
  
	invalid_entry <- valid_entry_week
	invalid_entry$value[invalid_entry$location == rand_location &
												invalid_entry$target == rand_target &
												invalid_entry$type == "Bin"] <- -0.5
	
	expect_error(verify_probabilities(invalid_entry))
	expect_error(verify_entry(invalid_entry))
	
})


test_that("Return error when probabilities sum to less than 0.9", {
  rand_location <- sample(unique(valid_entry_week$location), 1)
  rand_target <- sample(unique(valid_entry_week$target), 1)
  
	invalid_entry <- valid_entry_week
	invalid_entry$value[invalid_entry$location == rand_location &
												invalid_entry$target == rand_target &
												invalid_entry$type == "Bin"] <- 0.01
	
	expect_error(verify_probabilities(invalid_entry))
	expect_error(verify_entry(invalid_entry))
	
	
})


test_that("Return error when probabilities sum to more than 1.1", {
  rand_location <- sample(unique(valid_entry_week$location), 1)
  rand_target <- sample(unique(valid_entry_week$target), 1)
  
	invalid_entry <- valid_entry_week
	invalid_entry$value[invalid_entry$location == rand_location &
												invalid_entry$target == rand_target &
												invalid_entry$type == "Bin"] <- 0.1
	
	expect_error(verify_probabilities(invalid_entry))
	expect_error(verify_entry(invalid_entry))
	
})

test_that("Return warning when point forecast is missing", {
  rand_location <- sample(unique(valid_entry_week$location), 1)
  rand_target <- sample(unique(valid_entry_week$target), 1)
			
  invalid_entry <- valid_entry_week
	invalid_entry$value[invalid_entry$location == rand_location &
												invalid_entry$target == rand_target &
												invalid_entry$type == "Point"] <- NA
	
	expect_warning(verify_point(invalid_entry))
	expect_warning(verify_entry(invalid_entry))
	
})

test_that("Return error when point forecast is negative", {
  rand_location <- sample(unique(valid_entry_week$location), 1)
  rand_target <- sample(unique(valid_entry_week$target), 1)
			
  invalid_entry <- valid_entry_week
	invalid_entry$value[invalid_entry$location == rand_location &
												invalid_entry$target == rand_target &
												invalid_entry$type == "Point"] <- -1
	expect_error(verify_point(invalid_entry))
	expect_error(verify_entry(invalid_entry))
	
})

