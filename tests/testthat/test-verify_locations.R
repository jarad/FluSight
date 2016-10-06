context("verify_locations")

test_that("Correct entries are successful.",{
  expect_true(verify_locations(minimal_entry))
  expect_true(verify_locations(full_entry   ))
})

test_that("Missing US National report errors.", {
  tmp_entry <- full_entry[full_entry$location != "US National",]
  expect_error(verify_locations(tmp_entry))
})

test_that("Extra location reports warning.", {
  tmp_entry <- full_entry
  tmp_entry$location[1] = "extra location"
  expect_warning(verify_locations(tmp_entry))
})

test_that("Missing regional location reports message.", {
  tmp_entry <- full_entry
  region_locations = setdiff(unique(full_entry$location), "US National")
  for (i in seq_along(region_locations)) {
    tmp_entry <- full_entry[full_entry$location != region_locations[i],]
    expect_message(verify_locations(tmp_entry))
  }
})
