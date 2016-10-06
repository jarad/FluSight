context("verify_units")

test_that("Correct entries are successful.",{
  expect_true(verify_units(minimal_entry))
  expect_true(verify_units(full_entry   ))
})

test_that("Missing units report errors.", {
  valid_units <- unique(minimal_entry$unit)
  
  for (i in seq_along(valid_units)) {
    tmp_entry <- minimal_entry[minimal_entry$unit != valid_units[i],]
    expect_error(verify_units(tmp_entry))
  }
})

test_that("Extra unit reports warning.", {
  tmp_entry <- minimal_entry
  tmp_entry$unit[1] <- "extra unit"
  expect_warning(verify_units(tmp_entry))
})
