context("verify_types")

test_that("Correct entries are successful.",{
  expect_true(verify_types(minimal_entry))
  expect_true(verify_types(full_entry   ))
  expect_true(verify_types(full_entry_hosp, challenge = "hospital"))
  expect_true(verify_types(full_entry_state, challenge = "state_ili"))
})

test_that("Misspecified challenge throws errors", {
  expect_error(verify_types(full_entry, challenge = "hosp"))
})

test_that("Missing types report errors.", {
  valid_types <- unique(minimal_entry$type)
  
  for (i in seq_along(valid_types)) {
    tmp_entry <- minimal_entry[minimal_entry$type != valid_types[i],]
    expect_error(verify_types(tmp_entry))
  }
})

test_that("Extra type reports warning.", {
  tmp_entry <- minimal_entry
  tmp_entry$type[1] <- "extra type"
  expect_warning(verify_types(tmp_entry))
})
