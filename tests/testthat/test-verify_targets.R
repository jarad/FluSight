context("verify_targets")

test_that("Correct entries are successful.",{
  expect_true(verify_targets(minimal_entry))
  expect_true(verify_targets(full_entry   ))
  expect_true(verify_targets(full_entry_hosp, challenge ="hospital"))
  expect_true(verify_targets(full_entry_state, challenge = "state_ili"))
})

test_that("Missing targets report errors.", {
  valid_targets <- unique(minimal_entry$target)
  
  for (i in seq_along(valid_targets)) {
    tmp_entry <- minimal_entry[minimal_entry$target != valid_targets[i],]
    expect_error(verify_targets(tmp_entry))
  }
})

test_that("Extra target reports warning.", {
  tmp_entry <- minimal_entry
  tmp_entry$target[1] <- "extra target"
  expect_warning(verify_targets(tmp_entry))
})
