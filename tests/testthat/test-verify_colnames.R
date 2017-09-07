context("verify_colnames")

test_that("Correct entries are successful.",{
  expect_warning(verify_colnames(minimal_entry))
  expect_true(verify_colnames(full_entry_score))
  expect_true(verify_colnames(full_entry_hosp_score, challenge = "hospital"))
  expect_true(verify_colnames(full_entry_state_score, challenge = "state_ili"))
})

test_that("Missing columns report errors.", {
  for (i in seq_along(colnames(minimal_entry))) {
    tmp_entry <- minimal_entry[,-i]
    expect_error(verify_colnames(tmp_entry))
  }
})

# test_that("Missing forecast_week reports warning", {
#   expect_warning(verify_colnames(minimal_entry))
#   expect_warning(verify_colnames(full_entry))
# })

test_that("Extra column report warnings.", {
  tmp_entry <- minimal_entry
  tmp_entry$extra_column = NA
  expect_warning(verify_colnames(tmp_entry))
})


