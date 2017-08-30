context("verify_agegrp")

test_that("Correct entries are successful.",{
  expect_true(verify_agegrp(full_entry_hosp))
})

test_that("Missing Overall report errors.", {
  tmp_entry <- full_entry_hosp[full_entry_hosp$age_grp != "Overall",]
  expect_error(verify_agegrp(tmp_entry))
})

test_that("Extra age group reports error.", {
  tmp_entry <- full_entry_hosp
  tmp_entry$age_grp[1] = "extra age"
  expect_error(verify_agegrp(tmp_entry))
})

test_that("Missing specific age groups reports message.", {
  tmp_entry <- full_entry_hosp
  age_grps <- setdiff(unique(full_entry_hosp$age_grp), "Overall")
  for (i in seq_along(age_grps)) {
    tmp_entry <- full_entry_hosp[full_entry_hosp$age_grp != age_grps[i],]
    expect_message(verify_agegrp(tmp_entry))
  }
})
