context("get_week")

test_that("get_week gives warning", {
  expect_warning(get_week("2016-11-07"))
})


test_that("First submission date works", {
  expect_equal(suppressWarnings(get_week("2016-11-07")), 43)
  expect_equal(suppressWarnings(get_week("2016-11-14")), 44)
  expect_equal(suppressWarnings(get_week("2016-11-21")), 45)
  expect_equal(suppressWarnings(get_week("2016-11-28")), 46)
  expect_equal(suppressWarnings(get_week("2016-12-05")), 47)
  expect_equal(suppressWarnings(get_week("2016-12-12")), 48)
  expect_equal(suppressWarnings(get_week("2016-12-19")), 49)
  expect_equal(suppressWarnings(get_week("2016-12-26")), 50)
  expect_equal(suppressWarnings(get_week("2017-01-02")), 51)
  expect_equal(suppressWarnings(get_week("2017-01-09")), 52)
  expect_equal(suppressWarnings(get_week("2017-01-16")),  1)
})


context("write_entry")

valid_file  <- system.file("extdata", "valid-test.csv", package="FluSight")
valid_entry <- read_entry(valid_file)

test_that("Valid entry successfully writes",{
  expect_true(write_entry(valid_entry, path = tempdir(),
                          team_name = "defaultName", week = 0))
})

test_that("Missing arguments result in proper warnings/errors", {
  expect_error(write_entry(valid_entry))
  expect_error(write_entry(valid_entry, path = tempdir()))
  expect_error(write_entry())
  expect_error(write_entry(valid_entry, path = tempdir(), week = 0))

  expect_warning(write_entry(valid_entry, path = tempdir(),
                             team_name = "defaultName"))
})
