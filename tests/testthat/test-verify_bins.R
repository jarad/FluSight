context("verify_bins")

test_that("Correct entries are successful.",{
  expect_true(verify_bins(minimal_entry))
  expect_true(verify_bins(full_entry   ))
})

test_that("Missing bins report errors.", {
  rand_target <- sample(minimal_entry$target, 1)
  
  valid_bins <- unique(minimal_entry$bin_start_incl[minimal_entry$target ==
                                                      rand_target & 
                                                    minimal_entry$type == 
                                                      "Bin"])
  
  for (i in seq_along(valid_bins)) {
    tmp_entry <- minimal_entry[minimal_entry$bin_start_incl != valid_bins[i], ]
    expect_error(verify_bins(tmp_entry))
  }
})

test_that("Extra bin reports warning.", {
  rand_target <- sample(minimal_entry$target, 1)
  
  extra_row <- head(minimal_entry[minimal_entry$target == rand_target &
                                    minimal_entry$type == "Bin", ], n = 1)
  extra_row$bin_start_incl <- "extra"
  tmp_entry <- rbind(minimal_entry, extra_row)
  
  expect_warning(verify_bins(tmp_entry))
})
