context("create_seasonal")

test_that("Onset creation works", {
  locations <- unique(valid_ILI$location)

  for (i in seq_along(locations)) {
    tmp_onset <- create_onset(valid_ILI, locations[i])
    tmp_truth <- truth_1516[truth_1516$location == locations[i] &
                              truth_1516$target == "Season onset", ]
    expect_equivalent(tmp_onset, tmp_truth)
  }
})

test_that("Peak creation works", {
  locations <- unique(valid_ILI$location)
  
  for (i in seq_along(locations)) {
    tmp_peak <- create_peak(valid_ILI, locations[i])
    tmp_truth <- truth_1516[truth_1516$location == locations[i] &
                              truth_1516$target %in% 
                              c("Season peak week", "Season peak percentage"), ]
    expect_equivalent(tmp_peak, tmp_truth)
  }
  
})

