context("create_seasonal")

test_that("Onset creation works", {
  locations <- unique(weekILI$location)
  
  for (i in seq_along(locations)) {
    tmp_onset <- create_onset(weekILI, locations[i])
    watmp_truth <- truth_1516[truth_1516$location == locations[i] &
                              truth_1516$target == "Season onset", ]
    expect_equal(tmp_onset, tmp_truth)
  }
})