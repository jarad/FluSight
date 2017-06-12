context("expand_truth")

test_that("expand_week works", {
  skip_on_cran()
  
  tmp_week <- truth_1516 %>%
                filter(target %in% c("Season onset", "Season peak week")) %>%
                expand_week(1)
  tmp_valid <- filter(valid_exp_truth, target %in%
                                 c("Season onset", "Season peak week"))
  
  expect_equivalent(tmp_week, tmp_valid)  	
})

test_that("expand_percent works", {
  skip_on_cran()
  
  tmp_percent <- truth_1516 %>%
                    filter(target %in% c("Season peak percentage", "1 wk ahead",
                                         "2 wk ahead", "3 wk ahead", 
                                         "4 wk ahead")) %>%
                    expand_percent(5)
  tmp_valid <- filter(valid_exp_truth, target %in%
                        c("Season peak percentage", "1 wk ahead",
                          "2 wk ahead", "3 wk ahead", 
                          "4 wk ahead"))
  
  expect_equivalent(tmp_percent, tmp_valid)
})

test_that("expand_truth works", {
  skip_on_cran()
  
  tmp_truth <- expand_truth(truth_1516)
  
  expect_equivalent(tmp_truth, valid_exp_truth)
})

test_that("expand_truth handles NA truth", {
  skip_on_cran()
  
  rand_location <- sample(unique(truth_1516$location), 1) 
  
  tmp_truth <- truth_1516 %>%
    mutate(bin_start_incl = ifelse(location == rand_location & 
                                     target %in% c("Season peak week", "Season peak percentage"),
                                   NA,
                                   bin_start_incl))
  
  tmp_valid <- valid_exp_truth %>%
    mutate(bin_start_incl = ifelse(location == rand_location & 
                                     target %in% c("Season peak week", "Season peak percentage"),
                                   NA,
                                   bin_start_incl)) %>%
    unique()

  expect_equivalent(expand_truth(tmp_truth), tmp_valid)
})

test_that("expand_week deals with New Year transition", {
  skip_on_cran()
  
  # Set one region to have onset week 52
  tmp_truth <- truth_1516 %>%
    filter(target %in% c("Season onset", "Season peak week"))
  rand_location <- sample(unique(tmp_truth$location), 1)
  tmp_truth$bin_start_incl[tmp_truth$location == rand_location &
                             tmp_truth$target == "Season onset"] <- "52.0"
  
  # Set expanded truth to reflect week 52 onset
  tmp_valid <- filter(valid_exp_truth, target %in%
                        c("Season onset", "Season peak week"))
  tmp_valid$bin_start_incl[tmp_valid$location == rand_location &
                             tmp_valid$target == "Season onset"] <- 
    c("51.0", "52.0", "1.0")

  tmp_week <- expand_week(tmp_truth, 1)
  
  expect_equivalent(tmp_week, tmp_valid)
  
  
  
  # Set one region to have onset week 1
  tmp_truth <- truth_1516 %>%
    filter(target %in% c("Season onset", "Season peak week"))
  rand_location <- sample(unique(tmp_truth$location), 1)
  tmp_truth$bin_start_incl[tmp_truth$location == rand_location &
                             tmp_truth$target == "Season onset"] <- "1.0"
  
  # Set expanded truth to reflect week 52 onset
  tmp_valid <- filter(valid_exp_truth, target %in%
                        c("Season onset", "Season peak week"))
  tmp_valid$bin_start_incl[tmp_valid$location == rand_location &
                             tmp_valid$target == "Season onset"] <- 
    c("52.0", "1.0", "2.0")
  
  tmp_week <- expand_week(tmp_truth, 1)
  
  expect_equivalent(tmp_week, tmp_valid)
  
  
  
  # Set one region to have peak week 52
  tmp_truth <- truth_1516 %>%
    filter(target %in% c("Season onset", "Season peak week"))
  rand_location <- sample(unique(tmp_truth$location), 1)
  tmp_truth$bin_start_incl[tmp_truth$location == rand_location &
                             tmp_truth$target == "Season peak week"] <- "52.0"
  
  # Set expanded truth to reflect week 52 peak
  tmp_valid <- filter(valid_exp_truth, target %in%
                        c("Season onset", "Season peak week"))
  tmp_valid$bin_start_incl[tmp_valid$location == rand_location &
                             tmp_valid$target == "Season peak week"] <- 
    c("51.0", "52.0", "1.0")
  
  tmp_week <- expand_week(tmp_truth, 1)
  
  expect_equivalent(tmp_week, tmp_valid)
  
  
  
  # Set one region to have peak week 1
  tmp_truth <- truth_1516 %>%
    filter(target %in% c("Season onset", "Season peak week"))
  rand_location <- sample(unique(tmp_truth$location), 1)
  tmp_truth$bin_start_incl[tmp_truth$location == rand_location &
                             tmp_truth$target == "Season peak week"] <- "1.0"
  
  # Set expanded truth to reflect week 1 peak
  tmp_valid <- filter(valid_exp_truth, target %in%
                        c("Season onset", "Season peak week"))
  tmp_valid$bin_start_incl[tmp_valid$location == rand_location &
                             tmp_valid$target == "Season peak week"] <- 
    c("52.0", "1.0", "2.0")
  
  tmp_week <- expand_week(tmp_truth, 1)
  
  expect_equivalent(tmp_week, tmp_valid)
  
})

test_that("expand_week catches 'none' in onset", {
  skip_on_cran()
  
  # Set one region to have no onset
  tmp_truth <- truth_1516 %>%
                  filter(target %in% c("Season onset", "Season peak week"))
  rand_location <- sample(unique(tmp_truth$location), 1)
  tmp_truth$bin_start_incl[tmp_truth$location == rand_location &
                             tmp_truth$target == "Season onset"] <- "none"
  
  # Set expanded truth to reflect no onset
  tmp_valid <- filter(valid_exp_truth, target %in%
                        c("Season onset", "Season peak week"))
  tmp_valid$bin_start_incl[tmp_valid$location == rand_location &
                             tmp_valid$target == "Season onset"] <- "none"
  tmp_valid <- unique(tmp_valid)
  
  
  tmp_week <- expand_week(tmp_truth, 1)
  
  expect_equivalent(tmp_week, tmp_valid)  	
})

test_that("expand_percent doesn't return negative results", {
  skip_on_cran()
  
  # Set one peak percentage to be < 0.5
  tmp_truth <- truth_1516 %>%
    filter(target %in% c("Season peak percentage", "1 wk ahead",
                         "2 wk ahead", "3 wk ahead", 
                         "4 wk ahead"))
  rand_location <- sample(unique(tmp_truth$location), 1)
  tmp_truth$bin_start_incl[tmp_truth$location == rand_location &
                             tmp_truth$target == "Season peak percentage"] <- 
                             "0.3"

  # Set expanded truth to reflect reduced peak percentage
  tmp_valid <- filter(valid_exp_truth, target %in%
                        c("Season peak percentage", "1 wk ahead",
                          "2 wk ahead", "3 wk ahead", 
                          "4 wk ahead"))
  tmp_valid$bin_start_incl[tmp_valid$location == rand_location &
                             tmp_valid$target == "Season peak percentage"] <- 
                             c("-0.2", "-0.1", "0.0", "0.1", "0.2", 
                               "0.3", "0.4", "0.5", "0.6", "0.7", "0.8")
  tmp_valid <- filter(tmp_valid, as.numeric(bin_start_incl) >= 0)

  tmp_percent <- expand_percent(tmp_truth, 5)
 
  expect_equivalent(tmp_percent, tmp_valid)

})


test_that("expand_truth handles consecutive peaks", {
  skip_on_cran()
  
  rand_location <- sample(unique(truth_1516$location), 1)
  peak_week <- truth_1516 %>%
    filter(location == rand_location, target == "Season peak week") %>%
    select(bin_start_incl) %>%
    as.double()
    
  
  tmp_truth <- truth_1516 %>%
    bind_rows(tibble(target = "Season peak week",
                     location = rand_location,
                     forecast_week = NA,
                     bin_start_incl = trimws(format(peak_week + 1, nsmall = 1))))
  
  tmp_valid <- valid_exp_truth %>%
    bind_rows(tibble(target = "Season peak week",
                     location = rand_location,
                     forecast_week = NA,
                     bin_start_incl = trimws(format(peak_week + 2, nsmall = 1)))) 
  
  expect_equivalent(expand_truth(tmp_truth), tmp_valid)
})