context("generate_point_forecasts")

test_that("generate_point_forecast throws error with invalid method", {
  expect_error(generate_point_forecast(d, method = "test"))
  expect_error(generate_point_forecasts(d, method = "test"))
})


test_that("generate_point_forecast works", {
	
  set.seed(123)
  # Random onset data
  weeks <- full_entry %>%
	            filter(location == "US National", target == "Season onset",
	                   type == "Bin") %>%
	            mutate(value = runif(length(value)),
	                   bin_start_incl = suppressWarnings(as.numeric(bin_start_incl))) %>%
	            mutate(value = value/sum(value))
 
  week_exp <- weeks %>%
                na.omit() %>%
                  mutate(bin_start_incl = ifelse(bin_start_incl >= 40,
                                                 bin_start_incl, 
                                                 bin_start_incl + 52)) %>%
                  summarize(week_exp = round(sum(bin_start_incl * value/sum(value)), 0)) %>%
                  mutate(week_exp = ifelse(week_exp <= 52, 
                                           week_exp, week_exp - 52)) %>%
                  first()

  week_mode <- weeks$bin_start_incl[weeks$value == max(weeks$value)]
  week_mode <- as.numeric(ifelse(week_mode == "none", NA, week_mode))

  week_med <- weeks %>%
                    mutate(bin_start_incl = ifelse(bin_start_incl >= 40,
                                                   bin_start_incl, 
                                                   bin_start_incl + 52)) %>%
                    arrange(bin_start_incl) %>%
                    mutate(cumsum = cumsum(value)) %>%
                    filter(bin_start_incl == 
                             bin_start_incl[min(which(cumsum >= 0.5))]) %>%
                    mutate(bin_start_incl = ifelse(bin_start_incl <= 52, 
                                                   bin_start_incl, bin_start_incl - 52)) %>%
                    pull(bin_start_incl) 
 

  # Random percent data
  percent <- full_entry %>%
              filter(location == "HHS Region 1", target == "1 wk ahead",
                     type == "Bin") %>%
              mutate(value = runif(length(value))) %>%
              mutate(value = value/sum(value))
  
  percent_exp <- round(sum(as.numeric(percent$bin_start_incl) *
                           percent$value), 1)
  percent_mode <- as.numeric(percent$bin_start_incl
                             [percent$value == max(percent$value)])
  percent_med <- percent %>%
                  mutate(bin_start_incl = as.numeric(bin_start_incl)) %>%
                  arrange(bin_start_incl) %>%
                  mutate(cumsum = cumsum(value)) %>%
                  filter(bin_start_incl == 
                           bin_start_incl[min(which(cumsum >= 0.5))]) %>%
                  pull(bin_start_incl) 

  
  # Full test data
  d <- rbind(weeks, percent) %>%
        group_by(location, target)
  d$bin_start_incl[is.na(d$bin_start_incl)] <- "none"
  
  exp_truth <- data.frame(location = c("HHS Region 1", "US National"),
                        target = c("1 wk ahead", "Season onset"),
                        value = c(percent_exp, week_exp),
                        type = "Point",
                        stringsAsFactors = FALSE) 
  med_truth <- data.frame(location = c("HHS Region 1", "US National"),
                          target = c("1 wk ahead", "Season onset"),
                          value = c(percent_med, week_med),
                          type = "Point",
                          stringsAsFactors = FALSE)
  mode_truth <- data.frame(location = c("HHS Region 1", "US National"),
                           target = c("1 wk ahead", "Season onset"),
                           value = c(percent_mode, week_mode),
                          type = "Point",
                          stringsAsFactors = FALSE)
 
  # Test function
  expect_equivalent(exp_truth, generate_point_forecast(d, method = "Expect"))
  expect_equivalent(med_truth, generate_point_forecast(d, method = "Median"))
  expect_equivalent(mode_truth, generate_point_forecast(d, method = "Mode"))
  
})


test_that("median and mode recognize 'none' for onset if applicable", {
  weeks <- full_entry %>%
    filter(location %in% c("US National", "HHS Region 1"), 
                           target == "Season onset", type == "Bin") %>%
    group_by(location, target) %>%
    mutate(value = ifelse(bin_start_incl == "none",
                          3, value),
           value = value/sum(value))
  
  week_mode <- weeks %>%
                filter(bin_start_incl == bin_start_incl[value == max(value)]) %>%
                select(location, target, value = bin_start_incl, type) %>%
                mutate(value = as.numeric(ifelse(value == "none",
                                      NA, value)),
                       type = "Point")

  week_med <- weeks %>%
    mutate(bin_start_incl = suppressWarnings(as.numeric(bin_start_incl)),
           bin_start_incl = ifelse(bin_start_incl >= 40,
                                   bin_start_incl, 
                                   bin_start_incl + 52),
           type = "Point") %>%
    arrange(location, target, bin_start_incl) %>%
    mutate(cumsum = cumsum(value),
           bin_start_incl = ifelse(is.na(bin_start_incl),
                                   "none", bin_start_incl)) %>%
    filter(bin_start_incl == 
             bin_start_incl[min(which(cumsum >= 0.5))]) %>%
    select(location, target, value = bin_start_incl, type) %>%
    mutate(value = as.numeric(ifelse(value == "none",
                                     NA, value)))

  expect_equivalent(week_mode, generate_point_forecast(weeks, method = "Mode"))
  expect_equivalent(week_med, generate_point_forecast(weeks, method = "Median"))
  
})

