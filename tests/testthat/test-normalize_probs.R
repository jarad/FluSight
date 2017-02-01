context("normalize_probs")

test_that("normalize_probs doesn't affect valid submission", {
  expect_equivalent(full_entry_score, normalize_probs(full_entry_score))
  expect_equivalent(minimal_entry, normalize_probs(minimal_entry))
})

test_that("normalize_probs correctly normalizes prob < 1", {
  rand_location <- sample(unique(full_entry_score$location), 1)
  rand_target <- sample(unique(full_entry_score$target), 1)
  
  tmp_min <- minimal_entry %>%
    mutate(value = ifelse(target == rand_target & type == "Bin", value * 0.95, value))

  tmp_full <- full_entry_score %>%
    mutate(value = ifelse(target == rand_target & location == rand_location & type == "Bin",
                          value * 0.95, value))
  
  expect_equivalent(minimal_entry, normalize_probs(tmp_min))
  expect_equivalent(full_entry_score, normalize_probs(tmp_full))
})

test_that("normalize_probs correctly normalizes 1 < prob", {
  rand_location <- sample(unique(full_entry_score$location), 1)
  rand_target <- sample(unique(full_entry_score$target), 1)
  
  tmp_min <- minimal_entry %>%
    mutate(value = ifelse(target == rand_target & type == "Bin", value * 1.05, value))
  
  tmp_full <- full_entry_score %>%
    mutate(value = ifelse(target == rand_target & location == rand_location & type == "Bin",
                          value * 1.05, value))
  
  expect_equivalent(minimal_entry, normalize_probs(tmp_min))
  expect_equivalent(full_entry_score, normalize_probs(tmp_full))
})

test_that("normalize_probs normalizes close prob and ignores invalid", {
  i <- sample(1:6, 1)
  
  rand_location1 <- unique(full_entry_score$location)[i]
  rand_target1 <- unique(full_entry_score$target)[i]
  rand_location2 <- unique(full_entry_score$location)[i+1]
  rand_target2 <- unique(full_entry_score$target)[i+1]
  
  tmp_min <- minimal_entry %>%
    mutate(value = ifelse(target == rand_target1 & type == "Bin", value * 1.05, value)) %>%
    mutate(value = ifelse(target == rand_target2 & type == "Bin", 0, value))
  
  tmp_full <- full_entry_score %>%
    mutate(value = ifelse(target == rand_target1 & location == rand_location1 & type == "Bin",
                          value * 1.05, value)) %>%
    mutate(value = ifelse(target == rand_target2 & location == rand_location2 & type == "Bin",
                          0, value))
  
  expect_equivalent(
    minimal_entry %>% 
      mutate(value = ifelse(target == rand_target2 & type == "Bin", 0, value)),
    normalize_probs(tmp_min, ignore_invalid = TRUE))
  expect_equivalent(
    full_entry_score %>%
      mutate(value = ifelse(target == rand_target2 & location == rand_location2 & type == "Bin",
                            0, value)),
    normalize_probs(tmp_full, ignore_invalid = TRUE))
})