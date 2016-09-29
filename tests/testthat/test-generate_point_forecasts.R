context("Generate point forecasts")




test_that("generate_point_forecast works", {
	n <- 100

  values <- rnorm(n)
  probs <- runif(n); probs <- probs/sum(probs)
  exp_value = sum(values*probs)

  d = data.frame(bin_start_incl = values,
  							 value = probs)

  d_truth = data.frame(value = exp_value,
  										 type="Point",
  										 stringsAsFactors = FALSE)

  expect_equal(d_truth, generate_point_forecast(d))
})


