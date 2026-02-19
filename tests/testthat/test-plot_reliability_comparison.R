testthat::test_that("plot_reliability_comparison creates valid plots", {
  responses_df <- get_responses_df()
  reliability_df <- run_reliability_analysis(responses_df, include_omega = TRUE)
  
  # Test each metric type
  metrics <- c("alpha_std", "omega_total", "both", "difference")
  
  for (m in metrics) {
    plot_obj <- plot_reliability_comparison(reliability_df, metric = m)
    testthat::expect_true("ggplot" %in% class(plot_obj))
  }
})

testthat::test_that("plot_reliability_comparison validates metric parameter", {
  responses_df <- get_responses_df()
  reliability_df <- run_reliability_analysis(responses_df, include_omega = TRUE)
  
  # Should error on invalid metric
  testthat::expect_error(
    plot_reliability_comparison(reliability_df, metric = "invalid"),
    "metric must be one of"
  )
})
