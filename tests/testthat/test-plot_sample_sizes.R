test_that("check plot output", {
  # Get data
  responses_df <- get_responses_df()

  # Run reliability analysis across all contexts
  reliability_results <- run_reliability_analysis(
    responses_df,
    include_omega = TRUE
  )

  # View summary table
  summary_table <- create_reliability_table(reliability_results)
  print(summary_table)

  # Create visualizations
  plot_alpha <- plot_reliability_comparison(
    reliability_results,
    metric = "alpha_std"
  )
  plot_both <- plot_reliability_comparison(reliability_results, metric = "both")
  plot_diff <- plot_reliability_comparison(
    reliability_results,
    metric = "difference"
  )
  plot_n <- plot_sample_sizes(reliability_results)

  # Display plots
  plot_alpha
  plot_both
  plot_diff
  plot_n

  # Test
  testthat::expect_true("ggplot" %in% class(plot_alpha))
  testthat::expect_true("ggplot" %in% class(plot_both))
  testthat::expect_true("ggplot" %in% class(plot_diff))
  testthat::expect_true("ggplot" %in% class(plot_n))
})
