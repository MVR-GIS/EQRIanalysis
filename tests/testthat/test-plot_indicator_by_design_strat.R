testthat::test_that("check plot_indicator_by_design_strat", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[7]
  plot1 <- plot_indicator_by_design_strat(indicators_df, indicator_name)
  plot1
  testthat::expect_true("ggplot" %in% class(plot1))
})
