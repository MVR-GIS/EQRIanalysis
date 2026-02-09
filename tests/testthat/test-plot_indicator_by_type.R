testthat::test_that("rcheck plot_indicator_by_type", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[7]
  
  plot1 <- plot_indicator_by_type(indicators_df, indicator_name)
  #plot1
  testthat::expect_true("ggplot" %in% class(plot1))
})
