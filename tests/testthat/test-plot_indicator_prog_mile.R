test_that("check plot_indicator_prog_mile works", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[2]
  program_name <- indicators_df$PROGRAMTYPE_NAME[1]
  
  plot1 <- plot_indicator_prog_mile(indicator_name, program_name)
  plot1
  testthat::expect_true("ggplot" %in% class(plot1))
})
test_that("check plot_indicator_prog_mile works", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[3]
  program_name <- indicators_df$PROGRAMTYPE_NAME[2]
  
  plot2 <- plot_indicator_prog_mile(indicator_name, program_name)
  plot2
  testthat::expect_true("ggplot" %in% class(plot2))
})