test_that("check plot_indicator_prog_mile works", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[2]      # Cost
  program_name <- indicators_df$PROGRAMTYPE_NAME[1] # Military
  
  plot1 <- plot_indicator_prog_mile(indicator_name, program_name)
  plot1
  testthat::expect_true("ggplot" %in% class(plot1))
})
test_that("check plot_indicator_prog_mile works", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[3]      # QA
  program_name <- indicators_df$PROGRAMTYPE_NAME[2] # Military
  
  plot2 <- plot_indicator_prog_mile(indicator_name, program_name)
  plot2
  testthat::expect_true("ggplot" %in% class(plot2))
})
test_that("check plot_indicator_prog_mile works", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[4]      # QC
  program_name <- indicators_df$PROGRAMTYPE_NAME[2] # Military
  
  plot3 <- plot_indicator_prog_mile(indicator_name, program_name)
  plot3
  testthat::expect_true("ggplot" %in% class(plot3))
})