test_that("check plot_indicator_alluvial works", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[1]      # Confidence
  program_name <- indicators_df$PROGRAMTYPE_NAME[1] # Military
  milestone_name <- indicators_df$MILESTONE_DESC[1] # 95%
  
  plot1 <- plot_indicator_alluvial(indicator_name, 
                                   program_name, milestone_name)
  plot1
  testthat::expect_true("ggplot" %in% class(plot1))
})
test_that("check plot_indicator_alluvial works", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[2]      # Cost
  program_name <- indicators_df$PROGRAMTYPE_NAME[2] # Military
  milestone_name <- indicators_df$MILESTONE_DESC[2] # 95%
  
  plot2 <- plot_indicator_alluvial(indicator_name, 
                                   program_name, milestone_name)
  plot2
  testthat::expect_true("ggplot" %in% class(plot2))
})

test_that("check plot_indicator_alluvial works", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[3]      # QA
  program_name <- indicators_df$PROGRAMTYPE_NAME[2] # Military
  milestone_name <- indicators_df$MILESTONE_DESC[2] # 95%
  
  plot3 <- plot_indicator_alluvial(indicator_name, 
                                   program_name, milestone_name)
  plot3
  testthat::expect_true("ggplot" %in% class(plot3))
})