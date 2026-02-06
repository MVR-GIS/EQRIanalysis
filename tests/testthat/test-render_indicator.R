testthat::test_that("render_question returns valid Markdown", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Select a indicator name to test
  indicator_name <- indicators_df$INDICATOR[1]
  
  # Call render_question
  markdown_output <- render_indicator(indicators_df, indicator_name)
  
  # Verify output
  testthat::expect_type(markdown_output, "character")
  testthat::expect_true(paste0("## ", indicator_name) == markdown_output[1])
  testthat::expect_true(paste0("![](plots/indicators/q_usace_", indicator_name, ".svg)") == markdown_output[3])
})

testthat::test_that("render_indicator handles invalid indicator names gracefully", {
  # Use the package function to get the data
  indicators_df <- get_indicators_df()
  
  # Call render_question with an invalid indicator name
  invalid_indicator_name <- "Fun-n-Games"
  testthat::expect_error(
    render_indicator(indicators_df, invalid_indicator_name),
    "Indicator name not found in the indicator dataset."
  )
})
