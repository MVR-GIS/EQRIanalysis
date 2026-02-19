test_that("check calculate_cronbach_alpha", {
  responses_df <- get_responses_df()

  indicator_name = "Cost"
  program_name = "Military"
  milestone_name = "95% (Final Design)"
  check.keys = FALSE

  # Test with valid context (following plot_indicator_alluvial test pattern)
  wide_df <- calculate_cronbach_alpha(
    responses_df, 
    indicator_name,
    program_name,
    milestone_name
  )
  
})
