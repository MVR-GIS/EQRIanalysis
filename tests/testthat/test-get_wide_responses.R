testthat::test_that("get_wide_responses returns valid wide-format data", {
  responses_df <- get_responses_df()
  
  # Test with valid context (following plot_indicator_alluvial test pattern)
  wide_df <- get_wide_responses(
    responses_df, 
    indicator_name = "Cost",
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # Test 1: Returns a data frame
  testthat::expect_true(is.data.frame(wide_df))
  
  # Test 2: All columns should start with "Q" and be numeric
  testthat::expect_true(all(grepl("^Q\\d+$", colnames(wide_df))))
  testthat::expect_true(all(sapply(wide_df, is.numeric)))
  
  # Test 3: No list columns
  testthat::expect_false(any(sapply(wide_df, is.list)))
  
  # Test 4: Should have at least 1 column
  testthat::expect_true(ncol(wide_df) >= 1)
})

testthat::test_that("get_wide_responses matches alluvial pattern dimensions", {
  responses_df <- get_responses_df()
  
  # Use same context as plot_indicator_alluvial test
  indicator_name <- "Confidence"
  program_name <- "Military"
  milestone_name <- "95% (Final Design)"
  
  # Get wide data
  wide_df <- get_wide_responses(
    responses_df, 
    indicator_name,
    program_name,
    milestone_name
  )
  
  # Get filtered alluvial data for comparison
  alluvial_df <- get_alluvial_df()
  alluvial_filtered <- alluvial_df %>%
    dplyr::filter(
      INDICATOR == indicator_name,
      PROGRAMTYPE_NAME == program_name,
      MILESTONE_DESC == milestone_name
    )
  
  # Number of rows should match unique questionnaire events
  n_events_alluvial <- length(unique(alluvial_filtered$QUESTIONNAIREEVENT_ID))
  testthat::expect_equal(nrow(wide_df), n_events_alluvial)
})

testthat::test_that("get_wide_responses works for all indicator contexts", {
  responses_df <- get_responses_df()
  indicators_df <- get_indicators_df()
  
  # Test a few indicator-program-milestone combinations
  # (Following plot_indicator_alluvial test pattern)
  test_contexts <- list(
    list(ind = "Confidence", prog = "Military", mile = "95% (Final Design)"),
    list(ind = "Cost", prog = "Military", mile = "95% (Final Design)"),
    list(ind = "QA", prog = "Military", mile = "95% (Final Design)")
  )
  
  for (ctx in test_contexts) {
    wide_df <- get_wide_responses(
      responses_df,
      ctx$ind,
      ctx$prog,
      ctx$mile
    )
    
    testthat::expect_true(
      is.data.frame(wide_df),
      info = paste("Failed for:", ctx$ind, ctx$prog, ctx$mile)
    )
    
    testthat::expect_false(
      any(sapply(wide_df, is.list)),
      info = paste("List columns found for:", ctx$ind, ctx$prog, ctx$mile)
    )
  }
  })

  testthat::test_that("get_wide_responses requires all context parameters", {
  responses_df <- get_responses_df()
  
  # Should error if indicator missing
  testthat::expect_error(
    get_wide_responses(responses_df),
    "indicator_name is required"
  )
  
  # Should error if program missing
  testthat::expect_error(
    get_wide_responses(responses_df, "Confidence"),
    "program_name is required"
  )
  
  # Should error if milestone missing
  testthat::expect_error(
    get_wide_responses(responses_df, "Confidence", "Military"),
    "milestone_name is required"
  )
})

testthat::test_that("get_wide_responses validates parameters", {
  responses_df <- get_responses_df()
  
  # Invalid indicator
  testthat::expect_error(
    get_wide_responses(responses_df, "BadIndicator", "Military", "95% (Final Design)"),
    "indicator_name must be one of"
  )
  
  # Invalid program
  testthat::expect_error(
    get_wide_responses(responses_df, "Confidence", "BadProgram", "95% (Final Design)"),
    "program_name must be one of"
  )
  
  # Invalid milestone
  testthat::expect_error(
    get_wide_responses(responses_df, "Confidence", "Military", "BadMilestone"),
    "milestone_name must be one of"
  )
})
