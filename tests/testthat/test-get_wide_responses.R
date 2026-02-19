testthat::test_that("get_wide_responses requires program and milestone only", {
  responses_df <- get_responses_df()
  
  # Should error if program missing
  testthat::expect_error(
    get_wide_responses(responses_df),
    "program_name is required"
  )
  
  # Should error if milestone missing
  testthat::expect_error(
    get_wide_responses(responses_df, "Military"),
    "milestone_name is required"
  )
})

testthat::test_that("get_wide_responses validates parameters", {
  responses_df <- get_responses_df()
  
  # Invalid program
  testthat::expect_error(
    get_wide_responses(responses_df, "BadProgram", "95% (Final Design)"),
    "program_name must be one of"
  )
  
  # Invalid milestone
  testthat::expect_error(
    get_wide_responses(responses_df, "Military", "BadMilestone"),
    "milestone_name must be one of"
  )
})

testthat::test_that("get_wide_responses returns complete questionnaire data", {
  responses_df <- get_responses_df()
  
  # Test with valid context
  wide_df <- get_wide_responses(
    responses_df, 
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
  
  # Test 4: Should have many questions (all questions, not just one indicator)
  # This should be much larger than before
  testthat::expect_true(ncol(wide_df) >= 10)
})

testthat::test_that("get_wide_responses handles all questions regardless of indicator", {
  responses_df <- get_responses_df()
  
  # Get wide data for a context
  wide_df <- get_wide_responses(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # Count unique questions in this context across all indicators
  n_unique_questions <- responses_df %>%
    dplyr::filter(
      PROGRAMTYPE_NAME == "Military",
      MILESTONE_DESC == "95% (Final Design)"
    ) %>%
    dplyr::pull(QUESTION_NUMBER) %>%
    unique() %>%
    length()
  
  # Wide format should include all unique questions
  testthat::expect_equal(ncol(wide_df), n_unique_questions)
})

testthat::test_that("get_wide_responses works for multiple contexts", {
  responses_df <- get_responses_df()
  
  # Test multiple program × milestone combinations
  test_contexts <- list(
    list(prog = "Military", mile = "95% (Final Design)"),
    list(prog = "Military", mile = "65% (Intermediate Design)"),
    list(prog = "Civil Works", mile = "95% (Final Design)")
  )
  
  for (ctx in test_contexts) {
    wide_df <- get_wide_responses(
      responses_df,
      ctx$prog,
      ctx$mile
    )
    
    testthat::expect_true(
      is.data.frame(wide_df),
      info = paste("Failed for:", ctx$prog, "×", ctx$mile)
    )
    
    testthat::expect_false(
      any(sapply(wide_df, is.list)),
      info = paste("List columns found for:", ctx$prog, "×", ctx$mile)
    )
    
    # Should have substantial number of questions
    testthat::expect_true(
      ncol(wide_df) >= 5,
      info = paste("Too few questions for:", ctx$prog, "×", ctx$mile)
    )
  }
})

testthat::test_that("get_wide_responses produces unique rows per questionnaire event", {
  responses_df <- get_responses_df()
  
  wide_df <- get_wide_responses(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # Should have no duplicate rows (each row = unique questionnaire event)
  testthat::expect_equal(nrow(wide_df), nrow(unique(wide_df)))
  
  # Number of rows should match unique questionnaire events in this context
  n_events_expected <- responses_df %>%
    dplyr::filter(
      PROGRAMTYPE_NAME == "Military",
      MILESTONE_DESC == "95% (Final Design)"
    ) %>%
    dplyr::pull(QUESTIONNAIREEVENT_ID) %>%
    unique() %>%
    length()
  
  testthat::expect_equal(nrow(wide_df), n_events_expected)
})
