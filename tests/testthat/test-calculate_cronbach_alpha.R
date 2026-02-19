testthat::test_that("calculate_cronbach_alpha requires program and milestone", {
  responses_df <- get_responses_df()
  
  # Should error if program missing
  testthat::expect_error(
    calculate_cronbach_alpha(responses_df),
    "program_name is required"
  )
  
  # Should error if milestone missing
  testthat::expect_error(
    calculate_cronbach_alpha(responses_df, "Military"),
    "milestone_name is required"
  )
})

testthat::test_that("calculate_cronbach_alpha analyzes complete questionnaire", {
  responses_df <- get_responses_df()
  
  result <- calculate_cronbach_alpha(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # Should return valid structure
  testthat::expect_true(is.list(result))
  testthat::expect_true(all(c("context", "sample", "reliability", "interpretation") %in% names(result)))
  
  # Context should only have program and milestone (no indicator)
  testthat::expect_true(all(c("program", "milestone") %in% names(result$context)))
  testthat::expect_false("indicator" %in% names(result$context))
  
  # Should analyze many questions (complete questionnaire)
  testthat::expect_true(result$sample$n_questions_analyzed >= 10)
  
  # Alpha should be in valid range
  testthat::expect_true(result$reliability$alpha_std >= 0)
  testthat::expect_true(result$reliability$alpha_std <= 1)
})

testthat::test_that("calculate_cronbach_alpha handles zero-variance questions", {
  responses_df <- get_responses_df()
  
  result <- calculate_cronbach_alpha(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # If questions were removed, should be documented
  if (!is.null(result$sample$removed_questions)) {
    testthat::expect_true(result$sample$n_questions_removed > 0)
    testthat::expect_equal(
      result$sample$n_questions_removed,
      length(result$sample$removed_questions)
    )
  }
  
  # Should still have analyzed questions remaining
  testthat::expect_true(result$sample$n_questions_analyzed >= 2)
})

