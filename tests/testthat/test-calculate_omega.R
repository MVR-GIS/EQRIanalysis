testthat::test_that("calculate_omega requires program and milestone", {
  responses_df <- get_responses_df()
  
  # Should error if program missing (inherited from get_wide_responses)
  testthat::expect_error(
    calculate_omega(responses_df),
    "program_name is required"
  )
  
  # Should error if milestone missing
  testthat::expect_error(
    calculate_omega(responses_df, "Military"),
    "milestone_name is required"
  )
})

testthat::test_that("calculate_omega returns valid structure or NULL", {
  responses_df <- get_responses_df()
  
  result <- calculate_omega(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # Should return either a valid list or NULL
  testthat::expect_true(is.list(result) || is.null(result))
  
  if (!is.null(result)) {
    # Should have expected structure
    testthat::expect_true(all(c("context", "sample", "omega") %in% names(result)))
    
    # Context should match input
    testthat::expect_equal(result$context$program, "Military")
    testthat::expect_equal(result$context$milestone, "95% (Final Design)")
    
    # Omega values should be in valid range [0, 1]
    testthat::expect_true(result$omega$omega_total >= 0)
    testthat::expect_true(result$omega$omega_total <= 1)
    
    # Should have alpha for comparison
    testthat::expect_true(is.numeric(result$omega$alpha_comparison))
  }
})

testthat::test_that("calculate_omega handles insufficient items gracefully", {
  responses_df <- get_responses_df()
  
  # Test with a context that might have few items
  # Omega requires at least 3 items
  # This should either return results or NULL with warning
  testthat::expect_warning(
    result <- calculate_omega(
      responses_df,
      program_name = "Civil Works",
      milestone_name = "15% (Project Initiation)"
    ),
    "Omega|items|sample size",
    ignore.case = TRUE
  )
})

testthat::test_that("calculate_omega compares with alpha", {
  responses_df <- get_responses_df()
  
  # Calculate both alpha and omega for same context
  alpha_result <- calculate_cronbach_alpha(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  omega_result <- calculate_omega(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  if (!is.null(omega_result)) {
    # Alpha from omega should be close to standalone alpha calculation
    # (may differ slightly due to handling of zero-variance items)
    testthat::expect_true(
      abs(omega_result$omega$alpha_comparison - alpha_result$reliability$alpha_std) < 0.05,
      info = "Omega's alpha comparison should match standalone alpha calculation"
    )
    
    # Omega total should be >= alpha per psychometric theory
    # (omega is upper bound on reliability)
    testthat::expect_true(
      omega_result$omega$omega_total >= omega_result$omega$alpha_comparison - 0.01,
      info = "Omega total should be >= alpha (allowing for rounding)"
    )
  }
})

testthat::test_that("calculate_omega handles multiple contexts", {
  responses_df <- get_responses_df()
  
  test_contexts <- list(
    list(prog = "Military", mile = "95% (Final Design)"),
    list(prog = "Military", mile = "65% (Intermediate Design)")
  )
  
  for (ctx in test_contexts) {
    result <- suppressWarnings({
      calculate_omega(responses_df, ctx$prog, ctx$mile)
    })
    
    # Should return list or NULL (not error)
    testthat::expect_true(
      is.list(result) || is.null(result),
      info = paste("Failed for:", ctx$prog, "×", ctx$mile)
    )
  }
})

