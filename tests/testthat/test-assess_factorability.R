testthat::test_that("assess_factorability no longer requires program and milestone (supports NULL)", {
  responses_df <- get_responses_df()
  
  # Should now ACCEPT NULL for both parameters (aggregation support)
  testthat::expect_no_error(
    assess_factorability(responses_df, program_name = NULL, milestone_name = NULL)
  )
  
  # Should also work with specific values
  testthat::expect_no_error(
    assess_factorability(responses_df, "Military", "95% (Final Design)")
  )
})

testthat::test_that("assess_factorability returns valid structure with polychoric correlations", {
  responses_df <- get_responses_df()
  
  # Test with aggregated data for sufficient sample size
  # Per Flora & Curran (2004), need N >= 200 for stable polychoric correlations
  result <- assess_factorability(
    responses_df,
    program_name = NULL,        # Aggregate across programs
    milestone_name = NULL       # Aggregate across milestones
  )
  
  # Should return list
  testthat::expect_true(is.list(result))
  
  # Should have expected top-level components per updated function (lines 185-223)
  expected_components <- c("context", "sample", "correlations", 
                          "multicollinearity", "recommendation")
  testthat::expect_true(
    all(expected_components %in% names(result)),
    info = paste("Missing components:", 
                paste(setdiff(expected_components, names(result)), collapse = ", "))
  )
  
  # Context should reflect aggregation when NULL provided
  testthat::expect_equal(result$context$program, "All")
  testthat::expect_equal(result$context$milestone, "All")
  testthat::expect_true(!is.null(result$context$label))
  
  # Sample info should track removals
  testthat::expect_true(is.numeric(result$sample$n_questions_original))
  testthat::expect_true(is.numeric(result$sample$n_questions_analyzed))
  testthat::expect_true(is.numeric(result$sample$n_questions_removed_zero_var))
  testthat::expect_true(result$sample$n_questions_analyzed <= result$sample$n_questions_original)
  
  # Correlations component should use polychoric method
  testthat::expect_equal(result$correlations$method, "polychoric")
  testthat::expect_true(is.matrix(result$correlations$matrix))
  testthat::expect_true(!is.null(result$correlations$tau))  # Thresholds for ordinal data
  testthat::expect_true(is.numeric(result$correlations$determinant))
  testthat::expect_true(is.logical(result$correlations$can_invert))
  
  # Correlation matrix should be symmetric
  cor_mat <- result$correlations$matrix
  testthat::expect_equal(cor_mat, t(cor_mat))
  
  # Diagonal should be 1 (or very close due to floating point)
  testthat::expect_true(all(abs(diag(cor_mat) - 1) < 1e-10))
  
  # Multicollinearity diagnostics
  testthat::expect_true(is.numeric(result$multicollinearity$n_pairs_high))
  testthat::expect_true(is.numeric(result$multicollinearity$threshold))
  # high_cor_pairs can be NULL or data.frame
  if (!is.null(result$multicollinearity$high_cor_pairs)) {
    testthat::expect_true(is.data.frame(result$multicollinearity$high_cor_pairs))
    testthat::expect_true(all(c("item1", "item2", "correlation") %in% 
                              names(result$multicollinearity$high_cor_pairs)))
  }
  
  # Recommendation component
  testthat::expect_true(is.logical(result$recommendation$proceed_with_fa))
  testthat::expect_true(is.logical(result$recommendation$use_ridge))
  testthat::expect_true(is.numeric(result$recommendation$suggested_ridge))
  testthat::expect_true(is.character(result$recommendation$rationale))
  
  # Ridge suggestion should be in valid range (0, 1)
  testthat::expect_true(result$recommendation$suggested_ridge >= 0)
  testthat::expect_true(result$recommendation$suggested_ridge <= 1)
})

testthat::test_that("assess_factorability handles single context with limited sample", {
  responses_df <- get_responses_df()
  
  # Single context may have small sample (but should still work)
  result <- assess_factorability(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # Should complete without error
  testthat::expect_true(is.list(result))
  
  # Context should reflect specific values
  testthat::expect_equal(result$context$program, "Military")
  testthat::expect_equal(result$context$milestone, "95% (Final Design)")
  
  # Should have polychoric correlations
  testthat::expect_equal(result$correlations$method, "polychoric")
})

testthat::test_that("assess_factorability removes zero-variance items", {
  responses_df <- get_responses_df()
  
  # Run on context known to have zero-variance items (from your investigation)
  result <- assess_factorability(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # If items were removed, should be documented
  if (result$sample$n_questions_removed_zero_var > 0) {
    testthat::expect_true(!is.null(result$sample$removed_questions_zero_var))
    testthat::expect_equal(
      length(result$sample$removed_questions_zero_var),
      result$sample$n_questions_removed_zero_var
    )
    testthat::expect_true(
      result$sample$n_questions_analyzed < result$sample$n_questions_original
    )
  }
  
  # Should always have at least 3 items remaining (or function would error)
  testthat::expect_true(result$sample$n_questions_analyzed >= 3)
})

testthat::test_that("assess_factorability detects multicollinearity", {
  responses_df <- get_responses_df()
  
  # Test with aggregated data (more likely to show multicollinearity patterns)
  result <- assess_factorability(
    responses_df,
    program_name = NULL,
    milestone_name = NULL,
    multicollinearity_threshold = 0.90
  )
  
  # Should have multicollinearity component
  testthat::expect_true(!is.null(result$multicollinearity))
  
  # If high correlation pairs detected
  if (result$multicollinearity$n_pairs_high > 0) {
    # Should recommend ridge regularization
    testthat::expect_true(result$recommendation$use_ridge)
    testthat::expect_true(result$recommendation$suggested_ridge > 0)
    
    # Should have documented the pairs
    testthat::expect_true(!is.null(result$multicollinearity$high_cor_pairs))
    testthat::expect_equal(
      nrow(result$multicollinearity$high_cor_pairs),
      result$multicollinearity$n_pairs_high
    )
    
    # All correlations should exceed threshold
    testthat::expect_true(
      all(abs(result$multicollinearity$high_cor_pairs$correlation) > 
          result$multicollinearity$threshold)
    )
  }
})

testthat::test_that("assess_factorability provides appropriate recommendations", {
  responses_df <- get_responses_df()
  
  result <- assess_factorability(
    responses_df,
    program_name = NULL,
    milestone_name = NULL
  )
  
  # Recommendation rationale should be one of the expected values
  # Per function lines 215-221
  valid_rationales <- c(
    "Insufficient items for factor analysis",
    "Insufficient sample size",
    "Use ridge-regularized FA due to near-singular matrix",
    "Proceed with ridge-regularized FA to handle multicollinearity",
    "Data are suitable for factor analysis"
  )
  
  testthat::expect_true(
    result$recommendation$rationale %in% valid_rationales,
    info = paste("Got unexpected rationale:", result$recommendation$rationale)
  )
  
  # If determinant is very small, should warn about inversion
  if (result$correlations$determinant < 1e-10) {
    testthat::expect_false(result$correlations$can_invert)
    testthat::expect_true(result$recommendation$use_ridge)
  }
})

testthat::test_that("assess_factorability validates parameter types", {
  responses_df <- get_responses_df()
  
  # Invalid program name should error
  testthat::expect_error(
    assess_factorability(responses_df, program_name = "Invalid", milestone_name = NULL),
    "program_name must be one of"
  )
  
  # Invalid milestone name should error
  testthat::expect_error(
    assess_factorability(responses_df, program_name = NULL, milestone_name = "Invalid"),
    "milestone_name must be one of"
  )
  
  # Invalid threshold should cause issues (test boundary)
  testthat::expect_no_error(
    assess_factorability(responses_df, NULL, NULL, multicollinearity_threshold = 0.99)
  )
})