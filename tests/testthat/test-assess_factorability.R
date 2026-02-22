testthat::test_that("assess_factorability handles near-zero variance items", {
  responses_df <- get_responses_df()
  
  # Aggregated context may expose near-zero variance items
  # that pass simple var > 0 check but fail cor() computation
  result <- assess_factorability(
    responses_df,
    program_name = NULL,
    milestone_name = NULL
  )
  
  # Should succeed (after removing problematic items)
  testthat::expect_true(is.list(result))
  
  # If items were removed due to near-zero variance
  if (result$sample$n_questions_removed_zero_var > 0) {
    message(paste(
      "Note:", result$sample$n_questions_removed_zero_var, 
      "items removed due to zero/near-zero variance in aggregated context"
    ))
  }
  
  # Correlation matrix should have NO NAs
  testthat::expect_false(
    any(is.na(result$correlations$matrix)),
    info = "After robust variance checking, correlation matrix should not contain NAs"
  )
})

testthat::test_that("diagnose_variance_issues identifies problematic items", {
  responses_df <- get_responses_df()
  wide_data <- get_wide_responses(responses_df, NULL, NULL)
  
  result <- diagnose_variance_issues(wide_data)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true(is.data.frame(result$all_items))
  testthat::expect_true(is.numeric(result$n_problematic))
  
  # If problematic items found, they should have clear reasons
  if (result$n_problematic > 0) {
    testthat::expect_true(nrow(result$problematic_items) == result$n_problematic)
    testthat::expect_true(all(!is.na(result$problematic_items$reason)))
  }
})

testthat::test_that("assess_factorability accepts NULL for aggregation", {
  responses_df <- get_responses_df()
  
  # Should accept NULL for both parameters (aggregation support)
  testthat::expect_no_error(
    results <- assess_factorability(responses_df, program_name = NULL, milestone_name = NULL)
  )
  
  # Should also work with specific values
  testthat::expect_no_error(
    results <- assess_factorability(responses_df, "Military", "95% (Final Design)")
  )
})

testthat::test_that("assess_factorability returns valid structure", {
  responses_df <- get_responses_df()
  
  # Test with aggregated data for maximum sample size
  result <- assess_factorability(
    responses_df,
    program_name = NULL,
    milestone_name = NULL
  )
  
  # Should return list
  testthat::expect_true(is.list(result))
  
  # Should have expected top-level components
  expected_components <- c("context", "sample", "correlations", 
                          "multicollinearity", "recommendation")
  testthat::expect_true(
    all(expected_components %in% names(result)),
    info = paste("Missing components:", 
                paste(setdiff(expected_components, names(result)), collapse = ", "))
  )
  
  # Context validation
  testthat::expect_equal(result$context$program, "All")
  testthat::expect_equal(result$context$milestone, "All")
  testthat::expect_true(!is.null(result$context$label))
  
  # Sample info validation
  testthat::expect_true(is.numeric(result$sample$n_questions_original))
  testthat::expect_true(is.numeric(result$sample$n_questions_analyzed))
  testthat::expect_true(result$sample$n_questions_analyzed <= result$sample$n_questions_original)
  
  # Correlations validation
  testthat::expect_true(result$correlations$method %in% c("polychoric", "pearson"))
  testthat::expect_true(is.matrix(result$correlations$matrix))
  testthat::expect_false(any(is.na(result$correlations$matrix)), 
                         info = "Correlation matrix should not contain NAs")
  testthat::expect_true(is.numeric(result$correlations$determinant))
  testthat::expect_false(is.na(result$correlations$determinant),
                         info = "Determinant should not be NA")
  
  # Correlation matrix properties
  cor_mat <- result$correlations$matrix
  testthat::expect_equal(cor_mat, t(cor_mat), info = "Matrix should be symmetric")
  testthat::expect_true(all(abs(diag(cor_mat) - 1) < 1e-10), 
                       info = "Diagonal should be 1")
  
  # Multicollinearity diagnostics
  testthat::expect_true(is.numeric(result$multicollinearity$n_pairs_high))
  testthat::expect_true(is.numeric(result$multicollinearity$threshold))
  
  # Recommendation validation
  testthat::expect_true(is.logical(result$recommendation$proceed_with_fa))
  testthat::expect_true(is.logical(result$recommendation$use_ridge))
  testthat::expect_true(is.numeric(result$recommendation$suggested_ridge))
  testthat::expect_true(result$recommendation$suggested_ridge >= 0 && 
                       result$recommendation$suggested_ridge <= 1)
})

testthat::test_that("assess_factorability handles polychoric/Pearson fallback", {
  responses_df <- get_responses_df()
  
  # Single context likely triggers Pearson fallback due to small N
  result_small <- assess_factorability(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)",
    use_polychoric = TRUE  # Request polychoric
  )
  
  # Should succeed (fallback to Pearson if polychoric fails)
  testthat::expect_true(is.list(result_small))
  testthat::expect_true(result_small$correlations$method %in% c("polychoric", "pearson"))
  
  # If Pearson was used, should have notes
  if (result_small$correlations$method == "pearson") {
    testthat::expect_true(!is.null(result_small$recommendation$notes))
    testthat::expect_null(result_small$correlations$tau)
  }
  
  # If polychoric was used, should have tau
  if (result_small$correlations$method == "polychoric") {
    testthat::expect_true(!is.null(result_small$correlations$tau))
  }
})

testthat::test_that("assess_factorability can skip polychoric", {
  responses_df <- get_responses_df()
  
  # Explicitly request Pearson only
  result_pearson <- assess_factorability(
    responses_df,
    program_name = NULL,
    milestone_name = NULL,
    use_polychoric = FALSE
  )
  
  testthat::expect_equal(result_pearson$correlations$method, "pearson")
  testthat::expect_null(result_pearson$correlations$tau)
})

testthat::test_that("assess_factorability removes zero-variance items", {
  responses_df <- get_responses_df()
  
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
  }
  
  # Should always have at least 3 items (or would error)
  testthat::expect_true(result$sample$n_questions_analyzed >= 3)
})

testthat::test_that("assess_factorability detects multicollinearity", {
  responses_df <- get_responses_df()
  
  result <- assess_factorability(
    responses_df,
    program_name = NULL,
    milestone_name = NULL,
    multicollinearity_threshold = 0.90
  )
  
  # If high correlations detected
  if (result$multicollinearity$n_pairs_high > 0) {
    testthat::expect_true(result$recommendation$use_ridge)
    testthat::expect_true(!is.null(result$multicollinearity$high_cor_pairs))
    testthat::expect_equal(
      nrow(result$multicollinearity$high_cor_pairs),
      result$multicollinearity$n_pairs_high
    )
  }
})

testthat::test_that("assess_factorability validates parameters", {
  responses_df <- get_responses_df()
  
  # Invalid program
  testthat::expect_error(
    assess_factorability(responses_df, program_name = "Invalid", milestone_name = NULL),
    "program_name must be one of"
  )
  
  # Invalid milestone
  testthat::expect_error(
    assess_factorability(responses_df, program_name = NULL, milestone_name = "Invalid"),
    "milestone_name must be one of"
  )
})

testthat::test_that("assess_factorability handles contexts appropriately", {
  responses_df <- get_responses_df()
  
  # TEST 1: Small single context SHOULD fail (expected behavior)
  testthat::expect_error(
    result_context <- assess_factorability(
      responses_df,
      program_name = "Military",
      milestone_name = "95% (Final Design)"
    ),
    class = "simpleError"
  )
  
  # TEST 2: Aggregated context SHOULD succeed
  result_aggregated <- assess_factorability(
    responses_df,
    program_name = NULL,
    milestone_name = NULL
  )
  
  testthat::expect_true(is.list(result_aggregated))
  testthat::expect_gt(result_aggregated$sample$n_questions_analyzed, 3)
})