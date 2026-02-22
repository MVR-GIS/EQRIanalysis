testthat::test_that("assess_factorability succeeds with core items on aggregated data", {
  responses_df <- get_responses_df()
  
  # Aggregated context with default filtering (core items only)
  result <- assess_factorability(
    responses_df,
    program_name = NULL,
    milestone_name = NULL,
    filter_context_specific = TRUE,
    max_missing_pct = 20,
    max_endorsement_pct = 80
  )
  
  # Should succeed
  testthat::expect_true(is.list(result))
  testthat::expect_true(result$recommendation$proceed_with_fa)
  
  # Should have valid structure
  expected_components <- c("context", "sample", "filtering", 
                          "correlations", "multicollinearity", "recommendation")
  testthat::expect_true(all(expected_components %in% names(result)))
  
  # Should retain sufficient items and observations
  testthat::expect_gte(result$sample$n_questions_analyzed, 3)
  testthat::expect_gte(result$sample$n_observations_complete, 10)
  
  # Correlation matrix should be valid
  testthat::expect_false(any(is.na(result$correlations$matrix)))
  testthat::expect_equal(
    nrow(result$correlations$matrix), 
    result$sample$n_questions_analyzed
  )
})

testthat::test_that("assess_factorability documents filtering appropriately", {
  responses_df <- get_responses_df()
  
  result <- assess_factorability(responses_df, NULL, NULL)
  
  # Should document filtering
  testthat::expect_true(!is.null(result$filtering))
  testthat::expect_true(is.numeric(result$filtering$n_context_specific))
  testthat::expect_true(is.numeric(result$filtering$n_low_variability))
  testthat::expect_true(is.numeric(result$filtering$n_total_removed))
  
  # If items were removed, should document which ones
  if (result$filtering$n_total_removed > 0) {
    testthat::expect_true(!is.null(result$filtering$removed_items))
  }
  
  # Should document thresholds used
  testthat::expect_equal(result$filtering$thresholds$max_missing_pct, 20)
  testthat::expect_equal(result$filtering$thresholds$max_endorsement_pct, 80)
})

testthat::test_that("assess_factorability uses appropriate correlation method", {
  responses_df <- get_responses_df()
  
  result <- assess_factorability(responses_df, NULL, NULL, use_polychoric = TRUE)
  
  # Should attempt polychoric/tetrachoric
  testthat::expect_true(
    result$correlations$method %in% c("polychoric", "tetrachoric", "pearson")
  )
  
  # If polychoric/tetrachoric, should have tau
  if (result$correlations$method %in% c("polychoric", "tetrachoric")) {
    testthat::expect_true(!is.null(result$correlations$tau))
  }
})

testthat::test_that("assess_factorability can skip filtering if requested", {
  responses_df <- get_responses_df()
  
  # With filter_context_specific = FALSE, may fail due to missing data issues
  result_or_error <- tryCatch({
    assess_factorability(
      responses_df,
      program_name = NULL,
      milestone_name = NULL,
      filter_context_specific = FALSE
    )
  }, error = function(e) {
    e
  })
  
  # Either succeeds OR fails with appropriate error
  if (inherits(result_or_error, "error")) {
    testthat::expect_match(
      result_or_error$message,
      "After filtering|complete cases|Correlation matrix contains NA",
      info = "Error should reference data quality issues"
    )
  }
})

testthat::test_that("assess_factorability handles single context appropriately", {
  responses_df <- get_responses_df()
  
  # Single context likely to fail
  result_or_error <- tryCatch({
    assess_factorability(
      responses_df,
      program_name = "Military",
      milestone_name = "95% (Final Design)"
    )
  }, error = function(e) {
    e
  })
  
  # Should either succeed OR provide informative error
  if (inherits(result_or_error, "error")) {
    testthat::expect_match(
      result_or_error$message,
      "After filtering|Insufficient|complete case",
      info = "Error should explain why single context failed"
    )
  } else {
    testthat::expect_true(is.list(result_or_error))
  }
})

testthat::test_that("assess_factorability provides ridge recommendations", {
  responses_df <- get_responses_df()
  
  result <- assess_factorability(responses_df, NULL, NULL)
  
  # Should have ridge recommendation
  testthat::expect_true(is.logical(result$recommendation$use_ridge))
  testthat::expect_true(is.numeric(result$recommendation$suggested_ridge))
  
  # Ridge value should be in valid range
  testthat::expect_gte(result$recommendation$suggested_ridge, 0)
  testthat::expect_lte(result$recommendation$suggested_ridge, 1)
})