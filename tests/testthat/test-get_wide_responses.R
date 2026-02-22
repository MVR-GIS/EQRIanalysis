testthat::test_that("get_wide_responses supports aggregation", {
  responses_df <- get_responses_df()
  
  # Test aggregating across all contexts
  wide_all <- get_wide_responses(
    responses_df,
    program_name = NULL,
    milestone_name = NULL
  )
  
  # Should have more rows than single context
  wide_single <- get_wide_responses(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  testthat::expect_true(nrow(wide_all) > nrow(wide_single))
})

testthat::test_that("assess_factorability uses polychoric correlations", {
  responses_df <- get_responses_df()
  
  result <- assess_factorability(
    responses_df,
    program_name = NULL,  # Aggregate for sample size
    milestone_name = NULL
  )
  
  testthat::expect_equal(result$correlations$method, "polychoric")
  testthat::expect_true(is.matrix(result$correlations$matrix))
  testthat::expect_true(!is.null(result$correlations$tau))
})

testthat::test_that("run_efa_ordinal produces valid output", {
  responses_df <- get_responses_df()
  
  result <- run_efa_ordinal(
    responses_df,
    program_name = NULL,
    milestone_name = NULL,
    nfactors = 3,
    correct = 0.1
  )
  
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$analysis_parameters$correlation_method, "polychoric")
  testthat::expect_equal(result$analysis_parameters$nfactors, 3)
  testthat::expect_true(is.matrix(result$factor_loadings$loadings_matrix))
})