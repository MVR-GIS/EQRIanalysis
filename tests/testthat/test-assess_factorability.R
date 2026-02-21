testthat::test_that("assess_factorability requires program and milestone", {
  responses_df <- get_responses_df()
  
  testthat::expect_error(
    assess_factorability(responses_df),
    "program_name is required"
  )
  
  testthat::expect_error(
    assess_factorability(responses_df, "Military"),
    "milestone_name is required"
  )
})

testthat::test_that("assess_factorability returns valid structure", {
  responses_df <- get_responses_df()
  program_name = "Military"
  milestone_name = "95% (Final Design)"
  
  result <- assess_factorability(
    responses_df,
    program_name,
    milestone_name
  )
  
  # Should return list
  testthat::expect_true(is.list(result))
  
  # Should have expected components
  expected_components <- c("context", "sample", "kmo", "bartlett", "recommendation")
  testthat::expect_true(all(expected_components %in% names(result)))
  
  # KMO should be between 0 and 1
  testthat::expect_true(result$kmo$overall_msa >= 0 && result$kmo$overall_msa <= 1)
  
  # Bartlett should have p-value
  testthat::expect_true(!is.na(result$bartlett$p_value))
})

testthat::test_that("run_parallel_analysis suggests reasonable number of factors", {
  responses_df <- get_responses_df()
  
  result <- run_parallel_analysis(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # Should suggest at least 1 factor
  testthat::expect_true(result$recommendation$n_factors >= 1)
  
  # Should not suggest more factors than items
  testthat::expect_true(result$recommendation$n_factors <= result$sample$n_questions)
})

testthat::test_that("run_efa produces interpretable results", {
  responses_df <- get_responses_df()
  
  # Test with specified number of factors
  result <- run_efa(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)",
    nfactors = 3
  )
  
  # Should have loadings matrix
  testthat::expect_true(is.matrix(result$factor_loadings$loadings_matrix))
  
  # Number of factors should match request
  testthat::expect_equal(ncol(result$factor_loadings$loadings_matrix), 3)
  
  # Should have fit indices
  testthat::expect_true(!is.null(result$fit_indices$tli))
})