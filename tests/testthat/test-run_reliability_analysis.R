testthat::test_that("run_reliability_analysis works for all contexts", {
  responses_df <- get_responses_df()
  
  # Run analysis
  results <- run_reliability_analysis(responses_df, include_omega = TRUE)
  
  # Should return a data frame
  testthat::expect_true(is.data.frame(results))
  
  # Should have expected columns
  expected_cols <- c("program", "milestone", "n_observations", 
                    "alpha_std", "alpha_interpretation")
  testthat::expect_true(all(expected_cols %in% colnames(results)))
  
  # Should have at least one row per program type
  testthat::expect_true(nrow(results) >= 2)
  
  # Alpha values should be in valid range
  testthat::expect_true(all(results$alpha_std >= 0 & results$alpha_std <= 1))
})

testthat::test_that("run_reliability_analysis can skip omega", {
  responses_df <- get_responses_df()
  
  # Run without omega (faster)
  results <- run_reliability_analysis(responses_df, include_omega = FALSE)
  
  # Omega columns should be NA
  testthat::expect_true(all(is.na(results$omega_total)))
})
