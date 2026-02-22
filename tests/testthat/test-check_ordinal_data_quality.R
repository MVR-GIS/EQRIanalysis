testthat::test_that("check_ordinal_data_quality returns valid structure", {
  responses_df <- get_responses_df()
  wide_data <- get_wide_responses(responses_df, NULL, NULL)
  
  result <- check_ordinal_data_quality(wide_data)
  
  testthat::expect_true(is.list(result))
  testthat::expect_true(is.data.frame(result$item_diagnostics))
  testthat::expect_true(is.logical(result$overall_sufficient))
  testthat::expect_true(is.character(result$recommendation))
  
  # Item diagnostics should have required columns
  expected_cols <- c("item", "n_categories", "min_cell_count", "required_n", "overall_sufficient")
  testthat::expect_true(all(expected_cols %in% names(result$item_diagnostics)))
  
  # Should have one row per item
  testthat::expect_equal(nrow(result$item_diagnostics), ncol(wide_data))
})

testthat::test_that("check_ordinal_data_quality applies Flora & Curran criteria", {
  responses_df <- get_responses_df()
  wide_data <- get_wide_responses(responses_df, NULL, NULL)
  
  result <- check_ordinal_data_quality(wide_data)
  
  # Required N should match Flora & Curran (2004) criteria
  for (i in 1:nrow(result$item_diagnostics)) {
    n_cat <- result$item_diagnostics$n_categories[i]
    required <- result$item_diagnostics$required_n[i]
    
    expected_required <- dplyr::case_when(
      n_cat <= 2 ~ 500,
      n_cat == 3 ~ 400,
      n_cat == 4 ~ 300,
      n_cat >= 5 ~ 200,
      TRUE ~ 300
    )
    
    testthat::expect_equal(required, expected_required)
  }
})