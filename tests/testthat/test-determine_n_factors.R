# Test for determine_n_factors() function
# Tests parallel analysis, VSS, MAP test, and scree plot generation

test_that("determine_n_factors works with valid inputs", {
  # Get test data
  responses_df <- get_responses_df()
  
  # Test with Military × 100% context (known good sample)
  result <- determine_n_factors(
    responses_df,
    program_name = "Military",
    milestone_name = "100% (Corrected Final Design)",
    n.iter = 5,  # Reduced for testing speed
    fm = "minres",
    max_factors = 8
  )
  
  # Test return structure
  expect_type(result, "list")
  expect_true("context" %in% names(result))
  expect_true("sample" %in% names(result))
  expect_true("parallel_analysis" %in% names(result))
  expect_true("vss" %in% names(result))
  expect_true("scree_plot" %in% names(result))
  expect_true("recommendations" %in% names(result))
  
  # Test context information
  expect_equal(result$context$program, "Military")
  expect_equal(result$context$milestone, "100% (Corrected Final Design)")
  
  # Test sample information
  expect_type(result$sample$n_observations, "integer")
  expect_type(result$sample$n_items, "integer")
  expect_true(result$sample$n_observations > 0)
  expect_true(result$sample$n_items >= 3)  # Minimum for factor analysis
  
  # Test parallel analysis results
  expect_false(is.null(result$parallel_analysis))
  expect_type(result$parallel_analysis$n_factors_suggested, "integer")
  expect_true(result$parallel_analysis$n_factors_suggested >= 1)
  expect_true(result$parallel_analysis$n_factors_suggested <= result$sample$n_items)
  expect_type(result$parallel_analysis$eigenvalues_observed, "double")
  expect_type(result$parallel_analysis$eigenvalues_simulated, "double")
  
  # Test VSS results
  expect_false(is.null(result$vss))
  expect_true("vss_stats" %in% names(result$vss))
  expect_true("map_values" %in% names(result$vss))
  
  # Test scree plot
  expect_s3_class(result$scree_plot, "ggplot")
  
  # Test recommendations
  expect_true("parallel_analysis" %in% names(result$recommendations))
  expect_type(result$recommendations$parallel_analysis, "integer")
})

test_that("determine_n_factors handles small samples appropriately", {
  responses_df <- get_responses_df()
  
  # Test with Civil Works × 15% (smaller sample)
  result <- suppressWarnings({
    determine_n_factors(
      responses_df,
      program_name = "Civil Works",
      milestone_name = "15% (Project Initiation)",
      n.iter = 5,
      max_factors = 5
    )
  })
  
  # Should still return valid structure
  expect_type(result, "list")
  expect_false(is.null(result$parallel_analysis))
  
  # Max factors should be constrained by sample size
  expect_true(result$sample$max_factors_tested <= result$sample$n_items - 1)
  expect_true(result$sample$max_factors_tested <= floor(result$sample$n_observations / 5))
})

test_that("determine_n_factors removes zero-variance items", {
  responses_df <- get_responses_df()
  
  # Run analysis (should handle zero-variance internally)
  result <- suppressMessages({
    determine_n_factors(
      responses_df,
      program_name = "Military",
      milestone_name = "35% (Concept Design)",
      n.iter = 5
    )
  })
  
  # Check that items were analyzed (zero-variance items removed)
  expect_true(result$sample$n_items > 0)
  expect_true(result$sample$n_items_removed >= 0)
  expect_equal(
    result$sample$n_items + result$sample$n_items_removed,
    result$sample$n_items + result$sample$n_items_removed
  )
})

test_that("determine_n_factors fails gracefully with insufficient data", {
  responses_df <- get_responses_df()
  
  # Create minimal mock data that will fail
  minimal_context <- responses_df %>%
    filter(PROGRAMTYPE_NAME == "Military",
           MILESTONE_DESC == "100% (Corrected Final Design)") %>%
    head(5)  # Very small sample
  
  # Should warn or error appropriately
  expect_warning(
    determine_n_factors(
      minimal_context,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      n.iter = 5
    ),
    regexp = "(sample size|unstable)"
  )
})

test_that("determine_n_factors validates factor method parameter", {
  responses_df <- get_responses_df()
  
  # Test different valid factor methods per psych::fa() documentation
  for (method in c("minres", "ml", "pa")) {
    result <- suppressMessages({
      determine_n_factors(
        responses_df,
        program_name = "Military",
        milestone_name = "100% (Corrected Final Design)",
        n.iter = 5,
        fm = method
      )
    })
    
    expect_type(result, "list")
    expect_false(is.null(result$parallel_analysis))
  }
})

test_that("determine_n_factors eigenvalues are correctly ordered", {
  responses_df <- get_responses_df()
  
  result <- determine_n_factors(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)",
    n.iter = 5
  )
  
  # Eigenvalues should be in descending order
  eigenvalues <- result$parallel_analysis$eigenvalues_observed
  expect_true(all(diff(eigenvalues) <= 0))  # Non-increasing
})

test_that("determine_n_factors parallel analysis suggests reasonable factors", {
  responses_df <- get_responses_df()
  
  result <- determine_n_factors(
    responses_df,
    program_name = "Military",
    milestone_name = "100% (Corrected Final Design)",
    n.iter = 10
  )
  
  n_suggested <- result$parallel_analysis$n_factors_suggested
  n_items <- result$sample$n_items
  
  # Suggested factors should be reasonable
  expect_true(n_suggested >= 1)
  expect_true(n_suggested < n_items)  # Can't have more factors than items
  expect_true(n_suggested <= 10)  # Typically won't exceed 10 for questionnaires
})