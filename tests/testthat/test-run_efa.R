test_that("run_efa executes successfully with valid inputs", {
  responses_df <- get_responses_df()
  
  # Test with Military × 100% context
  result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 3,  # Specify to avoid parallel analysis
      fm = "minres",
      rotate = "oblimin"
    )
  })
  
  # Test structure
  expect_type(result, "list")
  expect_true("context" %in% names(result))
  expect_true("fit_indices" %in% names(result))
  expect_true("loadings" %in% names(result))
  expect_true("communalities" %in% names(result))
  
  # Test context
  expect_equal(result$context$program, "Military")
  expect_equal(result$context$milestone, "100% (Corrected Final Design)")
  
  # Test extraction info
  expect_equal(result$extraction_info$method, "minres")
  expect_equal(result$extraction_info$rotation, "oblimin")
  expect_equal(result$extraction_info$nfactors, 3)
  
  # Test loadings dimensions
  expect_equal(ncol(result$loadings), 3)  # 3 factors
  expect_true(nrow(result$loadings) >= 3)  # At least 3 items
})

test_that("run_efa uses parallel analysis when nfactors is NULL", {
  responses_df <- get_responses_df()
  
  result <- suppressMessages({
    suppressWarnings({
      run_efa(
        responses_df,
        program_name = "Military",
        milestone_name = "35% (Concept Design)",
        nfactors = NULL,  # Should trigger parallel analysis
        fm = "minres"
      )
    })
  })
  
  expect_type(result, "list")
  expect_true(result$extraction_info$nfactors > 0)
})

test_that("run_efa fit indices are calculated", {
  responses_df <- get_responses_df()
  
  result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2
    )
  })
  
  # Check fit indices exist
  expect_true("TLI" %in% names(result$fit_indices))
  expect_true("RMSEA" %in% names(result$fit_indices))
  expect_true("RMSR" %in% names(result$fit_indices))
  
  # Check fit interpretations
  expect_true("TLI" %in% names(result$fit_interpretation))
  expect_true(result$fit_interpretation$TLI %in% c("Excellent", "Acceptable", "Poor", "Not available"))
})

test_that("run_efa handles different extraction methods", {
  responses_df <- get_responses_df()
  
  for (method in c("minres", "ml", "pa")) {
    result <- suppressMessages({
      suppressWarnings({
        run_efa(
          responses_df,
          program_name = "Military",
          milestone_name = "100% (Corrected Final Design)",
          nfactors = 2,
          fm = method
        )
      })
    })
    
    expect_equal(result$extraction_info$method, method)
    expect_type(result$loadings, "list")
  }
})

test_that("run_efa handles different rotation methods", {
  responses_df <- get_responses_df()
  
  for (rotation in c("oblimin", "promax", "varimax")) {
    result <- suppressMessages({
      run_efa(
        responses_df,
        program_name = "Military",
        milestone_name = "100% (Corrected Final Design)",
        nfactors = 2,
        rotate = rotation
      )
    })
    
    expect_equal(result$extraction_info$rotation, rotation)
    
    # Factor correlations only for oblique rotations
    if (rotation %in% c("oblimin", "promax")) {
      expect_false(is.null(result$factor_correlations))
    }
  }
})