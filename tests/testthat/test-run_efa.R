test_that("run_efa executes successfully with valid inputs", {
  responses_df <- get_responses_df()
  
  # Test with Military × 100% context
  result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,  # Reduced from 3 for stability
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
  expect_equal(result$extraction_info$nfactors, 2)
  
  # Test loadings dimensions
  expect_equal(ncol(result$loadings), 2)  # 2 factors
  expect_true(nrow(result$loadings) >= 2)  # At least 2 items
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
  expect_true(result$fit_interpretation$TLI %in% 
    c("Excellent", "Acceptable", "Poor", "Not available"))
})

test_that("run_efa handles different extraction methods", {
  responses_df <- get_responses_df()
  
  # Per psych::fa() documentation and Fabrigar et al. (1999),
  # different methods have different sample size requirements:
  # - minres: Most robust, works with small samples
  # - pa: Robust, works with small samples  
  # - ml: Requires N >= 200, function will warn and fall back to minres
  
  # Test 1: minres (should succeed silently)
  result_minres <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,
      fm = "minres"
    )
  })
  
  expect_equal(result_minres$extraction_info$method, "minres")
  expect_type(result_minres$loadings, "list")
  expect_true("fit_indices" %in% names(result_minres))
  
  # Test 2: pa (should succeed, may have warnings from psych package)
  result_pa <- suppressMessages({
    suppressWarnings({
      run_efa(
        responses_df,
        program_name = "Military",
        milestone_name = "100% (Corrected Final Design)",
        nfactors = 2,
        fm = "pa"
      )
    })
  })
  
  expect_equal(result_pa$extraction_info$method, "pa")
  expect_type(result_pa$loadings, "list")
  
  # Test 3: ml with small sample (should warn and fall back to minres)
  # Per psych::fa() documentation: "Maximum likelihood requires N >= 200"
  # Per Fabrigar et al. (1999): "Use minres or pa for samples under 200"
  expect_warning(
    result_ml <- suppressMessages({
      run_efa(
        responses_df,
        program_name = "Military",
        milestone_name = "100% (Corrected Final Design)",
        nfactors = 2,
        fm = "ml"
      )
    }),
    regexp = "Maximum likelihood.*requires N >= 200|Switching to minres",
    info = paste(
      "Maximum likelihood (ml) should warn when N < 200 and",
      "automatically switch to minres for robustness.",
      "See psych::fa() documentation and Fabrigar et al. (1999)."
    )
  )
  
  # Verify fallback actually happened
  expect_equal(result_ml$extraction_info$method, "minres")
  expect_type(result_ml$loadings, "list")
  
  # Verify the result is valid despite fallback
  expect_true("fit_indices" %in% names(result_ml))
  expect_true("loadings" %in% names(result_ml))
  expect_true("communalities" %in% names(result_ml))
})

test_that("run_efa handles different rotation methods", {
  responses_df <- get_responses_df()
  
  for (rotation in c("oblimin", "promax", "varimax", "none")) {
    result <- suppressMessages({
      suppressWarnings({
        run_efa(
          responses_df,
          program_name = "Military",
          milestone_name = "100% (Corrected Final Design)",
          nfactors = 2,
          fm = "minres",  # Use robust method for rotation tests
          rotate = rotation
        )
      })
    })
    
    expect_equal(result$extraction_info$rotation, rotation)
    
    # Factor correlations only for oblique rotations
    if (rotation %in% c("oblimin", "promax")) {
      expect_false(is.null(result$factor_correlations))
    }
  }
})

test_that("run_efa identifies problematic items", {
  responses_df <- get_responses_df()
  
  result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,
      fm = "minres"
    )
  })
  
  # Problematic items list should exist
  expect_true("problematic_items" %in% names(result))
  expect_true("low_communality" %in% names(result$problematic_items))
  expect_true("cross_loadings" %in% names(result$problematic_items))
})

test_that("run_efa loadings_clean suppresses small loadings", {
  responses_df <- get_responses_df()
  
  result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,
      fm = "minres",
      loading_threshold = 0.40
    )
  })
  
  # loadings_clean should have NAs for loadings < threshold
  loadings_clean <- result$loadings_clean
  loadings_all <- result$loadings
  
  expect_true(any(is.na(loadings_clean)))  # Some values should be suppressed
  expect_false(any(is.na(loadings_all)))   # Full matrix should have no NAs
})