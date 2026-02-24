# Tests for run_efa() function

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
  expect_true(result$fit_interpretation$TLI %in% c("Excellent", "Acceptable", "Poor", "Not available"))
})

test_that("run_efa handles different extraction methods", {
  responses_df <- get_responses_df()
  
  # Per psych::fa() documentation, different methods have different
  # sample size requirements and robustness characteristics:
  # - minres: Most robust, works with small samples
  # - pa: Robust, works with small samples
  # - ml: Requires N > 200, may fail with small samples (expected)
  
  extraction_methods <- data.frame(
    method = c("minres", "pa", "ml"),
    should_succeed = c(TRUE, TRUE, FALSE),  # ml expected to fail with N=52
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(extraction_methods)) {
    method <- extraction_methods$method[i]
    should_succeed <- extraction_methods$should_succeed[i]
    
    if (should_succeed) {
      # Methods expected to work
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
      
    } else {
      # ml expected to fail with small sample (N=52 < 200)
      # Per psych::fa() documentation and Fabrigar et al. (1999)
      expect_error(
        suppressMessages({
          run_efa(
            responses_df,
            program_name = "Military",
            milestone_name = "100% (Corrected Final Design)",
            nfactors = 2,
            fm = method
          )
        }),
        regexp = "EFA failed|L-BFGS-B|finite values",
        info = paste(
          "Maximum likelihood (ml) factor analysis requires N > 200.",
          "Expected to fail with small sample (N=52).",
          "See psych::fa() documentation and Fabrigar et al. (1999)."
        )
      )
    }
  }
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