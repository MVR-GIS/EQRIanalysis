# Test for run_factor_number_analysis() batch function

test_that("run_factor_number_analysis processes multiple contexts", {
  responses_df <- get_responses_df()
  
  # Define test contexts
  test_contexts <- data.frame(
    program = c("Military", "Military"),
    milestone = c("100% (Corrected Final Design)", "35% (Concept Design)"),
    stringsAsFactors = FALSE
  )
  
  # Run batch analysis
  results <- suppressMessages({
    run_factor_number_analysis(
      responses_df,
      contexts = test_contexts,
      n.iter = 5,
      fm = "minres"
    )
  })
  
  # Test return structure
  expect_type(results, "list")
  expect_equal(length(results), 2)
  
  # Test each result
  for (i in 1:length(results)) {
    result <- results[[i]]
    expect_type(result, "list")
    expect_true("context" %in% names(result))
    expect_true("parallel_analysis" %in% names(result))
  }
})

test_that("run_factor_number_analysis requires correct context structure", {
  responses_df <- get_responses_df()
  
  # Missing 'milestone' column
  bad_contexts <- data.frame(
    program = c("Military"),
    wrong_col = c("100% (Corrected Final Design)")
  )
  
  expect_error(
    run_factor_number_analysis(responses_df, contexts = bad_contexts),
    regexp = "must have 'program' and 'milestone' columns"
  )
})

test_that("run_factor_number_analysis handles analysis failures gracefully", {
  responses_df <- get_responses_df()
  
  # Include valid and potentially problematic contexts
  test_contexts <- data.frame(
    program = c("Military", "Invalid Program"),
    milestone = c("100% (Corrected Final Design)", "Invalid Milestone"),
    stringsAsFactors = FALSE
  )
  
  # Should complete without error, with warnings for failed contexts
  expect_warning(
    results <- suppressMessages({
      run_factor_number_analysis(
        responses_df,
        contexts = test_contexts,
        n.iter = 5
      )
    }),
    regexp = "failed"
  )
  
  # Should return results for successful contexts
  expect_type(results, "list")
})

test_that("run_factor_number_analysis passes parameters correctly", {
  responses_df <- get_responses_df()
  
  test_contexts <- data.frame(
    program = "Military",
    milestone = "100% (Corrected Final Design)",
    stringsAsFactors = FALSE
  )
  
  # Test with custom parameters
  results <- suppressMessages({
    run_factor_number_analysis(
      responses_df,
      contexts = test_contexts,
      n.iter = 15,
      fm = "ml"
    )
  })
  
  result <- results[[1]]
  
  # Verify parameters were used
  expect_equal(result$parallel_analysis$n_iterations, 15)
})

test_that("run_factor_number_analysis names results correctly", {
  responses_df <- get_responses_df()
  
  test_contexts <- data.frame(
    program = c("Military", "Civil Works"),
    milestone = c("100% (Corrected Final Design)", "15% (Project Initiation)"),
    stringsAsFactors = FALSE
  )
  
  results <- suppressMessages({
    run_factor_number_analysis(
      responses_df,
      contexts = test_contexts,
      n.iter = 5
    )
  })
  
  # Check names format: "Program × Milestone"
  expected_names <- c(
    "Military × 100% (Corrected Final Design)",
    "Civil Works × 15% (Project Initiation)"
  )
  
  expect_equal(sort(names(results)), sort(expected_names))
})