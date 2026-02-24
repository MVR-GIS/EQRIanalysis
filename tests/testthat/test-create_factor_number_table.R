# Test for create_factor_number_table() summary function

test_that("create_factor_number_table formats results correctly", {
  responses_df <- get_responses_df()
  
  # Run analysis
  test_contexts <- data.frame(
    program = c("Military", "Military"),
    milestone = c("100% (Corrected Final Design)", "35% (Concept Design)"),
    stringsAsFactors = FALSE
  )
  
  results <- suppressMessages({
    run_factor_number_analysis(
      responses_df,
      contexts = test_contexts,
      n.iter = 5
    )
  })
  
  # Create table
  table <- create_factor_number_table(results)
  
  # Test structure
  expect_s3_class(table, "data.frame")
  expect_equal(nrow(table), 2)
  
  # Test required columns
  expect_true("Context" %in% names(table))
  expect_true("N" %in% names(table))
  expect_true("Items" %in% names(table))
  expect_true("Parallel Analysis" %in% names(table))
  
  # Test data types
  expect_type(table$Context, "character")
  expect_type(table$N, "integer")
  expect_type(table$Items, "integer")
})

test_that("create_factor_number_table handles NULL results", {
  # Create results with NULL entry
  results <- list(
    "Context 1" = NULL,
    "Context 2" = list(
      sample = list(n_observations = 50, n_items = 10),
      recommendations = list(
        parallel_analysis = 3,
        vss_complexity1 = 3,
        map_test = 2
      )
    )
  )
  
  table <- create_factor_number_table(results)
  
  # Should skip NULL and process valid result
  expect_s3_class(table, "data.frame")
  expect_equal(nrow(table), 1)
})

test_that("create_factor_number_table handles missing recommendations", {
  # Create result with incomplete recommendations
  results <- list(
    "Test Context" = list(
      sample = list(n_observations = 30, n_items = 8),
      recommendations = list(
        parallel_analysis = 2
        # Missing VSS and MAP
      )
    )
  )
  
  table <- create_factor_number_table(results)
  
  expect_s3_class(table, "data.frame")
  expect_equal(nrow(table), 1)
  expect_true(is.na(table$`VSS-1`))
  expect_true(is.na(table$`MAP Test`))
  expect_equal(table$`Parallel Analysis`, 2)
})

test_that("create_factor_number_table produces expected column names", {
  responses_df <- get_responses_df()
  
  test_contexts <- data.frame(
    program = "Military",
    milestone = "100% (Corrected Final Design)",
    stringsAsFactors = FALSE
  )
  
  results <- suppressMessages({
    run_factor_number_analysis(responses_df, test_contexts, n.iter = 5)
  })
  
  table <- create_factor_number_table(results)
  
  expected_cols <- c("Context", "N", "Items", "Parallel Analysis", "VSS-1", "MAP Test")
  expect_true(all(expected_cols %in% names(table)))
})

test_that("create_factor_number_table handles empty input", {
  results <- list()
  
  table <- create_factor_number_table(results)
  
  # Should return empty data frame with correct structure
  expect_s3_class(table, "data.frame")
  expect_equal(nrow(table), 0)
})

test_that("create_factor_number_table values are within reasonable ranges", {
  responses_df <- get_responses_df()
  
  test_contexts <- data.frame(
    program = c("Military", "Civil Works"),
    milestone = c("100% (Corrected Final Design)", "15% (Project Initiation)"),
    stringsAsFactors = FALSE
  )
  
  results <- suppressMessages({
    run_factor_number_analysis(responses_df, test_contexts, n.iter = 5)
  })
  
  table <- create_factor_number_table(results)
  
  # Sample sizes should be positive
  expect_true(all(table$N > 0))
  
  # Items should be >= 3 (minimum for factor analysis)
  expect_true(all(table$Items >= 3))
  
  # Parallel Analysis suggestions should be positive and < items
  pa_values <- table$`Parallel Analysis`[!is.na(table$`Parallel Analysis`)]
  expect_true(all(pa_values > 0))
  expect_true(all(pa_values < table$Items[!is.na(table$`Parallel Analysis`)]))
})