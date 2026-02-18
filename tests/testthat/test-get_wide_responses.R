# tests/testthat/test-get_wide_responses.R

testthat::test_that("get_wide_responses returns valid wide-format data", {
  # Get test data
  responses_df <- get_responses_df()
  
  # Test 1: Returns a data frame
  wide_df <- get_wide_responses(responses_df)
  testthat::expect_true(is.data.frame(wide_df))
  
  # Test 2: Check dimensions - columns should be questions
  # Each column name should start with "Q" followed by a number
  testthat::expect_true(all(grepl("^Q\\d+$", colnames(wide_df))))
  
  # Test 3: All values should be numeric (RESPONSEVALUE)
  testthat::expect_true(all(sapply(wide_df, is.numeric)))
  
  # Test 4: Number of rows should match unique questionnaire events
  n_events <- length(unique(responses_df$QUESTIONNAIREEVENT_ID))
  testthat::expect_equal(nrow(wide_df), n_events)
})

testthat::test_that("get_wide_responses filters by indicator correctly", {
  responses_df <- get_responses_df()
  
  # Test filtering for a specific indicator
  indicator_name <- "Confidence"
  wide_df_filtered <- get_wide_responses(responses_df, indicator_name)
  
  # Test 1: Should return a data frame
  testthat::expect_true(is.data.frame(wide_df_filtered))
  
  # Test 2: Should have fewer columns than unfiltered version
  wide_df_all <- get_wide_responses(responses_df)
  testthat::expect_true(ncol(wide_df_filtered) <= ncol(wide_df_all))
  
  # Test 3: All column names should start with Q
  testthat::expect_true(all(grepl("^Q\\d+$", colnames(wide_df_filtered))))
  
  # Test 4: Verify columns match questions for that indicator
  expected_questions <- responses_df %>%
    dplyr::filter(INDICATOR == indicator_name) %>%
    dplyr::pull(QUESTION_NUMBER) %>%
    unique() %>%
    sort()
  
  actual_questions <- as.numeric(gsub("^Q", "", colnames(wide_df_filtered)))
  
  testthat::expect_equal(sort(actual_questions), sort(expected_questions))
})

testthat::test_that("get_wide_responses handles all indicators", {
  responses_df <- get_responses_df()
  
  # Test all 7 indicators
  indicators <- c("Confidence", "Cost", "QA", "QC", "Schedule", "Scope", "Team")
  
  for (ind in indicators) {
    wide_df <- get_wide_responses(responses_df, ind)
    
    # Each should return a valid data frame
    testthat::expect_true(
      is.data.frame(wide_df),
      info = paste("Failed for indicator:", ind)
    )
    
    # Each should have at least 1 column (question)
    testthat::expect_true(
      ncol(wide_df) >= 1,
      info = paste("Failed for indicator:", ind)
    )
  }
})

testthat::test_that("get_wide_responses preserves response values", {
  responses_df <- get_responses_df()
  
  # Get wide format
  wide_df <- get_wide_responses(responses_df, "Confidence")
  
  # Test 1: Values should be within valid range
  # Assuming RESPONSEVALUE is 0-1 based on your indicator calculations
  all_values <- unlist(wide_df, use.names = FALSE)
  all_values <- all_values[!is.na(all_values)]
  
  testthat::expect_true(all(all_values >= 0))
  testthat::expect_true(all(all_values <= 1))
})

testthat::test_that("get_wide_responses handles missing data appropriately", {
  responses_df <- get_responses_df()
  
  # Get wide format for an indicator
  wide_df <- get_wide_responses(responses_df, "Team")
  
  # Test: Missing values should be NA (not removed)
  # This tests that pivot_wider handles incomplete responses correctly
  testthat::expect_true(is.data.frame(wide_df))
  
  # Test: At least some complete cases should exist
  complete_cases <- complete.cases(wide_df)
  testthat::expect_true(sum(complete_cases) > 0)
})

testthat::test_that("get_wide_responses column naming is consistent", {
  responses_df <- get_responses_df()
  
  # Get wide format
  wide_df <- get_wide_responses(responses_df)
  
  # Test 1: Column names should be in format Q1, Q2, Q3, etc.
  col_pattern <- "^Q\\d+$"
  testthat::expect_true(all(grepl(col_pattern, colnames(wide_df))))
  
  # Test 2: Extract question numbers and verify they're numeric
  question_nums <- as.numeric(gsub("^Q", "", colnames(wide_df)))
  testthat::expect_true(all(!is.na(question_nums)))
  testthat::expect_true(all(question_nums > 0))
})

testthat::test_that("get_wide_responses handles invalid indicator gracefully", {
  responses_df <- get_responses_df()
  
  # Test with non-existent indicator
  # This should return a data frame with 0 columns (just ID removed)
  # or handle it appropriately based on your implementation
  invalid_wide <- get_wide_responses(responses_df, "InvalidIndicator")
  
  # Should still return a data frame (even if empty)
  testthat::expect_true(is.data.frame(invalid_wide))
  
  # Should have 0 columns since no questions match
  testthat::expect_equal(ncol(invalid_wide), 0)
})