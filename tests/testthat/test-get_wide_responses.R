testthat::test_that("get_wide_responses requires indicator_name", {
  responses_df <- get_responses_df()
  
  # Should error if no indicator provided
  testthat::expect_error(
    get_wide_responses(responses_df),
    "indicator_name is required"
  )
})

testthat::test_that("get_wide_responses validates indicator_name", {
  responses_df <- get_responses_df()
  
  # Should error for invalid indicator
  testthat::expect_error(
    get_wide_responses(responses_df, "InvalidIndicator"),
    "indicator_name must be one of"
  )
})

testthat::test_that("get_wide_responses returns valid wide-format data", {
  responses_df <- get_responses_df()
  indicator_name = "Cost"
  
  # Test with valid indicator
  wide_df <- get_wide_responses(responses_df, indicator_name)
  
  # Test 1: Returns a data frame
  testthat::expect_true(is.data.frame(wide_df))
  
  # Test 2: Check dimensions - columns should be questions
  testthat::expect_true(all(grepl("^Q\\d+$", colnames(wide_df))))
  
  # Test 3: All values should be numeric (RESPONSEVALUE)
  testthat::expect_true(all(sapply(wide_df, is.numeric)))
  
  # Test 4: Number of rows should match unique questionnaire events
  # that have responses for this indicator
  n_events <- responses_df %>%
    dplyr::filter(INDICATOR == "Confidence") %>%
    dplyr::pull(QUESTIONNAIREEVENT_ID) %>%
    unique() %>%
    length()
  testthat::expect_equal(nrow(wide_df), n_events)
})

testthat::test_that("get_wide_responses handles all indicators", {
  responses_df <- get_responses_df()
  
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
    
    # No list columns (all should be atomic)
    list_cols <- sapply(wide_df, is.list)
    testthat::expect_false(
      any(list_cols),
      info = paste("List columns found in indicator:", ind)
    )
  }
})

testthat::test_that("get_wide_responses produces unique rows", {
  responses_df <- get_responses_df()
  
  wide_df <- get_wide_responses(responses_df, "Team")
  
  # Should have no duplicate rows (each row = unique questionnaire event)
  testthat::expect_equal(nrow(wide_df), nrow(unique(wide_df)))
})
