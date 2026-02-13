testthat::test_that("get_alluvial_df returns a valid data frame", {
  alluvial_df <- get_alluvial_df()
  
  # Test 1: Returns a data frame
  testthat::expect_true(is.data.frame(alluvial_df))
  
  # Test 2: Has expected columns
  expected_cols <- c(
    "QUESTIONNAIREEVENT_ID", 
    "QUESTION_NUMBER", 
    "QUESTION_SHORT",
    "PROGRAMTYPE_NAME",
    "MILESTONE_DESC",
    "INDICATOR", 
    "RESPONSE", 
    "RESPONSEVALUE",
    "INDICATOR_SCORE_BIN",
    "indicator_value"
  )
  testthat::expect_true(
    all(expected_cols %in% colnames(alluvial_df))
  )
  
  # Test 3: INDICATOR_SCORE_BIN is a factor with 5 levels
  testthat::expect_true(is.factor(alluvial_df$INDICATOR_SCORE_BIN))
  testthat::expect_equal(
    nlevels(alluvial_df$INDICATOR_SCORE_BIN), 
    5
  )
  
  # Test 4: INDICATOR_SCORE_BIN has correct level labels
  expected_levels <- c(
    "Very Low (0.0-0.2)", 
    "Low (0.2-0.4)", 
    "Moderate (0.4-0.6)", 
    "High (0.6-0.8)", 
    "Very High (0.8-1.0)"
  )
  testthat::expect_equal(
    levels(alluvial_df$INDICATOR_SCORE_BIN),
    expected_levels
  )
  
  # Test 5: indicator_value is numeric and in range [0, 1]
  testthat::expect_true(is.numeric(alluvial_df$indicator_value))
  testthat::expect_true(
    all(alluvial_df$indicator_value >= 0 & alluvial_df$indicator_value <= 1)
  )
  
  # Test 6: No missing values in key columns
  testthat::expect_false(any(is.na(alluvial_df$QUESTIONNAIREEVENT_ID)))
  testthat::expect_false(any(is.na(alluvial_df$INDICATOR)))
  testthat::expect_false(any(is.na(alluvial_df$INDICATOR_SCORE_BIN)))
})

testthat::test_that("get_alluvial_df binning is correct", {
  alluvial_df <- get_alluvial_df()
  
  # Test 11: Verify bin boundaries are correct
  # Values in "Very Low" bin should be <= 0.2
  very_low_rows <- alluvial_df[
    alluvial_df$INDICATOR_SCORE_BIN == "Very Low (0.0-0.2)", 
  ]
  testthat::expect_true(
    all(very_low_rows$indicator_value <= 0.2)
  )
  
  # Values in "Very High" bin should be > 0.8
  very_high_rows <- alluvial_df[
    alluvial_df$INDICATOR_SCORE_BIN == "Very High (0.8-1.0)", 
  ]
  testthat::expect_true(
    all(very_high_rows$indicator_value > 0.8)
  )
  
  # Values in "Moderate" bin should be > 0.4 and <= 0.6
  moderate_rows <- alluvial_df[
    alluvial_df$INDICATOR_SCORE_BIN == "Moderate (0.4-0.6)", 
  ]
  testthat::expect_true(
    all(moderate_rows$indicator_value > 0.4 & moderate_rows$indicator_value <= 0.6)
  )
})

testthat::test_that("get_alluvial_df data integrity checks", {
  alluvial_df <- get_alluvial_df()
  
  # Test 14: All QUESTIONNAIREEVENTs should have matching indicator_value
  # Group by QUESTIONNAIREEVENT_ID and INDICATOR, check uniqueness
  unique_event_indicator <- alluvial_df %>%
    dplyr::select(QUESTIONNAIREEVENT_ID, INDICATOR, indicator_value, INDICATOR_SCORE_BIN) %>%
    dplyr::distinct()
  
  # Each QUESTIONNAIREEVENT_ID + INDICATOR combo should have one indicator_value
  event_counts <- unique_event_indicator %>%
    dplyr::group_by(QUESTIONNAIREEVENT_ID, INDICATOR) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  testthat::expect_true(all(event_counts$n == 1))
  
  # Test 15: RESPONSEVALUE should match the responses data
  responses_df <- get_responses_df()
  
  # Sample check: verify first row's RESPONSEVALUE matches
  first_row <- alluvial_df[1, ]
  matching_response <- responses_df %>%
    dplyr::filter(
      QUESTIONNAIREEVENT_ID == first_row$QUESTIONNAIREEVENT_ID,
      QUESTION_NUMBER == first_row$QUESTION_NUMBER
    )
  
  testthat::expect_equal(
    first_row$RESPONSEVALUE, 
    matching_response$RESPONSEVALUE[1]
  )
})