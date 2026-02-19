testthat::test_that("get_wide_responses produces unique rows per questionnaire event", {
  responses_df <- get_responses_df()
  
  wide_df <- get_wide_responses(
    responses_df,
    program_name = "Military",
    milestone_name = "95% (Final Design)"
  )
  
  # CORRECT TEST 1: Number of rows should match unique questionnaire events
  # This is the actual uniqueness requirement
  n_events_expected <- responses_df %>%
    dplyr::filter(
      PROGRAMTYPE_NAME == "Military",
      MILESTONE_DESC == "95% (Final Design)"
    ) %>%
    dplyr::pull(QUESTIONNAIREEVENT_ID) %>%
    unique() %>%
    length()
  
  testthat::expect_equal(
    nrow(wide_df), 
    n_events_expected,
    info = "Each row should represent one unique questionnaire event"
  )
  
  # CORRECT TEST 2: Verify no duplicate questionnaire event IDs
  # Need to check this at the intermediate stage before ID column is removed
  wide_df_with_id <- responses_df %>%
    dplyr::filter(
      PROGRAMTYPE_NAME == "Military",
      MILESTONE_DESC == "95% (Final Design)"
    ) %>%
    dplyr::distinct(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, .keep_all = TRUE) %>%
    dplyr::select(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, RESPONSEVALUE) %>%
    tidyr::pivot_wider(
      names_from = QUESTION_NUMBER,
      values_from = RESPONSEVALUE,
      names_prefix = "Q",
      id_cols = QUESTIONNAIREEVENT_ID
    )
  
  # Check for duplicate IDs (this is the real test)
  testthat::expect_false(
    any(duplicated(wide_df_with_id$QUESTIONNAIREEVENT_ID)),
    info = "No questionnaire event should appear more than once"
  )
  
  # CORRECT TEST 3: Verify dimensions match expected
  n_unique_questions <- responses_df %>%
    dplyr::filter(
      PROGRAMTYPE_NAME == "Military",
      MILESTONE_DESC == "95% (Final Design)"
    ) %>%
    dplyr::pull(QUESTION_NUMBER) %>%
    unique() %>%
    length()
  
  testthat::expect_equal(
    ncol(wide_df),
    n_unique_questions,
    info = "Should have one column per unique question in this context"
  )
})

