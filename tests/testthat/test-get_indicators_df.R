testthat::test_that("check that get_responses_df works", {
  indicators_df <- get_indicators_df()
  testthat::expect_true(is.data.frame(indicators_df))
  testthat::expect_true(
    any(
      c("QUESTIONNAIREEVENT_ID", "INDICATOR") %in% 
      colnames(indicators_df)))
})
