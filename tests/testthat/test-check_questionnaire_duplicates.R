test_that("check_questionnaire_duplicates", {
check_result <- check_questionnaire_duplicates(
  EQRIanalysis::get_responses_df(),
  "Military",
  "95% (Final Design)"
)

expect_true(check_result$all_match)
})
