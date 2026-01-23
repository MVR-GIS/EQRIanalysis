source(here::here("R/get_questions_df.R"))

testthat::test_that("check that get_questions_df works", {
  questions_df <- get_questions_df()
  testthat::expect_true(is.data.frame(questions_df))
  testthat::expect_equal(colnames(questions_df), c("QUESTION_NUMBER", "QUESTION_TEXT"))
})
