testthat::test_that("check plot_question_by_type", {
  questions_df <- get_questions_df()
  responses_df <- get_responses_df()
  question_number <- 31
  plot1 <- plot_question_by_type(questions_df, responses_df, 
                                 question_number)
  plot1
  testthat::expect_true("ggplot" %in% class(plot1))
})
