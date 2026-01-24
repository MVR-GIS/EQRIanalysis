testthat::test_that("check prender_question", {
  questions_df <- get_questions_df()
  responses_df <- get_responses_df()
  question_number <- 1
  html1 <- render_question(questions_df, responses_df, 
                           question_number)
  testthat::expect_true("shiny.tag" %in% class(html1))
})