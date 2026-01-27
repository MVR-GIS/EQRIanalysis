testthat::test_that("check prender_question", {
  questions_df <- get_questions_df()
  responses_df <- get_responses_df()
  question_number <- 1
  html1 <- render_question(questions_df, responses_df, 
                           question_number)
  htmltools::browsable(html1)
  testthat::expect_true("shiny.tag" %in% class(html1))
})

testthat::test_that("check prender_question", {
  questions_df <- get_questions_df()
  responses_df <- get_responses_df()
  question_number <- 2
  html2 <- render_question(questions_df, responses_df, 
                           question_number)
  htmltools::browsable(html2)
  testthat::expect_true("shiny.tag" %in% class(html1))
})