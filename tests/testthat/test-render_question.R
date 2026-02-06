testthat::test_that("render_question returns valid Markdown", {
  # Use the package functions to get the data
  questions_df <- get_questions_df()
  responses_df <- get_responses_df()
  
  # Select a question number to test
  question_number <- questions_df$QUESTION_NUMBER[1]
  
  # Call render_question
  markdown_output <- render_question(questions_df, responses_df, question_number)
  
  # Verify output
  testthat::expect_type(markdown_output, "character")
  testthat::expect_true(paste0("## Q ", question_number) == markdown_output[1])
  testthat::expect_true(questions_df$QUESTION_TEXT[questions_df$QUESTION_NUMBER == question_number] == markdown_output[3])
  testthat::expect_true(paste0("![](plots/questions/q_usace_", question_number, ".png)") == markdown_output[5])
  testthat::expect_true(paste0("![](plots/questions/q_type_", question_number, ".png)") == markdown_output[7])
})

testthat::test_that("render_question handles invalid question numbers gracefully", {
  # Use the package functions to get the data
  questions_df <- get_questions_df()
  responses_df <- get_responses_df()
  
  # Call render_question with an invalid question number
  invalid_question_number <- max(questions_df$QUESTION_NUMBER) + 1
  testthat::expect_error(
    render_question(questions_df, responses_df, invalid_question_number),
    "Question number not found in the questions dataset\\."
  )
})
