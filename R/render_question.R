#' @title Render Question
#' @description Render the HTML output for a question. 
#' @param questions_df     data.frame; A data frame of questions 
#'                         returned by `get_questions_df`.
#' @param responses_df     data.frame; A data frame of responses
#'                         returned by `get_responses_df`.
#' @param question_number  integer; The question number to be ploted.
#' @returns an htmltools R object that represents an HTML tag.
#' @export
#' @importFrom dplyr filter
#' @importFrom htmltools div h2
#' @importFrom shiny plotOutput
render_question <- function(questions_df, 
                            responses_df,
                            question_number) {
  # Extract the current question
  current_question <- questions_df %>%
    filter(QUESTION_NUMBER == question_number)

  # Generate the plot
  plot <- plot_question_by_type(questions_df, responses_df, 
                                current_question$QUESTION_NUMBER)
  
  # Return a div containing the question text and plot
  div(
    h2(paste(current_question$QUESTION_NUMBER, ":", 
             current_question$QUESTION_TEXT)),
    plotOutput(outputId = paste0("plot-", 
                            current_question$QUESTION_NUMBER)),
    print(plot)
  )
}