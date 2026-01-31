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
#' @importFrom ggplot2 ggsave
#' @importFrom base64enc dataURI
#' @importFrom htmltools div h2

render_question <- function(questions_df, 
                            responses_df,
                            question_number) {
  # Extract the current question
  current_question <- questions_df %>%
    filter(QUESTION_NUMBER == question_number)

  if (nrow(current_question) == 0) {
    stop("Question number not found in the questions dataset.")
  }

  # Generate the plot
  plot_file <- paste0("plots/questions/q_type_", question_number, ".png")
  ggsave(
    filename = plot_file, 
    plot = plot_question_by_type(questions_df, responses_df, 
                                 current_question$QUESTION_NUMBER),
    width = 7,
    height = 4,
    units = "in",
    dpi = 96,
    create.dir = TRUE
    )
  
  # Create the Markdown string
  markdown <- c(
    paste("## Q", question_number),
    "",
    current_question$QUESTION_TEXT,
    "",
    paste0("![](", plot_file, ")"),
    ""
  )
  
  return(markdown)
}