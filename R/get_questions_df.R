#' @title Get Questions Data Frame
#' @description Gets a data frame of question numbers and question text. 
#' @returns data frame of questions
#' @export
#' @importFrom dplyr select arrange distinct mutate
#' @importFrom stringr str_to_title
get_questions_df <- function() {
  # Get responses
  responses_df <- get_responses_df()

  # Get df of questions
  questions_df <- responses_df %>%
    select(QUESTION_NUMBER, QUESTION_TEXT, QUESTION_SHORT) %>%
    arrange(QUESTION_NUMBER) %>%
    distinct(QUESTION_NUMBER, .keep_all = TRUE) %>%
    mutate(short_name = str_to_title(QUESTION_SHORT)) %>%
    mutate(short_name = factor(short_name))

  return(questions_df)
}
