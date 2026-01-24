#' @title Get Questions Data Frame
#' @description Gets a data frame of question numbers and question text. 
#' @returns data frame of questions
#' @export
#' @importFrom here here
#' @importFrom dplyr select arrange distinct
get_questions_df <- function() {
  # Get responses
  responses <- EQRIanalysis::responses

  # Get df of questions
  questions_df <- responses %>%
    dplyr::select(QUESTION_NUMBER, QUESTION_TEXT) %>%
    dplyr::arrange(QUESTION_NUMBER) %>%
    dplyr::distinct(QUESTION_NUMBER, .keep_all = TRUE)
  
  return(questions_df)
}
