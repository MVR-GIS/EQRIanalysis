#' @title Summarize Questions by Indicator
#' @description Create a summary table showing which questions belong to each indicator.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @returns data.frame with indicator names and their associated questions
#' @export
#' 
#' @importFrom dplyr %>% group_by summarize n_distinct
#' @importFrom tidyr nest unnest
#' 
summarize_indicator_questions <- function(responses_df) {
  
  indicator_summary <- responses_df %>%
    group_by(INDICATOR) %>%
    summarize(
      n_questions = n_distinct(QUESTION_NUMBER),
      n_responses = n(),
      questions = paste(sort(unique(QUESTION_NUMBER)), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(INDICATOR)
  
  return(indicator_summary)
}