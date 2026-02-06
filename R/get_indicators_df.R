#' @title Get Indicators Data Frame
#' @description Get questionnaire events with the indicator metric scores 
#'   calculated. 
#' @returns a data frame of questionnaire events with indicator scores
#' @export
#' @importFrom dplyr %>% group_by summarize n across first 
get_indicators_df <- function() {
  responses_df <- get_responses_df()

  indicator_df <- responses_df %>%
    group_by(QUESTIONNAIREEVENT_ID, INDICATOR) %>%
    summarize(
      indicator_value = mean(RESPONSEVALUE),
      question_count = n(),
      across(
        c(PROJECT_ID, SUBPROJECT_ID, MILESTONE_ID:DIVISION),
        first
      ),
      .groups = 'drop'
    )

  return(indicator_df)
}