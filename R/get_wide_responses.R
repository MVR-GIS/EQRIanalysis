#' @title Prepare Wide Format Data for Reliability Analysis
#' @description Convert long-format responses to wide format suitable for 
#'   psychometric analysis. Each row represents one questionnaire event,
#'   columns represent question responses.
#' @param responses_df data.frame; Long-format responses from get_responses_df()
#' @param indicator_name character; Optional. Filter to specific indicator.
#'   One of: "Confidence", "Cost", "QA", "QC", "Schedule", "Scope", "Team"
#' @returns data.frame in wide format with QUESTIONNAIREEVENT_ID as rows,
#'   QUESTION_NUMBER as columns, RESPONSEVALUE as values
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>% filter select
#' 
get_wide_responses <- function(responses_df, indicator_name = NULL) {
  
  # Filter to specific indicator if requested
  if (!is.null(indicator_name)) {
    responses_filtered <- responses_df %>%
      filter(INDICATOR == indicator_name)
  } else {
    responses_filtered <- responses_df
  }
  
  # Convert to wide format
  # Rows = questionnaire events, Columns = questions
  wide_df <- responses_filtered %>%
    select(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, RESPONSEVALUE) %>%
    pivot_wider(
      id_cols = QUESTIONNAIREEVENT_ID,
      names_from = QUESTION_NUMBER,
      values_from = RESPONSEVALUE,
      names_prefix = "Q"  # Prefix with "Q" to make valid R column names
    )
  
  # Remove ID column for analysis (psych functions expect only item columns)
  wide_matrix <- wide_df %>%
    select(-QUESTIONNAIREEVENT_ID) %>%
    as.data.frame()
  
  return(wide_matrix)
}