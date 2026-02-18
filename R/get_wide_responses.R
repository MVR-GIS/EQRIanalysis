#' @title Prepare Wide Format Data for Reliability Analysis
#' @description Convert long-format responses to wide format suitable for 
#'   psychometric analysis. Each row represents one questionnaire event,
#'   columns represent question responses.
#' @param responses_df data.frame; Long-format responses from get_responses_df()
#' @param indicator_name character; REQUIRED. Must specify which indicator to analyze.
#'   One of: "Confidence", "Cost", "QA", "QC", "Schedule", "Scope", "Team"
#' @returns data.frame in wide format with QUESTIONNAIREEVENT_ID as rows,
#'   QUESTION_NUMBER as columns, RESPONSEVALUE as values
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>% filter select
#' 
get_wide_responses <- function(responses_df, indicator_name) {
  
  # CRITICAL: indicator_name is REQUIRED (not optional)
  if (missing(indicator_name) || is.null(indicator_name)) {
    stop("indicator_name is required. Must be one of: Confidence, Cost, QA, QC, Schedule, Scope, Team")
  }
  
  # Validate indicator
  valid_indicators <- c("Confidence", "Cost", "QA", "QC", 
                       "Schedule", "Scope", "Team")
  if (!indicator_name %in% valid_indicators) {
    stop(paste("indicator_name must be one of:", 
               paste(valid_indicators, collapse = ", ")))
  }
  
  # MUST filter to specific indicator to avoid duplicates
  responses_filtered <- responses_df %>%
    filter(INDICATOR == indicator_name)
  
  # Verify uniqueness after filtering
  dup_check <- responses_filtered %>%
    dplyr::group_by(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%
    dplyr::filter(n > 1L)
  
  if (nrow(dup_check) > 0) {
    warning(paste("Found", nrow(dup_check), 
                  "duplicate QUESTIONNAIREEVENT_ID + QUESTION_NUMBER combinations",
                  "even after filtering to indicator:", indicator_name,
                  "\nThis suggests data quality issues. Using first value."))
  }
  
  # Convert to wide format
  wide_df <- responses_filtered %>%
    select(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, RESPONSEVALUE) %>%
    pivot_wider(
      id_cols = QUESTIONNAIREEVENT_ID,
      names_from = QUESTION_NUMBER,
      values_from = RESPONSEVALUE,
      names_prefix = "Q",
      values_fn = first  # Take first value if duplicates remain (safety)
    )
  
  # Remove ID column for analysis (psych functions expect only item columns)
  wide_matrix <- wide_df %>%
    select(-QUESTIONNAIREEVENT_ID) %>%
    as.data.frame()
  
  return(wide_matrix)
}