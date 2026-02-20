#' @title Check for Duplicate Questionnaire Events
#' @description Diagnostic function to verify no duplicate questionnaire events
#'   exist in a specific context after data transformation
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param program_name character; Program type
#' @param milestone_name character; Milestone
#' @returns list with diagnostic information
#' @export
#' 
#' @section Development Notes:
#'   This function was developed with AI assistance (GitHub Copilot, 2026-02-20).
#'   Human direction and oversight was provided at each implementation step. 
#'   See `dev/sessions/2026-02-20.md` for complete development context.
#' 
check_questionnaire_duplicates <- function(responses_df, 
                                          program_name, 
                                          milestone_name) {
  
  # Filter to context
  filtered <- responses_df %>%
    filter(
      PROGRAMTYPE_NAME == program_name,
      MILESTONE_DESC == milestone_name
    )
  
  # Check unique events
  n_unique_events <- length(unique(filtered$QUESTIONNAIREEVENT_ID))
  
  # Check after distinct
  distinct_data <- filtered %>%
    distinct(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, .keep_all = TRUE)
  
  n_events_after_distinct <- length(unique(distinct_data$QUESTIONNAIREEVENT_ID))
  
  # Pivot to wide
  wide_with_id <- distinct_data %>%
    select(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, RESPONSEVALUE) %>%
    pivot_wider(
      names_from = QUESTION_NUMBER,
      values_from = RESPONSEVALUE,
      names_prefix = "Q",
      id_cols = QUESTIONNAIREEVENT_ID
    )
  
  # Check for duplicate IDs
  has_dup_ids <- any(duplicated(wide_with_id$QUESTIONNAIREEVENT_ID))
  
  # Results
  list(
    context = paste(program_name, "×", milestone_name),
    n_unique_events = n_unique_events,
    n_events_after_distinct = n_events_after_distinct,
    n_rows_in_wide = nrow(wide_with_id),
    has_duplicate_ids = has_dup_ids,
    all_match = (n_unique_events == n_events_after_distinct) && 
                (n_events_after_distinct == nrow(wide_with_id)) &&
                !has_dup_ids
  )
}