#' @title Prepare Wide Format Data for Reliability Analysis
#' @description Convert long-format responses to wide format suitable for 
#'   psychometric analysis. Each row represents one questionnaire event,
#'   columns represent question responses. Filters to a specific questionnaire
#'   context (program type × milestone) where all questions are administered.
#' @param responses_df data.frame; Long-format responses from get_responses_df()
#' @param program_name character; REQUIRED. Program type. One of:
#'   "Military", "Civil Works"
#' @param milestone_name character; REQUIRED. Project milestone. One of:
#'   "15% (Project Initiation)", "35% (Concept Design)", 
#'   "65% (Intermediate Design)", "95% (Final Design)",
#'   "100% (Corrected Final Design)"
#' @returns data.frame in wide format with one row per questionnaire event,
#'   one column per question (named Q1, Q2, etc.), values are RESPONSEVALUE.
#'   This represents the complete questionnaire as administered in this context.
#' @export
#' @references
#'   Streiner, D. L., Norman, G. R., & Cairney, J. (2015). Health Measurement 
#'   Scales: A practical guide to their development and use (5th ed.). 
#'   Oxford University Press. Chapter 4: Reliability.
#'   
#'   Per psychometric best practice, reliability is assessed for the complete
#'   instrument (all questions) within a specific administration context,
#'   not for individual subscales (indicators) in isolation.
#' @section Development Notes:
#'   This function was developed with AI assistance (GitHub Copilot, 2026-02-20).
#'   Human direction and oversight was provided at each implementation step. 
#'   See `dev/sessions/2026-02-20.md` for complete development context.
#' 
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>% filter select mutate across distinct
#' @importFrom forcats fct_drop
#' 
get_wide_responses <- function(responses_df, 
                               program_name,
                               milestone_name) {
  
  # Validate required parameters
  if (missing(program_name) || is.null(program_name)) {
    stop("program_name is required. Must be one of: Military, Civil Works")
  }
  if (missing(milestone_name) || is.null(milestone_name)) {
    stop("milestone_name is required. Must be one of: 15% (Project Initiation), 35% (Concept Design), 65% (Intermediate Design), 95% (Final Design), 100% (Corrected Final Design)")
  }
  
  # Validate program type
  valid_programs <- c("Military", "Civil Works")
  if (!program_name %in% valid_programs) {
    stop(paste("program_name must be one of:", 
               paste(valid_programs, collapse = ", ")))
  }
  
  # Validate milestone
  valid_milestones <- c("15% (Project Initiation)", 
                       "35% (Concept Design)", 
                       "65% (Intermediate Design)", 
                       "95% (Final Design)",
                       "100% (Corrected Final Design)")
  if (!milestone_name %in% valid_milestones) {
    stop(paste("milestone_name must be one of:", 
               paste(valid_milestones, collapse = ", ")))
  }
  
  # Filter to specific questionnaire administration context
  # NOTE: We do NOT filter by INDICATOR - that's a post-hoc analytical construct
  # All questions are answered in each PROGRAMTYPE × MILESTONE context
  responses_filtered <- responses_df %>%
    filter(
      PROGRAMTYPE_NAME == program_name,
      MILESTONE_DESC == milestone_name
    ) %>%
    # Drop unused factor levels
    mutate(across(where(is.factor), fct_drop))
  
  # CRITICAL: Since questions can contribute to multiple indicators,
  # we need to get unique QUESTIONNAIREEVENT_ID × QUESTION_NUMBER combinations
  # Take the first RESPONSEVALUE if duplicates exist (should be identical)
  responses_unique <- responses_filtered %>%
    distinct(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, .keep_all = TRUE) %>%
    select(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, RESPONSEVALUE)
  
  # Check if we have sufficient data
  n_events <- length(unique(responses_unique$QUESTIONNAIREEVENT_ID))
  if (n_events < 10) {
    warning(paste(
      "Only", n_events, "questionnaire events found for",
      program_name, "at", milestone_name, "milestone.",
      "Reliability analysis requires at least 10 observations for stable estimates.",
      "Consider aggregating across milestones or program types if needed."
    ))
  }
  
  # Verify uniqueness after distinct() - should be no duplicates now
  dup_check <- responses_unique %>%
    dplyr::group_by(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%
    dplyr::filter(n > 1L)
  
  if (nrow(dup_check) > 0) {
    stop(paste(
      "Found", nrow(dup_check), 
      "duplicate QUESTIONNAIREEVENT_ID + QUESTION_NUMBER combinations",
      "even after using distinct(). This indicates a data quality issue.",
      "Please inspect the source data."
    ))
  }
  
  # Convert to wide format
  # Each row = one questionnaire event (complete questionnaire administration)
  # Each column = one question across all indicators
  wide_df <- responses_unique %>%
    pivot_wider(
      names_from = QUESTION_NUMBER,
      values_from = RESPONSEVALUE,
      names_prefix = "Q",
      id_cols = QUESTIONNAIREEVENT_ID
    )
  
  # Remove ID column (psych functions expect only item columns)
  wide_matrix <- wide_df %>%
    select(-QUESTIONNAIREEVENT_ID) %>%
    as.data.frame()
  
  # Verify all columns are numeric
  if (!all(sapply(wide_matrix, is.numeric))) {
    stop("Non-numeric columns detected. All item responses must be numeric for reliability analysis.")
  }
  
  # Report on data dimensions
  message(paste(
    "\nPrepared wide-format data for reliability analysis:",
    "\n  Context:", program_name, "×", milestone_name,
    "\n  Questionnaire events (n):", nrow(wide_matrix),
    "\n  Questions analyzed:", ncol(wide_matrix),
    "\n"
  ))
  
  return(wide_matrix)
}