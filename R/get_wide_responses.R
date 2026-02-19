#' @title Prepare Wide Format Data for Reliability Analysis
#' @description Convert long-format responses to wide format suitable for 
#'   psychometric analysis. Each row represents one questionnaire event,
#'   columns represent question responses. Filters to a specific context
#'   (indicator × program type × milestone) to ensure homogeneous observations.
#' @param responses_df data.frame; Long-format responses from get_responses_df()
#' @param indicator_name character; REQUIRED. Indicator to analyze. One of:
#'   "Confidence", "Cost", "QA", "QC", "Schedule", "Scope", "Team"
#' @param program_name character; REQUIRED. Program type. One of:
#'   "Military", "Civil Works"
#' @param milestone_name character; REQUIRED. Project milestone. One of:
#'   "15% (Project Initiation)", "35% (Concept Design)", 
#'   "65% (Intermediate Design)", "95% (Final Design)",
#'   "100% (Corrected Final Design)"
#' @returns data.frame in wide format with one row per questionnaire event,
#'   one column per question (named Q1, Q2, etc.), values are RESPONSEVALUE
#' @export
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>% filter select mutate across
#' @importFrom forcats fct_drop
#' @references
#'   Based on the data preparation pattern from plot_indicator_alluvial().
#'   Filters by indicator, program type, and milestone to ensure homogeneous
#'   observations for reliability analysis per psychometric best practice.
#' 
get_wide_responses <- function(responses_df, 
                               indicator_name,
                               program_name,
                               milestone_name) {
  
  # Validate required parameters
  if (missing(indicator_name) || is.null(indicator_name)) {
    stop("indicator_name is required. Must be one of: Confidence, Cost, QA, QC, Schedule, Scope, Team")
  }
  if (missing(program_name) || is.null(program_name)) {
    stop("program_name is required. Must be one of: Military, Civil Works")
  }
  if (missing(milestone_name) || is.null(milestone_name)) {
    stop("milestone_name is required. Must be one of: 15% (Project Initiation), 35% (Concept Design), 65% (Intermediate Design), 95% (Final Design), 100% (Corrected Final Design)")
  }
  
  # Validate indicator
  valid_indicators <- c("Confidence", "Cost", "QA", "QC", 
                       "Schedule", "Scope", "Team")
  if (!indicator_name %in% valid_indicators) {
    stop(paste("indicator_name must be one of:", 
               paste(valid_indicators, collapse = ", ")))
  }
  
  # Validate program type
  valid_programs <- c("Military", "Civil Works")
  if (!program_name %in% valid_programs) {
    stop(paste("program_name must be one of:", 
               paste(valid_programs, collapse = ", ")))
  }
  
  # Validate milestone (partial match for flexibility)
  valid_milestones <- c("15% (Project Initiation)", 
                       "35% (Concept Design)", 
                       "65% (Intermediate Design)", 
                       "95% (Final Design)",
                       "100% (Corrected Final Design)")
  if (!milestone_name %in% valid_milestones) {
    stop(paste("milestone_name must be one of:", 
               paste(valid_milestones, collapse = ", ")))
  }
  
  # Filter to specific context (following plot_indicator_alluvial pattern)
  # This ensures homogeneous observations for reliability analysis
  responses_filtered <- responses_df %>%
    filter(
      INDICATOR == indicator_name,
      PROGRAMTYPE_NAME == program_name,
      MILESTONE_DESC == milestone_name
    ) %>%
    # Drop unused factor levels
    mutate(across(where(is.factor), fct_drop))
  
  # Check if we have sufficient data
  n_events <- length(unique(responses_filtered$QUESTIONNAIREEVENT_ID))
  if (n_events < 10) {
    warning(paste(
      "Only", n_events, "questionnaire events found for this combination.",
      "Reliability analysis requires at least 10 observations.",
      "Consider aggregating across program types or milestones."
    ))
  }
  
  # Verify uniqueness (should be guaranteed after 3-way filter)
  dup_check <- responses_filtered %>%
    dplyr::group_by(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%
    dplyr::filter(n > 1L)
  
  if (nrow(dup_check) > 0) {
    stop(paste(
      "Found", nrow(dup_check), 
      "duplicate QUESTIONNAIREEVENT_ID + QUESTION_NUMBER combinations",
      "even after filtering. This indicates a data quality issue.",
      "Please inspect the source data."
    ))
  }
  
  # Convert to wide format (following plot_indicator_alluvial pattern)
  # Use QUESTION_NUMBER instead of QUESTION_SHORT to maintain numeric mapping
  wide_df <- responses_filtered %>%
    select(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, RESPONSEVALUE) %>%
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
  
  return(wide_matrix)
}