#' @title Prepare Wide Format Data for Reliability Analysis
#' @description Convert long-format responses to wide format suitable for 
#'   psychometric analysis. Each row represents one questionnaire event,
#'   columns represent question responses. Can filter to specific context or
#'   aggregate across contexts for factor analysis.
#' @param responses_df data.frame; Long-format responses from get_responses_df()
#' @param program_name character; Program type. One of: "Military", "Civil Works",
#'   or NULL to aggregate across all programs
#' @param milestone_name character; Project milestone. One of:
#'   "15% (Project Initiation)", "35% (Concept Design)", 
#'   "65% (Intermediate Design)", "95% (Final Design)",
#'   "100% (Corrected Final Design)", or NULL to aggregate across all milestones
#' @returns data.frame in wide format with one row per questionnaire event,
#'   one column per question (named Q1, Q2, etc.), values are RESPONSEVALUE
#' @export
#' @references
#'   Streiner, D. L., Norman, G. R., & Cairney, J. (2015). Health Measurement 
#'   Scales: A practical guide to their development and use (5th ed.). 
#'   Oxford University Press. Chapter 4: Reliability.
#'   
#'   Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of 
#'   alternative methods of estimation for confirmatory factor analysis 
#'   with ordinal data. Psychological Methods, 9(4), 466-491.
#'   https://doi.org/10.1037/1082-989X.9.4.466
#'   
#'   Per Flora & Curran, factor analysis of ordinal data requires minimum
#'   N = 200 for stable polychoric correlations. Aggregation across contexts
#'   may be necessary to achieve adequate sample size.
#' @section Development Notes:
#'   This function was updated 2026-02-21 to support context aggregation
#'   for factor analysis with adequate sample sizes. Parameters now accept
#'   NULL to aggregate across program types and/or milestones.
#'   See `dev/sessions/2026-02-21.md` for complete development context.
#' 
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>% filter select mutate across distinct
#' @importFrom forcats fct_drop
#' 
get_wide_responses <- function(responses_df, 
                               program_name = NULL,
                               milestone_name = NULL) {
  
  # Define valid values for validation
  valid_programs <- c("Military", "Civil Works")
  valid_milestones <- c(
    "15% (Project Initiation)", 
    "35% (Concept Design)", 
    "65% (Intermediate Design)", 
    "95% (Final Design)",
    "100% (Corrected Final Design)"
  )
  
  # Start with full dataset
  responses_filtered <- responses_df
  
  # Filter by program if specified
  if (!is.null(program_name)) {
    # Validate
    if (!program_name %in% valid_programs) {
      stop(paste(
        "program_name must be one of:", 
        paste(valid_programs, collapse = ", "),
        "or NULL to aggregate across all programs"
      ))
    }
    # Apply filter
    responses_filtered <- responses_filtered %>%
      filter(PROGRAMTYPE_NAME == program_name)
    
    message(paste("Filtering to program type:", program_name))
  } else {
    message("Aggregating across ALL program types (Military & Civil Works)")
  }
  
  # Filter by milestone if specified
  if (!is.null(milestone_name)) {
    # Validate
    if (!milestone_name %in% valid_milestones) {
      stop(paste(
        "milestone_name must be one of:", 
        paste(valid_milestones, collapse = ", "),
        "or NULL to aggregate across all milestones"
      ))
    }
    # Apply filter
    responses_filtered <- responses_filtered %>%
      filter(MILESTONE_DESC == milestone_name)
    
    message(paste("Filtering to milestone:", milestone_name))
  } else {
    message("Aggregating across ALL milestones")
  }
  
  # Drop unused factor levels
  responses_filtered <- responses_filtered %>%
    mutate(across(where(is.factor), fct_drop))
  
  # CRITICAL: Get unique QUESTIONNAIREEVENT_ID × QUESTION_NUMBER combinations
  # Per psychometric best practice (Streiner et al., 2015)
  responses_unique <- responses_filtered %>%
    distinct(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, .keep_all = TRUE) %>%
    select(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, RESPONSEVALUE)
  
  # Check sample size
  n_events <- length(unique(responses_unique$QUESTIONNAIREEVENT_ID))
  message(paste("\nSample size:", n_events, "questionnaire events"))
  
  # Warn based on analysis type
  # Per Flora & Curran (2004): FA needs N >= 200
  if (n_events < 200) {
    warning(paste(
      "Sample size (n =", n_events, ") is below the recommended minimum",
      "of 200 for stable polychoric factor analysis (Flora & Curran, 2004).",
      "Consider aggregating across more contexts if performing factor analysis."
    ))
  } else if (n_events < 10) {
    warning(paste(
      "Sample size (n =", n_events, ") is very small.",
      "Reliability estimates require at least 10 observations.",
      "Results should be interpreted with extreme caution."
    ))
  }
  
  # Verify uniqueness
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
  # Each row = one questionnaire event
  # Each column = one question (named Q1, Q2, etc.)
  wide_data <- responses_unique %>%
    tidyr::pivot_wider(
      names_from = QUESTION_NUMBER,
      values_from = RESPONSEVALUE,
      names_prefix = "Q"
    ) %>%
    # Remove the ID column - not needed for analysis
    select(-QUESTIONNAIREEVENT_ID)
  
  message(paste("Converted to wide format:", nrow(wide_data), "rows ×", ncol(wide_data), "questions"))
  
  return(wide_data)
}