#' @title Calculate Cronbach's Alpha for Complete Questionnaire
#' @description Compute Cronbach's alpha reliability coefficient for the
#'   complete questionnaire (all questions) within a specific administration
#'   context (program type × milestone). This assesses the internal consistency
#'   of the entire instrument as administered, not individual indicator subscales.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param program_name character; Program type ("Military" or "Civil Works")
#' @param milestone_name character; Project milestone
#' @param check.keys logical; Auto-reverse negatively correlated items? Default TRUE.
#' @returns list containing alpha statistics and interpretation
#' @export
#' @references
#'   Streiner, D. L., Norman, G. R., & Cairney, J. (2015). Health Measurement 
#'   Scales: A practical guide to their development and use (5th ed.). 
#'   Oxford University Press.
#'   
#'   Cronbach, L. J. (1951). Coefficient alpha and the internal structure of 
#'   tests. Psychometrika, 16(3), 297-334.
#' @section Development Notes:
#' This function was developed with AI assistance (GitHub Copilot, 2026-02-20).
#' Human direction and oversight was provided at each implementation step. 
#' See `dev/sessions/2026-02-20.md` for complete development context.
#' 
#' @importFrom psych alpha
#' @importFrom dplyr case_when
#' 
calculate_cronbach_alpha <- function(responses_df, 
                                     program_name,
                                     milestone_name,
                                     check.keys = TRUE) {
  
  # Get wide format data for complete questionnaire in this context
  wide_data <- get_wide_responses(
    responses_df, 
    program_name,
    milestone_name
  )
  
  # CRITICAL: Check for and remove zero-variance items BEFORE calling psych::alpha()
  # Per psychometric best practice (Streiner et al., 2015)
  
  # Calculate variance for each item
  item_variances <- sapply(wide_data, var, na.rm = TRUE)
  
  # Identify zero-variance items
  zero_var_items <- names(item_variances)[item_variances == 0 | is.na(item_variances)]
  
  if (length(zero_var_items) > 0) {
    message(paste(
      "\nRemoving", length(zero_var_items), 
      "question(s) with zero variance:",
      paste(zero_var_items, collapse = ", ")
    ))
    message(paste(
      "These questions had identical responses across all observations",
      "in this context and do not contribute to internal consistency.\n"
    ))
    
    # Remove zero-variance items
    wide_data <- wide_data[, item_variances > 0 & !is.na(item_variances), drop = FALSE]
  }
  
  # Check for sufficient data AFTER removing zero-variance items
  if (ncol(wide_data) < 2) {
    stop(paste(
      "After removing zero-variance items, fewer than 2 questions remain",
      "with variance in this context.",
      "\nCronbach's alpha requires at least 2 items.",
      "\nOriginal questions:", ncol(wide_data) + length(zero_var_items),
      "\nZero-variance questions removed:", length(zero_var_items),
      "\nRemaining questions:", ncol(wide_data),
      "\nConsider aggregating across milestones or program types."
    ))
  }
  
  if (nrow(wide_data) < 10) {
    warning(paste(
      "Small sample size (n =", nrow(wide_data), ").",
      "Reliability estimates require at least 10 observations for stability.",
      "Results should be interpreted with caution."
    ))
  }
  
  # Calculate Cronbach's alpha
  alpha_result <- tryCatch({
    psych::alpha(wide_data, 
                 check.keys = check.keys,
                 warnings = FALSE)  # Suppress warnings - we handle variance checks
  }, error = function(e) {
    stop(paste(
      "Error calculating Cronbach's alpha:",
      e$message,
      "\nContext:", program_name, "×", milestone_name,
      "\nQuestions:", ncol(wide_data),
      "\nObservations:", nrow(wide_data)
    ))
  })
  
  # Extract key results
  results <- list(
    context = list(
      program = program_name,
      milestone = milestone_name
    ),
    sample = list(
      n_questions_original = ncol(wide_data) + length(zero_var_items),
      n_questions_analyzed = ncol(wide_data),
      n_questions_removed = length(zero_var_items),
      removed_questions = if (length(zero_var_items) > 0) zero_var_items else NULL,
      n_observations = nrow(wide_data)
    ),
    reliability = list(
      alpha_raw = alpha_result$total$raw_alpha,
      alpha_std = alpha_result$total$std.alpha,
      average_r = alpha_result$total$average_r
    ),
    item_statistics = alpha_result$item.stats,
    alpha_if_dropped = alpha_result$alpha.drop,
    full_output = alpha_result
  )
  
  # Add interpretation per Nunnally & Bernstein (1994)
  results$interpretation <- case_when(
    results$reliability$alpha_std >= 0.90 ~ "Excellent",
    results$reliability$alpha_std >= 0.80 ~ "Good",
    results$reliability$alpha_std >= 0.70 ~ "Acceptable",
    results$reliability$alpha_std >= 0.60 ~ "Questionable",
    TRUE ~ "Poor"
  )
  
  return(results)
}