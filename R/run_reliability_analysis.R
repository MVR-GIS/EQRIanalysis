#' @title Run Reliability Analysis Across All Contexts
#' @description Calculate Cronbach's alpha and McDonald's omega for all
#'   program type × milestone combinations in the dataset. This provides
#'   a comprehensive view of questionnaire reliability across contexts.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param include_omega logical; Calculate McDonald's omega in addition to alpha?
#'   Default TRUE. Set FALSE for faster computation.
#' @returns data.frame with one row per context containing reliability metrics
#' @export
#' @importFrom dplyr %>% distinct arrange bind_rows
#' @importFrom purrr map2
#' @references
#'   Streiner, D. L., Norman, G. R., & Cairney, J. (2015). Health Measurement 
#'   Scales: A practical guide to their development and use (5th ed.). 
#'   Oxford University Press. Chapter 4.
#' 
run_reliability_analysis <- function(responses_df, include_omega = TRUE) {
  
  # Get all unique program × milestone combinations
  contexts <- responses_df %>%
    distinct(PROGRAMTYPE_NAME, MILESTONE_DESC) %>%
    arrange(PROGRAMTYPE_NAME, MILESTONE_DESC)
  
  message(paste("\nRunning reliability analysis for", nrow(contexts), "contexts...\n"))
  
  # Initialize results list
  results_list <- list()
  
  # Loop through each context
  for (i in 1:nrow(contexts)) {
    program <- contexts$PROGRAMTYPE_NAME[i]
    milestone <- contexts$MILESTONE_DESC[i]
    
    message(paste("Analyzing:", program, "×", milestone))
    
    # Calculate Cronbach's alpha
    alpha_result <- tryCatch({
      suppressMessages({
        calculate_cronbach_alpha(responses_df, program, milestone)
      })
    }, error = function(e) {
      warning(paste("Alpha calculation failed for", program, "×", milestone, ":", e$message))
      NULL
    })
    
    # Calculate McDonald's omega if requested
    omega_result <- if (include_omega) {
      tryCatch({
        suppressMessages({
          suppressWarnings({
            calculate_omega(responses_df, program, milestone)
          })
        })
      }, error = function(e) {
        warning(paste("Omega calculation failed for", program, "×", milestone, ":", e$message))
        NULL
      })
    } else {
      NULL
    }
    
    # Extract key metrics into a single row
    if (!is.null(alpha_result)) {
      row <- data.frame(
        program = program,
        milestone = milestone,
        n_observations = alpha_result$sample$n_observations,
        n_questions_original = alpha_result$sample$n_questions_original,
        n_questions_analyzed = alpha_result$sample$n_questions_analyzed,
        n_questions_removed = alpha_result$sample$n_questions_removed,
        alpha_raw = alpha_result$reliability$alpha_raw,
        alpha_std = alpha_result$reliability$alpha_std,
        alpha_interpretation = alpha_result$interpretation,
        average_r = alpha_result$reliability$average_r,
        omega_total = if (!is.null(omega_result)) omega_result$omega$omega_total else NA,
        omega_hierarchical = if (!is.null(omega_result)) omega_result$omega$omega_hierarchical else NA,
        omega_alpha_diff = if (!is.null(omega_result)) {
          omega_result$omega$omega_total - omega_result$omega$alpha_comparison
        } else NA,
        stringsAsFactors = FALSE
      )
      
      results_list[[i]] <- row
    }
  }
  
  # Combine all results
  results_df <- bind_rows(results_list)
  
  # Add factor for ordering
  results_df$milestone <- factor(
    results_df$milestone,
    levels = c(
      "15% (Project Initiation)",
      "35% (Concept Design)",
      "65% (Intermediate Design)",
      "95% (Final Design)",
      "100% (Corrected Final Design)"
    )
  )
  
  results_df$alpha_interpretation <- factor(
    results_df$alpha_interpretation,
    levels = c("Poor", "Questionable", "Acceptable", "Good", "Excellent")
  )
  
  message("\nReliability analysis complete!")
  message(paste("Analyzed", nrow(results_df), "contexts"))
  
  return(results_df)
}