#' @title Assess Data Factorability for Factor Analysis
#' @description Evaluate whether the correlation matrix is suitable for factor
#'   analysis using Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy and
#'   Bartlett's test of sphericity.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param program_name character; Program type ("Military" or "Civil Works")
#' @param milestone_name character; Project milestone
#' @returns list containing KMO and Bartlett test results with interpretation
#' @export
#' @references
#'   Kaiser, H. F. (1974). An index of factorial simplicity. 
#'   Psychometrika, 39(1), 31-36.
#'   
#'   Bartlett, M. S. (1950). Tests of significance in factor analysis. 
#'   British Journal of Psychology, 3(2), 77-85.
#'   
#'   Kaiser, H. F., & Rice, J. (1974). Little jiffy, mark IV. 
#'   Educational and Psychological Measurement, 34(1), 111-117.
#'   
#' @section Interpretation Guidelines:
#'   KMO overall values (Kaiser & Rice, 1974): 
#'      * < 0.50 unacceptable
#'      * 0.50-0.59 miserable
#'      * 0.60-0.69 mediocre
#'      * 0.70-0.79 middling
#'      * 0.80-0.89 meritorious
#'      * >= 0.90 marvelous
#'   
#'   Bartlett's test should be significant (p < .05) to reject the null
#'   hypothesis that the correlation matrix is an identity matrix.
#'   
#' @section Development Notes:
#'   This function was developed with AI assistance (GitHub Copilot, 2026-02-21).
#'   Human direction and oversight was provided at each implementation step. 
#'   See `dev/sessions/2026-02-21.md` for complete development context.
#' 
#' @importFrom psych KMO cortest.bartlett
#' @importFrom dplyr case_when
#' 
assess_factorability <- function(responses_df, 
                                program_name = NULL,
                                milestone_name = NULL) {
  
  # Validate inputs
  if (is.null(program_name)) {
    stop("program_name is required. Must be 'Military' or 'Civil Works'.")
  }
  if (is.null(milestone_name)) {
    stop("milestone_name is required (e.g., '95% (Final Design)').")
  }
  
  # Get wide format data using existing infrastructure
  wide_data <- get_wide_responses(
    responses_df,
    program_name = program_name,
    milestone_name = milestone_name
  )
  
  # CRITICAL: Check for and remove zero-variance items BEFORE calculating correlation
  # Per psychometric best practice (Streiner et al., 2015) and psych package requirements
  
  # Calculate variance for each item
  item_variances <- sapply(wide_data, var, na.rm = TRUE)
  
  # Identify zero-variance items
  zero_var_items <- names(item_variances)[item_variances == 0 | is.na(item_variances)]
  
  n_questions_original <- ncol(wide_data)
  
  if (length(zero_var_items) > 0) {
    message(paste(
      "\nRemoving", length(zero_var_items), 
      "question(s) with zero variance:",
      paste(zero_var_items, collapse = ", ")
    ))
    message(paste(
      "These questions had identical responses across all observations",
      "in this context and cannot be included in factor analysis.\n"
    ))
    
    # Remove zero-variance items
    wide_data <- wide_data[, item_variances > 0 & !is.na(item_variances), drop = FALSE]
  }
  
  # Check for sufficient data AFTER removing zero-variance items
  if (ncol(wide_data) < 3) {
    stop(paste(
      "After removing zero-variance items, fewer than 3 questions remain",
      "with variance in this context.",
      "\nFactor analysis requires at least 3 items.",
      "\nOriginal questions:", n_questions_original,
      "\nZero-variance questions removed:", length(zero_var_items),
      "\nRemaining questions:", ncol(wide_data),
      "\nConsider aggregating across milestones or program types."
    ))
  }
  
  message(paste(
    "Questions (after removing zero-variance):", ncol(wide_data)
  ))

  message(paste(
    "\nAssessing factorability for:", program_name, "×", milestone_name,
    "\nQuestions:", ncol(wide_data),
    "\nObservations:", nrow(wide_data)
  ))
  
  # Calculate correlation matrix (needed for both tests)
  cor_matrix <- cor(wide_data, use = "pairwise.complete.obs")
  
  # 1. Kaiser-Meyer-Olkin (KMO) Test
  # Per psych::KMO() documentation
  kmo_result <- tryCatch({
    psych::KMO(cor_matrix)
  }, error = function(e) {
    warning(paste("KMO calculation failed:", e$message))
    NULL
  })
  
  # 2. Bartlett's Test of Sphericity
  # Per psych::cortest.bartlett() documentation
  bartlett_result <- tryCatch({
    psych::cortest.bartlett(cor_matrix, n = nrow(wide_data))
  }, error = function(e) {
    warning(paste("Bartlett's test failed:", e$message))
    NULL
  })
  
  # Interpret KMO (Kaiser & Rice, 1974 criteria)
  kmo_interpretation <- if (!is.null(kmo_result)) {
    kmo_overall <- kmo_result$MSA
    
    case_when(
      kmo_overall >= 0.90 ~ "Marvelous",
      kmo_overall >= 0.80 ~ "Meritorious", 
      kmo_overall >= 0.70 ~ "Middling",
      kmo_overall >= 0.60 ~ "Mediocre",
      kmo_overall >= 0.50 ~ "Miserable",
      TRUE ~ "Unacceptable"
    )
  } else {
    "Unable to calculate"
  }
  
  # Interpret Bartlett (standard p < .05 criterion)
  bartlett_interpretation <- if (!is.null(bartlett_result)) {
    if (bartlett_result$p.value < 0.001) {
      "Highly significant (p < .001) - correlation matrix is factorable"
    } else if (bartlett_result$p.value < 0.05) {
      "Significant (p < .05) - correlation matrix is factorable"
    } else {
      "Not significant - correlation matrix may be an identity matrix (not factorable)"
    }
  } else {
    "Unable to calculate"
  }
  
  # Overall recommendation
  proceed_with_fa <- if (!is.null(kmo_result) && !is.null(bartlett_result)) {
    kmo_result$MSA >= 0.60 && bartlett_result$p.value < 0.05
  } else {
    FALSE
  }
  
  # Compile results
  results <- list(
    context = list(
      program = program_name,
      milestone = milestone_name
    ),
    sample = list(
      n_questions_original = n_questions_original,
      n_questions_analyzed = ncol(wide_data),
      n_questions_removed = length(zero_var_items),
      removed_questions = if (length(zero_var_items) > 0) zero_var_items else NULL,
      n_observations = nrow(wide_data)
    ),
    kmo = list(
      overall_msa = if (!is.null(kmo_result)) kmo_result$MSA else NA,
      item_msa = if (!is.null(kmo_result)) kmo_result$MSAi else NA,
      interpretation = kmo_interpretation,
      full_output = kmo_result
    ),
    bartlett = list(
      chi_square = if (!is.null(bartlett_result)) bartlett_result$chisq else NA,
      df = if (!is.null(bartlett_result)) bartlett_result$df else NA,
      p_value = if (!is.null(bartlett_result)) bartlett_result$p.value else NA,
      interpretation = bartlett_interpretation,
      full_output = bartlett_result
    ),
    recommendation = list(
      proceed_with_fa = proceed_with_fa,
      rationale = if (proceed_with_fa) {
        "Data meet minimum criteria for factor analysis (KMO >= 0.60 and significant Bartlett test)"
      } else if (!is.null(kmo_result) && kmo_result$MSA < 0.60) {
        paste0("KMO value (", round(kmo_result$MSA, 3), ") below recommended minimum of 0.60")
      } else if (!is.null(bartlett_result) && bartlett_result$p.value >= 0.05) {
        "Bartlett's test not significant - variables may be too uncorrelated"
      } else {
        "Unable to assess - calculation failed"
      }
    )
  )
  
  # Print summary
  message("\n=== Factorability Assessment Results ===")
  message(paste("KMO Overall MSA:", 
                round(results$kmo$overall_msa, 3), 
                "-", results$kmo$interpretation))
  message(paste("Bartlett's Test: χ²(", results$bartlett$df, ") =", 
                round(results$bartlett$chi_square, 2),
                ", p =", format.pval(results$bartlett$p_value)))
  message(paste("\nRecommendation:", results$recommendation$rationale))
  
  return(results)
}
