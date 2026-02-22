#' @title Assess Data Factorability for Factor Analysis
#' @description Evaluate whether data are suitable for factor analysis using
#'   polychoric correlations (appropriate for ordinal data) and diagnostic tests.
#'   Checks for zero-variance items, multicollinearity, and matrix invertibility.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param program_name character; Program type or NULL for all programs
#' @param milestone_name character; Project milestone or NULL for all milestones
#' @param multicollinearity_threshold numeric; Correlation threshold (0-1) for
#'   flagging highly correlated item pairs. Default 0.90. Items with |r| above
#'   this are documented but NOT automatically removed (see Note).
#' @returns list containing polychoric correlations, diagnostic results, and
#'   recommendations for proceeding with factor analysis
#' @export
#' @references
#'   Holgado-Tello, F. P., Chacón-Moscoso, S., Barbero-García, I., & 
#'   Vila-Abad, E. (2010). Polychoric versus Pearson correlations in 
#'   exploratory and confirmatory factor analysis of ordinal variables. 
#'   Quality & Quantity, 44(1), 153-166.
#'   https://doi.org/10.1007/s11135-008-9190-y
#'   
#'   Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of 
#'   alternative methods of estimation for confirmatory factor analysis 
#'   with ordinal data. Psychological Methods, 9(4), 466-491.
#'   https://doi.org/10.1037/1082-989X.9.4.466
#'   
#'   Revelle, W. (2024). psych: Procedures for Psychological, Psychometric, 
#'   and Personality Research. Northwestern University.
#'   https://personality-project.org/r/psych/
#'   
#' @section Note on Multicollinearity:
#'   High multicollinearity is EXPECTED in best practice assessments where
#'   related items naturally correlate. Items are DOCUMENTED but not automatically
#'   removed. Use ridge-regularized factor analysis (correct parameter in fa.poly())
#'   to handle multicollinearity rather than removing theoretically important items.
#'   
#' @section Development Notes:
#'   This function was updated 2026-02-21 to use polychoric correlations
#'   per Holgado-Tello et al. (2010) recommendations for ordinal data.
#'   See `dev/sessions/2026-02-21.md` for complete development context.
#' 
#' @importFrom psych polychoric
#' @importFrom dplyr case_when
#' 
assess_factorability <- function(responses_df, 
                                program_name = NULL,
                                milestone_name = NULL,
                                multicollinearity_threshold = 0.90) {
  
  # Get wide format data with aggregation support
  wide_data <- get_wide_responses(
    responses_df,
    program_name = program_name,
    milestone_name = milestone_name
  )
  
  context_label <- paste(
    ifelse(is.null(program_name), "All Programs", program_name),
    "×",
    ifelse(is.null(milestone_name), "All Milestones", milestone_name)
  )
  
  message(paste(
    "\n=== Assessing Factorability ===",
    "\nContext:", context_label,
    "\nQuestions (original):", ncol(wide_data),
    "\nObservations:", nrow(wide_data)
  ))
  
  # ===== STEP 1: Remove zero-variance items =====
  # Per psychometric best practice (Streiner et al., 2015)
  message("\n--- Step 1: Checking for zero-variance items ---")
  
  item_variances <- sapply(wide_data, var, na.rm = TRUE)
  zero_var_items <- names(item_variances)[item_variances == 0 | is.na(item_variances)]
  n_questions_original <- ncol(wide_data)
  
  if (length(zero_var_items) > 0) {
    message(paste(
      "Removing", length(zero_var_items), 
      "question(s) with zero variance:",
      paste(zero_var_items, collapse = ", ")
    ))
    message("(Items with no variation cannot contribute to factor analysis)")
    wide_data <- wide_data[, item_variances > 0 & !is.na(item_variances), drop = FALSE]
  } else {
    message("No zero-variance items found")
  }
  
  # Check minimum items
  if (ncol(wide_data) < 3) {
    stop(paste(
      "After removing zero-variance items, only", ncol(wide_data), "questions remain.",
      "Factor analysis requires at least 3 items.",
      "Consider aggregating across more contexts."
    ))
  }
  
  # ===== STEP 2: Calculate polychoric correlation matrix =====
  # Per Holgado-Tello et al. (2010) - appropriate for ordinal data
  message("\n--- Step 2: Computing polychoric correlations ---")
  message("(Appropriate for ordinal/Likert-type data per Holgado-Tello et al., 2010)")
  
  poly_result <- tryCatch({
    psych::polychoric(wide_data)
  }, error = function(e) {
    stop(paste(
      "Polychoric correlation calculation failed:", e$message,
      "\nThis may indicate insufficient variability in responses.",
      "Try aggregating across more contexts."
    ))
  })
  
  cor_matrix <- poly_result$rho
  message("Successfully computed polychoric correlations")
  
  # ===== STEP 3: Check multicollinearity =====
  message("\n--- Step 3: Diagnosing multicollinearity ---")
  
  # Set diagonal to 0 to exclude self-correlations
  cor_matrix_check <- cor_matrix
  diag(cor_matrix_check) <- 0
  
  # Find high correlation pairs
  high_cor_indices <- which(abs(cor_matrix_check) > multicollinearity_threshold, arr.ind = TRUE)
  
  # Remove duplicates (matrix is symmetric)
  if (nrow(high_cor_indices) > 0) {
    high_cor_indices <- high_cor_indices[high_cor_indices[,1] < high_cor_indices[,2], , drop = FALSE]
  }
  
  multicollinear_pairs <- NULL
  
  if (nrow(high_cor_indices) > 0) {
    message(paste(
      "Found", nrow(high_cor_indices), 
      "item pair(s) with |r| >", multicollinearity_threshold
    ))
    
    # Document pairs
    pair_data <- data.frame(
      item1 = colnames(wide_data)[high_cor_indices[, 1]],
      item2 = colnames(wide_data)[high_cor_indices[, 2]],
      correlation = cor_matrix[high_cor_indices],
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(pair_data)) {
      message(paste("  ", pair_data$item1[i], "×", pair_data$item2[i], 
                    ": r =", round(pair_data$correlation[i], 3)))
    }
    
    message(paste(
      "\nNote: High multicollinearity is EXPECTED for best practice assessments.",
      "\nItems are documented but NOT automatically removed.",
      "\nWill use ridge-regularized factor analysis to handle this."
    ))
    
    multicollinear_pairs <- pair_data
  } else {
    message("No problematic multicollinearity detected")
  }
  
  # ===== STEP 4: Check matrix properties =====
  message("\n--- Step 4: Checking correlation matrix properties ---")
  
  cor_det <- det(cor_matrix)
  message(paste("Determinant:", format(cor_det, scientific = TRUE)))
  
  can_invert <- TRUE
  if (cor_det < 1e-10) {
    message("WARNING: Determinant is very small (near-singular matrix)")
    message("This indicates substantial multicollinearity")
    message("Recommendation: Use ridge-regularized FA (correct > 0 in fa.poly)")
    can_invert <- FALSE
  } else {
    message("Matrix determinant is acceptable")
  }
  
  # ===== STEP 5: Compile results =====
  message("\n--- Summary ---")
  message(paste("Questions analyzed:", ncol(wide_data)))
  message(paste("Questions removed (zero-variance):", length(zero_var_items)))
  message(paste("High correlation pairs:", nrow(high_cor_indices)))
  
  results <- list(
    context = list(
      program = ifelse(is.null(program_name), "All", program_name),
      milestone = ifelse(is.null(milestone_name), "All", milestone_name),
      label = context_label
    ),
    sample = list(
      n_questions_original = n_questions_original,
      n_questions_analyzed = ncol(wide_data),
      n_questions_removed_zero_var = length(zero_var_items),
      removed_questions_zero_var = if (length(zero_var_items) > 0) zero_var_items else NULL,
      n_observations = nrow(wide_data)
    ),
    correlations = list(
      method = "polychoric",
      matrix = cor_matrix,
      tau = poly_result$tau,  # Thresholds for ordinal categories
      determinant = cor_det,
      can_invert = can_invert
    ),
    multicollinearity = list(
      n_pairs_high = nrow(high_cor_indices),
      threshold = multicollinearity_threshold,
      high_cor_pairs = multicollinear_pairs
    ),
    recommendation = list(
      proceed_with_fa = (ncol(wide_data) >= 3 && nrow(wide_data) >= 10),
      use_ridge = (cor_det < 1e-10 || nrow(high_cor_indices) > 0),
      suggested_ridge = ifelse(cor_det < 1e-10, 0.1, 
                               ifelse(nrow(high_cor_indices) > 5, 0.05, 0.01)),
      rationale = case_when(
        ncol(wide_data) < 3 ~ "Insufficient items for factor analysis",
        nrow(wide_data) < 10 ~ "Insufficient sample size",
        !can_invert ~ "Use ridge-regularized FA due to near-singular matrix",
        nrow(high_cor_indices) > 0 ~ "Proceed with ridge-regularized FA to handle multicollinearity",
        TRUE ~ "Data are suitable for factor analysis"
      )
    )
  )
  
  message(paste("\nRecommendation:", results$recommendation$rationale))
  if (results$recommendation$use_ridge) {
    message(paste("Suggested ridge correction:", results$recommendation$suggested_ridge))
  }
  
  return(results)
}