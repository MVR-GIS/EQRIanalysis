#' @title Calculate McDonald's Omega for Complete Questionnaire
#' @description Compute McDonald's omega reliability coefficient using 
#'   hierarchical factor analysis. Omega is generally preferred over alpha
#'   as it makes fewer assumptions about item structure and provides more
#'   accurate reliability estimates when items have different factor loadings.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param program_name character; Program type ("Military" or "Civil Works")
#' @param milestone_name character; Project milestone
#' @param nfactors integer; Number of group factors to extract. Default is 1
#'   (assumes unidimensional scale). Set to NULL for automatic determination.
#' @returns list containing omega statistics and comparison with alpha, or
#'   NULL if calculation fails (with warning message)
#' @export
#' @references
#'   McDonald, R. P. (1999). Test theory: A unified treatment. 
#'   Mahwah, NJ: Erlbaum.
#'   
#'   Revelle, W., & Zinbarg, R. E. (2009). Coefficients alpha, beta, omega 
#'   and the glb: Comments on Sijtsma. Psychometrika, 74(1), 145-154.
#'   https://doi.org/10.1007/s11336-008-9102-z
#'   
#'   Dunn, T. J., Baguley, T., & Brunsden, V. (2014). From alpha to omega: 
#'   A practical solution to the pervasive problem of internal consistency 
#'   estimation. British Journal of Psychology, 105(3), 399-412.
#' @section Development Notes:
#'   This function was developed with AI assistance (GitHub Copilot, 2026-02-20).
#'   Human direction and oversight was provided at each implementation step. 
#'   See `dev/sessions/2026-02-20.md` for complete development context.
#' 
#' @importFrom psych omega
#' 
calculate_omega <- function(responses_df, 
                           program_name,
                           milestone_name,
                           nfactors = 1) {
  
  # Get wide format data
  wide_data <- get_wide_responses(
    responses_df,
    program_name,
    milestone_name
  )
  
  # Remove zero-variance items (same as calculate_cronbach_alpha)
  item_variances <- sapply(wide_data, var, na.rm = TRUE)
  zero_var_items <- names(item_variances)[item_variances == 0 | is.na(item_variances)]
  
  if (length(zero_var_items) > 0) {
    message(paste(
      "\n[Omega] Removing", length(zero_var_items), 
      "question(s) with zero variance:",
      paste(zero_var_items, collapse = ", "), "\n"
    ))
    wide_data <- wide_data[, item_variances > 0 & !is.na(item_variances), drop = FALSE]
  }
  
  # Check for sufficient items
  # Omega requires at least 3 items per psych::omega() documentation
  if (ncol(wide_data) < 3) {
    warning(paste(
      "Omega calculation requires at least 3 items.",
      "Context:", program_name, "×", milestone_name,
      "has", ncol(wide_data), "items after removing zero-variance questions.",
      "Returning NULL."
    ))
    return(NULL)
  }
  
  # Check sample size
  # Rule of thumb: n >= 100 or n >= 5*items (whichever is larger)
  min_n <- max(100, 5 * ncol(wide_data))
  if (nrow(wide_data) < min_n) {
    warning(paste(
      "Small sample size for omega calculation.",
      "Context:", program_name, "×", milestone_name,
      "has n =", nrow(wide_data), 
      "observations for", ncol(wide_data), "items.",
      "Recommended minimum:", min_n,
      "Results may be unstable."
    ))
  }
  
  # Calculate omega using psych package
  # This performs hierarchical factor analysis
  omega_result <- tryCatch({
    psych::omega(
      wide_data, 
      nfactors = nfactors,
      fm = "minres",       # Minimum residual factor method
      plot = FALSE,        # Don't auto-plot in function
      warnings = FALSE     # We handle warnings ourselves
    )
  }, error = function(e) {
    warning(paste(
      "Omega calculation failed for", program_name, "×", milestone_name,
      "\nError:", e$message,
      "\nReturning NULL."
    ))
    return(NULL)
  })
  
  if (is.null(omega_result)) {
    return(NULL)
  }
  
  # Extract results
  results <- list(
    context = list(
      program = program_name,
      milestone = milestone_name
    ),
    sample = list(
      n_questions_analyzed = ncol(wide_data),
      n_questions_removed = length(zero_var_items),
      removed_questions = if (length(zero_var_items) > 0) zero_var_items else NULL,
      n_observations = nrow(wide_data)
    ),
    omega = list(
      omega_total = omega_result$omega.tot,          # Total omega
      omega_hierarchical = omega_result$omega_h,     # Hierarchical omega
      omega_asymptotic = omega_result$omega.lim,     # Asymptotic omega
      alpha_comparison = omega_result$alpha          # Cronbach's alpha for comparison
    ),
    factor_structure = list(
      n_factors = nfactors,
      schmid = omega_result$schmid,                  # Schmid-Leiman transformation
      loadings = omega_result$omega.group            # Factor loadings
    ),
    full_output = omega_result
  )
  
  # Add interpretation note
  results$interpretation <- list(
    note = "Omega total (ω_t) is analogous to Cronbach's alpha but more accurate when items have unequal factor loadings.",
    comparison = if (!is.null(omega_result$alpha)) {
      diff <- results$omega$omega_total - results$omega$alpha_comparison
      if (abs(diff) > 0.05) {
        paste(
          "Omega (", round(results$omega$omega_total, 3), 
          ") differs from alpha (", round(results$omega$alpha_comparison, 3),
          ") by ", round(abs(diff), 3),
          ", suggesting items have unequal factor loadings.",
          sep = ""
        )
      } else {
        "Omega and alpha are similar, suggesting approximately equal factor loadings."
      }
    } else {
      "Alpha comparison not available."
    }
  )
  
  return(results)
}