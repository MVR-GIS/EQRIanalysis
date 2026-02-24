#' @title Run Exploratory Factor Analysis
#' @description Perform exploratory factor analysis (EFA) using specified
#'   extraction and rotation methods. Evaluates model fit and returns
#'   factor loadings, communalities, and fit indices.
#'
#' @param responses_df data.frame; Output from get_responses_df()
#' @param program_name character; Program type (e.g., "Military", "Civil Works")
#' @param milestone_name character; Milestone (e.g., "100% (Corrected Final Design)")
#' @param nfactors integer; Number of factors to extract. If NULL, uses
#'   parallel analysis recommendation.
#' @param fm character; Factor extraction method (default "minres"). Options:
#'   "minres" (minimum residual), "ml" (maximum likelihood), "pa" (principal axis),
#'   "wls" (weighted least squares), "gls" (generalized least squares)
#' @param rotate character; Rotation method (default "oblimin"). Options:
#'   "oblimin" (oblique), "promax" (oblique), "varimax" (orthogonal), "none"
#' @param loading_threshold numeric; Minimum factor loading to report (default 0.40
#'   per Costello & Osborne, 2005)
#' @param cross_loading_threshold numeric; Threshold for identifying problematic
#'   cross-loadings (default 0.32 per Costello & Osborne, 2005)
#'
#' @returns list containing:
#'   \item{context}{Program and milestone information}
#'   \item{sample}{Sample size and number of items analyzed}
#'   \item{extraction_info}{Extraction method and rotation details}
#'   \item{fit_indices}{TLI, RMSEA, RMSR, and interpretation}
#'   \item{loadings}{Factor loading matrix}
#'   \item{loadings_clean}{Loadings above threshold, suppressed otherwise}
#'   \item{communalities}{Variance explained per item}
#'   \item{variance_explained}{Proportion variance per factor and cumulative}
#'   \item{factor_correlations}{Correlations between factors (if oblique rotation)}
#'   \item{problematic_items}{Items with low communalities or cross-loadings}
#'   \item{fa_object}{Full psych::fa() object for additional analysis}
#'
#' @export
#'
#' @section References:
#'   Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J. (1999).
#'   Evaluating the use of exploratory factor analysis in psychological research.
#'   Psychological Methods, 4(3), 272-299.
#'
#'   Costello, A. B., & Osborne, J. (2005). Best practices in exploratory
#'   factor analysis: Four recommendations for getting the most from your analysis.
#'   Practical Assessment, Research, and Evaluation, 10(7), 1-9.
#'
#'   Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
#'   covariance structure analysis. Structural Equation Modeling, 6(1), 1-55.
#'
#' @section Interpretation:
#'   Factor Loadings: Values indicate strength of relationship between
#'   item and factor. Per Costello & Osborne (2005):#'
#'   * > 0.70: Excellent
#'   * 0.60-0.70: Very good
#'   * 0.50-0.60: Good
#'   * 0.40-0.50: Fair (acceptable minimum)
#'   * < 0.40: Poor (consider removing item)
#'
#'   Cross-loadings: Items loading > 0.32 on multiple factors may be
#'   problematic and should be reviewed for theoretical fit.
#'
#'   Communalities: Proportion of variance explained. Values < 0.25 indicate
#'   item is poorly represented by the factor solution.
#'
#'   Fit Indices (per Hu & Bentler, 1999):#'
#'   * TLI > 0.95: Excellent, 0.90-0.95: Acceptable
#'   * RMSEA < 0.05: Excellent, 0.05-0.08: Acceptable
#'   * RMSR < 0.05: Good
#' 
#' @section Sample Size Requirements:
#'   Different extraction methods have different sample size requirements
#'   (per Fabrigar et al., 1999; psych::fa() documentation):
#'   
#'   - **Maximum Likelihood (ml)**: Requires N >= 200. May fail with smaller
#'     samples due to optimization convergence issues.
#'   - **Minimum Residual (minres)**: Robust for small samples. Recommended
#'     for N < 200.
#'   - **Principal Axis (pa)**: Robust for small samples.
#'   
#'   If ml is requested with N < 200, function will issue a warning and
#'   automatically switch to minres.
#' 
#' @importFrom psych fa fa.parallel
#' @importFrom dplyr %>% mutate case_when
#'
run_efa <- function(
  responses_df,
  program_name,
  milestone_name,
  nfactors = NULL,
  fm = "minres",
  rotate = "oblimin",
  loading_threshold = 0.40,
  cross_loading_threshold = 0.32
) {
  # Get wide format data
  wide_data <- get_wide_responses(
    responses_df,
    program_name,
    milestone_name
  )

  # Remove zero-variance items
  item_variances <- sapply(wide_data, var, na.rm = TRUE)
  zero_var_items <- names(item_variances)[
    item_variances == 0 | is.na(item_variances)
  ]

  if (length(zero_var_items) > 0) {
    message(paste(
      "\n[EFA] Removing",
      length(zero_var_items),
      "zero-variance item(s):",
      paste(zero_var_items, collapse = ", ")
    ))
    wide_data <- wide_data[,
      item_variances > 0 & !is.na(item_variances),
      drop = FALSE
    ]
  }

  # Check minimum requirements
  n_items <- ncol(wide_data)
  n_obs <- nrow(wide_data)

  if (n_items < 3) {
    stop(paste(
      "EFA requires at least 3 items.",
      "Context has",
      n_items,
      "items after removing zero-variance questions."
    ))
  }

  # Determine number of factors if not specified
  if (is.null(nfactors)) {
    message("\nNumber of factors not specified. Running parallel analysis...")

    pa_result <- tryCatch(
      {
        psych::fa.parallel(
          wide_data,
          fm = fm,
          fa = "fa",
          n.iter = 20,
          plot = FALSE,
          main = paste(program_name, "×", milestone_name)
        )
      },
      error = function(e) {
        warning(paste("Parallel analysis failed:", e$message))
        NULL
      }
    )

    if (!is.null(pa_result)) {
      nfactors <- pa_result$nfact
      message(paste("Parallel analysis suggests", nfactors, "factors"))
    } else {
      # Fallback: use eigenvalue > 1 rule (Kaiser criterion)
      cor_matrix <- cor(wide_data, use = "pairwise.complete.obs")
      eigenvalues <- eigen(cor_matrix)$values
      nfactors <- sum(eigenvalues > 1)
      message(paste("Using Kaiser criterion:", nfactors, "factors"))
    }
  }

  # Validate nfactors
  max_factors <- min(n_items - 1, floor(n_obs / 5))
  if (nfactors > max_factors) {
    warning(paste(
      "Requested",
      nfactors,
      "factors exceeds maximum",
      max_factors,
      "for sample size. Adjusting to",
      max_factors
    ))
    nfactors <- max_factors
  }

  # Validate extraction method for sample size
  # Per psych::fa() documentation and Fabrigar et al. (1999)
  if (fm == "ml" && n_obs < 200) {
    warning(paste(
      "Maximum likelihood (ml) extraction requires N >= 200.",
      sprintf("Current sample: N = %d.", n_obs),
      "Switching to minres (minimum residual) for robustness.",
      "See Fabrigar et al. (1999) and psych::fa() documentation."
    ))
    fm <- "minres"
  }

  message(paste(
    "\n=== Running Exploratory Factor Analysis ===",
    "\nContext:",
    program_name,
    "×",
    milestone_name,
    "\nExtraction method:",
    fm,
    "\nRotation:",
    rotate,
    "\nFactors to extract:",
    nfactors,
    "\nItems:",
    n_items,
    "\nObservations:",
    n_obs,
    "\n"
  ))

  # Run EFA
  fa_result <- tryCatch(
    {
      psych::fa(
        wide_data,
        nfactors = nfactors,
        fm = fm,
        rotate = rotate,
        scores = "regression",
        SMC = TRUE,
        warnings = FALSE
      )
    },
    error = function(e) {
      stop(paste(
        "EFA failed for",
        program_name,
        "×",
        milestone_name,
        "\nError:",
        e$message,
        "\nTry different extraction method or reduce number of factors."
      ))
    }
  )

  # Extract loadings matrix
  loadings_matrix <- fa_result$loadings
  loadings_df <- as.data.frame(unclass(loadings_matrix))

  # Create "clean" loadings (suppress < threshold)
  loadings_clean <- loadings_df
  loadings_clean[abs(loadings_clean) < loading_threshold] <- NA

  # Extract communalities (h2 = variance explained per item)
  communalities <- fa_result$communality

  # Identify problematic items
  low_communality <- names(communalities)[communalities < 0.25]

  # Identify cross-loadings
  cross_loadings <- apply(abs(loadings_df), 1, function(row) {
    sum(row > cross_loading_threshold) > 1
  })
  cross_loaded_items <- names(cross_loadings)[cross_loadings]

  # Extract fit indices
  fit_indices <- list(
    TLI = fa_result$TLI,
    RMSEA = fa_result$RMSEA[1], # RMSEA point estimate
    RMSR = fa_result$rms,
    BIC = fa_result$BIC,
    chi_square = fa_result$chi,
    df = fa_result$dof,
    p_value = fa_result$PVAL
  )

  # Interpret fit per Hu & Bentler (1999)
  fit_interpretation <- list(
    TLI = case_when(
      is.na(fit_indices$TLI) ~ "Not available",
      fit_indices$TLI >= 0.95 ~ "Excellent",
      fit_indices$TLI >= 0.90 ~ "Acceptable",
      TRUE ~ "Poor"
    ),
    RMSEA = case_when(
      is.na(fit_indices$RMSEA) ~ "Not available",
      fit_indices$RMSEA <= 0.05 ~ "Excellent",
      fit_indices$RMSEA <= 0.08 ~ "Acceptable",
      TRUE ~ "Poor"
    ),
    RMSR = case_when(
      is.na(fit_indices$RMSR) ~ "Not available",
      fit_indices$RMSR <= 0.05 ~ "Good",
      fit_indices$RMSR <= 0.08 ~ "Acceptable",
      TRUE ~ "Poor"
    )
  )

  # Extract variance explained
  variance_explained <- fa_result$Vaccounted

  # Extract factor correlations (if oblique rotation)
  factor_correlations <- if (rotate %in% c("oblimin", "promax")) {
    fa_result$Phi
  } else {
    NULL
  }

  # Compile results
  results <- list(
    context = list(
      program = program_name,
      milestone = milestone_name
    ),
    sample = list(
      n_observations = n_obs,
      n_items = n_items,
      n_items_removed = length(zero_var_items),
      removed_items = if (length(zero_var_items) > 0) zero_var_items else NULL
    ),
    extraction_info = list(
      method = fm,
      rotation = rotate,
      nfactors = nfactors,
      loading_threshold = loading_threshold,
      cross_loading_threshold = cross_loading_threshold
    ),
    fit_indices = fit_indices,
    fit_interpretation = fit_interpretation,
    loadings = loadings_df,
    loadings_clean = loadings_clean,
    communalities = communalities,
    variance_explained = variance_explained,
    factor_correlations = factor_correlations,
    problematic_items = list(
      low_communality = if (length(low_communality) > 0) {
        low_communality
      } else {
        NULL
      },
      cross_loadings = if (length(cross_loaded_items) > 0) {
        cross_loaded_items
      } else {
        NULL
      }
    ),
    fa_object = fa_result # Full object for advanced users
  )

  # Print summary
  message("\n=== EFA RESULTS SUMMARY ===")
  message(sprintf("Context: %s × %s", program_name, milestone_name))
  message(sprintf(
    "Extraction: %s | Rotation: %s | Factors: %d\n",
    fm,
    rotate,
    nfactors
  ))

  message("Fit Indices:")
  message(sprintf("  TLI: %.3f (%s)", fit_indices$TLI, fit_interpretation$TLI))
  message(sprintf(
    "  RMSEA: %.3f (%s)",
    fit_indices$RMSEA,
    fit_interpretation$RMSEA
  ))
  message(sprintf(
    "  RMSR: %.3f (%s)\n",
    fit_indices$RMSR,
    fit_interpretation$RMSR
  ))

  if (!is.null(results$problematic_items$low_communality)) {
    message(sprintf(
      "⚠️  %d item(s) with low communality (< 0.25):",
      length(results$problematic_items$low_communality)
    ))
    message(paste(
      "  ",
      paste(results$problematic_items$low_communality, collapse = ", ")
    ))
  }

  if (!is.null(results$problematic_items$cross_loadings)) {
    message(sprintf(
      "⚠️  %d item(s) with cross-loadings (> %.2f on multiple factors):",
      length(results$problematic_items$cross_loadings),
      cross_loading_threshold
    ))
    message(paste(
      "  ",
      paste(results$problematic_items$cross_loadings, collapse = ", ")
    ))
  }

  message("\n===========================\n")

  return(results)
}
