#' @title Assess Data Factorability for Factor Analysis
#' @description Evaluate whether data are suitable for factor analysis.
#'   **IMPORTANT**: This function is designed for "core items" that are
#'   administered across contexts. Context-specific items (milestone-specific,
#'   program-specific) should be filtered out before calling this function.
#'   
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param program_name character; Program type or NULL for all programs
#' @param milestone_name character; Milestone or NULL for all milestones
#' @param filter_context_specific logical; If TRUE (default), automatically
#'   filters items with >20% missing data (likely context-specific per
#'   Schafer & Graham, 2002). If FALSE, user must ensure appropriate items.
#' @param max_missing_pct numeric; Maximum % missing to retain item (default 20).
#'   Per Schafer & Graham (2002), items >20% missing likely context-specific.
#' @param max_endorsement_pct numeric; Maximum % in dominant category (default 80).
#'   Per Nunnally & Bernstein (1994), items >80% lack discriminating power.
#' @param multicollinearity_threshold numeric; Correlation threshold (default 0.90)
#' @param use_polychoric logical; Attempt polychoric correlations (default TRUE)
#' 
#' @returns list containing:
#'   \itemize{
#'     \item context: Analysis context (program × milestone)
#'     \item sample: Sample size information and filtering results
#'     \item filtering: Documentation of which items were removed and why
#'     \item correlations: Correlation matrix, method, and diagnostics
#'     \item multicollinearity: High correlation pairs if present
#'     \item recommendation: Whether to proceed with FA and suggested parameters
#'   }
#'   
#' @export
#' 
#' @section Structural Missingness:
#'   Per Little & Rubin (2019, Statistical Analysis with Missing Data, Ch 1.4.3),
#'   questionnaires with **milestone-specific** or **program-specific** questions
#'   exhibit "missingness by design" - items are **not applicable** to all contexts.
#'   
#'   This function handles structural missingness by:
#'   \enumerate{
#'     \item Filtering context-specific items (>20% missing per Schafer & Graham, 2002)
#'     \item Analyzing remaining "core items" present across contexts
#'     \item Using listwise deletion on core items for valid correlation matrix
#'   }
#'   
#'   **DO NOT** attempt to impute structural missingness - it violates the
#'   missing data mechanism assumption.
#'   
#' @section Response Variability Requirements:
#'   Per Nunnally & Bernstein (1994, Psychometric Theory, p. 265), items should
#'   have balanced response distributions for stable correlations. This function
#'   removes items with >80% endorsement in the dominant category (default).
#'   
#'   Rationale:
#'   \itemize{
#'     \item Nunnally & Bernstein (1994): Recommend 80% threshold for stability
#'     \item Cohen (1988): Binary items need p between 0.10 and 0.90
#'     \item Flora & Curran (2004): Extreme distributions cause polychoric instability
#'   }
#'   
#' @references
#'   Little, R. J. A., & Rubin, D. B. (2019). Statistical Analysis with 
#'   Missing Data (3rd ed.). Wiley. Chapter 1.4.3: Missingness by Design.
#'   
#'   Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of 
#'   the state of the art. Psychological Methods, 7(2), 147-177.
#'   https://doi.org/10.1037/1082-989X.7.2.147
#'   
#'   Nunnally, J. C., & Bernstein, I. H. (1994). Psychometric Theory (3rd ed.).
#'   McGraw-Hill. Chapter 6: The Assessment of Reliability.
#'   
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
#'   Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences
#'   (2nd ed.). Lawrence Erlbaum. Chapter 6: The Pearson Product-Moment Correlation.
#'   
#'   Millsap, R. E. (2011). Statistical Approaches to Measurement Invariance.
#'   Routledge. Chapter 2: Measurement Invariance Concepts.
#'   
#'   Revelle, W. (2024). psych: Procedures for Psychological, Psychometric, 
#'   and Personality Research. Northwestern University.
#'   https://personality-project.org/r/psych/
#'   
#' @section Development Notes:
#'   This function was developed 2026-02-21 to 2026-02-22 with systematic
#'   attention to structural missingness, response variability, and correlation
#'   matrix validity per authoritative psychometric sources.
#'   See `dev/sessions/2026-02-21.md` and `dev/sessions/2026-02-22.md` for
#'   complete development context.
#' 
#' @importFrom psych polychoric tetrachoric
#' @importFrom dplyr case_when
#' 
#' @examples
#' \dontrun{
#' # Assess factorability on aggregated data (core items only)
#' responses_df <- get_responses_df()
#' 
#' result <- assess_factorability(
#'   responses_df,
#'   program_name = NULL,      # Aggregate all programs
#'   milestone_name = NULL     # Aggregate all milestones
#' )
#' 
#' # Check recommendation
#' print(result$recommendation$rationale)
#' 
#' # View correlation matrix
#' print(result$correlations$matrix[1:5, 1:5])
#' }
#' 
assess_factorability <- function(responses_df,
                                program_name = NULL,
                                milestone_name = NULL,
                                filter_context_specific = TRUE,
                                max_missing_pct = 20,
                                max_endorsement_pct = 80,
                                multicollinearity_threshold = 0.90,
                                use_polychoric = TRUE) {
  
  # ===========================================================================
  # SETUP: Get wide format data
  # ===========================================================================
  
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
  
  n_questions_original <- ncol(wide_data)
  n_observations_original <- nrow(wide_data)
  
  # ===========================================================================
  # STEP 0: Filter context-specific items
  # Per Little & Rubin (2019, Ch 1.4.3): Structural missingness by design
  # ===========================================================================
  
  context_specific_items <- character(0)
  
  if (filter_context_specific) {
    message("\n--- Step 0: Filtering context-specific items ---")
    message(sprintf(
      "Per Schafer & Graham (2002): Items with >%.0f%% missing likely context-specific",
      max_missing_pct
    ))
    
    missing_pct <- colMeans(is.na(wide_data)) * 100
    context_specific_items <- names(missing_pct)[missing_pct > max_missing_pct]
    
    if (length(context_specific_items) > 0) {
      message(paste(
        "\nRemoving", length(context_specific_items),
        "context-specific item(s):"
      ))
      
      for (item in context_specific_items) {
        message(sprintf("  %s: %.1f%% missing", item, missing_pct[item]))
      }
      
      message(paste(
        "\nPer Little & Rubin (2019, Ch 1.4.3 'Missingness by Design'):",
        "\nThese items are not universally applicable.",
        "\nThey should be analyzed within their specific contexts, not aggregated."
      ))
      
      wide_data <- wide_data[, !(names(wide_data) %in% context_specific_items), drop = FALSE]
    } else {
      message(sprintf(
        "No context-specific items detected (all items ≤%.0f%% missing)",
        max_missing_pct
      ))
    }
    
    message(sprintf("\nCore items remaining: %d", ncol(wide_data)))
  }
  
  # Check if we have enough items after filtering
  if (ncol(wide_data) < 3) {
    stop(paste(
      "After filtering context-specific items, only", ncol(wide_data), "question(s) remain.",
      "\nFactor analysis requires at least 3 items.",
      "\nContext-specific items removed:", length(context_specific_items),
      "\n\nPer Millsap (2011, Statistical Approaches to Measurement Invariance):",
      "\nWhen items vary by context, analyze contexts separately.",
      "\n\nRECOMMENDATION:",
      "\n  - Analyze each PROGRAMTYPE × MILESTONE context individually",
      "\n  - Use context-appropriate items for each analysis",
      "\n  - Test measurement invariance on common items in Phase 4"
    ))
  }
  
  # ===========================================================================
  # STEP 1: Listwise deletion on core items
  # Per R cor() documentation: use="everything" requires complete cases
  # Per Little & Rubin (2019): Listwise deletion on filtered set
  # ===========================================================================
  
  message("\n--- Step 1: Complete case analysis on core items ---")
  message(sprintf(
    "Core items: %d (%.1f%% of original %d items)",
    ncol(wide_data),
    100 * ncol(wide_data) / n_questions_original,
    n_questions_original
  ))
  
  complete_rows <- complete.cases(wide_data)
  n_complete <- sum(complete_rows)
  
  message(sprintf(
    "\nComplete cases: %d / %d (%.1f%%)",
    n_complete, n_observations_original, 100 * n_complete / n_observations_original
  ))
  
  # Check minimum sample size
  if (n_complete < 10) {
    stop(paste(
      "Only", n_complete, "complete case(s) for core items.",
      "\nInsufficient for factor analysis (minimum 10 required).",
      "\n\nPer Flora & Curran (2004):",
      "\n  - Polychoric FA requires N ≥ 200 for stable estimates",
      "\n  - Pearson FA requires N ≥ 100 minimum",
      "\n\nCurrent complete cases:", n_complete,
      "\n\nRECOMMENDATION:",
      "\n  - Aggregate across more contexts (if theoretically justified)",
      "\n  - OR analyze contexts separately with context-specific items"
    ))
  }
  
  # Warn if losing substantial data
  pct_retained <- n_complete / n_observations_original
  if (pct_retained < 0.5) {
    warning(paste(
      "Listwise deletion removes", n_observations_original - n_complete, "cases",
      sprintf("(%.1f%% of sample).", 100 * (1 - pct_retained)),
      "\n\nPer Enders (2010, Applied Missing Data Analysis):",
      "\nDeleting >50% may introduce bias if data not MCAR.",
      "\nConsider:",
      "\n  - Examining why data are missing in core items",
      "\n  - Whether aggregation across contexts is appropriate"
    ))
  }
  
  # Apply listwise deletion
  wide_data <- wide_data[complete_rows, , drop = FALSE]
  
  message(sprintf(
    "Proceeding with %d complete cases for %d core items",
    nrow(wide_data), ncol(wide_data)
  ))
  
  # ===========================================================================
  # STEP 2: Filter by response variability
  # Per Nunnally & Bernstein (1994, p. 265): >80% endorsement insufficient
  # Per Cohen (1988): Binary items need 10-90% split
  # ===========================================================================
  
  message("\n--- Step 2: Screening for response variability ---")
  
  item_stats <- lapply(names(wide_data), function(item_name) {
    x <- wide_data[[item_name]]
    
    var_val <- var(x, na.rm = TRUE)
    n_unique <- length(unique(x[!is.na(x)]))
    
    # Check proportion in dominant category
    freq_table <- table(x, useNA = "no")
    if (length(freq_table) > 0) {
      max_prop <- max(freq_table) / sum(freq_table)
    } else {
      max_prop <- 1
    }
    
    list(
      item = item_name,
      variance = var_val,
      n_unique = n_unique,
      max_category_pct = max_prop * 100
    )
  })
  
  stats_df <- do.call(rbind, lapply(item_stats, as.data.frame))
  
  # Identify low-variability items
  low_var_items <- stats_df[stats_df$max_category_pct > max_endorsement_pct, ]$item
  
  if (length(low_var_items) > 0) {
    message(paste(
      "\nRemoving", length(low_var_items),
      "item(s) with insufficient variability (>", max_endorsement_pct, "% endorsement):"
    ))
    
    for (item in low_var_items) {
      item_stat <- stats_df[stats_df$item == item, ]
      message(sprintf(
        "  %s: var=%.4f, dominant=%.1f%%",
        item, item_stat$variance, item_stat$max_category_pct
      ))
    }
    
    message(paste(
      "\nPer Nunnally & Bernstein (1994, Psychometric Theory, p. 265):",
      "\n  Items with >80% in one category lack discriminating power.",
      "\nPer Cohen (1988, Statistical Power Analysis, Ch 6):",
      "\n  Binary items need 10-90% split for stable correlations."
    ))
    
    wide_data <- wide_data[, !(names(wide_data) %in% low_var_items), drop = FALSE]
  } else {
    message(sprintf(
      "All core items have sufficient variability (≤%.0f%% endorsement)",
      max_endorsement_pct
    ))
  }
  
  # Check minimum items after all filtering
  if (ncol(wide_data) < 3) {
    stop(paste(
      "After filtering, only", ncol(wide_data), "item(s) remain.",
      "\nFactor analysis requires ≥ 3 items.",
      "\n\nFiltered items:",
      "\n  - Context-specific (>", max_missing_pct, "% missing):", length(context_specific_items),
      "\n  - Low variability (>", max_endorsement_pct, "% endorsement):", length(low_var_items),
      "\n  - Total removed:", length(context_specific_items) + length(low_var_items),
      "\n  - Retained:", ncol(wide_data),
      "\n\nThis suggests questionnaire items are:",
      "\n  1. Highly context-dependent (milestone/program-specific)",
      "\n  2. Have limited response variation in aggregated sample",
      "\n\nRECOMMENDATION: Analyze contexts separately rather than aggregating."
    ))
  }
  
  message(sprintf(
    "\nItems with sufficient variability: %d",
    ncol(wide_data)
  ))
  
  # ===========================================================================
  # STEP 3: Compute correlation matrix
  # Per Holgado-Tello et al. (2010): Polychoric preferred for ordinal data
  # Use use="everything" since we've already done listwise deletion
  # ===========================================================================
  
  message("\n--- Step 3: Computing correlation matrix ---")
  
  correlation_method <- "none"
  cor_matrix <- NULL
  tau <- NULL
  
  # Check if all items are binary (for tetrachoric vs polychoric)
  n_categories_per_item <- sapply(wide_data, function(x) {
    length(unique(x[!is.na(x)]))
  })
  all_binary <- all(n_categories_per_item == 2)
  
  if (use_polychoric) {
    if (all_binary) {
      message("All items binary - using tetrachoric correlations")
      message("(Per Revelle, 2024, psych package documentation)")
      
      cor_result <- tryCatch({
        psych::tetrachoric(wide_data, correct = TRUE)
      }, error = function(e) {
        warning(paste(
          "\nTetrachoric correlation failed:", e$message,
          "\nFalling back to Pearson correlations."
        ))
        NULL
      })
      
      if (!is.null(cor_result)) {
        cor_matrix <- cor_result$rho
        tau <- cor_result$tau
        
        # Check for NAs
        if (any(is.na(cor_matrix))) {
          n_na <- sum(is.na(cor_matrix))
          warning(paste(
            "\nTetrachoric matrix contains", n_na, "NA value(s).",
            "\nFalling back to Pearson."
          ))
          cor_matrix <- NULL
        } else {
          correlation_method <- "tetrachoric"
          message("Successfully computed tetrachoric correlations")
        }
      }
      
    } else {
      message("Items are ordinal/mixed - using polychoric correlations")
      message("(Per Holgado-Tello et al., 2010)")
      
      cor_result <- tryCatch({
        psych::polychoric(wide_data)
      }, error = function(e) {
        warning(paste(
          "\nPolychoric correlation failed:", e$message,
          "\nFalling back to Pearson correlations."
        ))
        NULL
      })
      
      if (!is.null(cor_result)) {
        cor_matrix <- cor_result$rho
        tau <- cor_result$tau
        
        # Check for NAs
        if (any(is.na(cor_matrix))) {
          n_na <- sum(is.na(cor_matrix))
          warning(paste(
            "\nPolychoric matrix contains", n_na, "NA value(s).",
            "\nThis may indicate sparse contingency tables.",
            "\nFalling back to Pearson."
          ))
          cor_matrix <- NULL
        } else {
          correlation_method <- "polychoric"
          message("Successfully computed polychoric correlations")
        }
      }
    }
  }
  
  # Fallback to Pearson if polychoric/tetrachoric failed or not requested
  if (is.null(cor_matrix)) {
    message("\n--- Computing Pearson correlations ---")
    message("(Note: Less appropriate for ordinal data per Holgado-Tello et al., 2010)")
    
    # Use "everything" since we've done listwise deletion
    # Per R cor() documentation
    cor_matrix <- cor(wide_data, use = "everything")
    correlation_method <- "pearson"
    
    # Sanity check
    if (any(is.na(cor_matrix))) {
      stop(paste(
        "Correlation matrix contains NA values even after listwise deletion.",
        "\nThis should not happen. Please report as a bug with your data context:",
        "\n  Program:", ifelse(is.null(program_name), "All", program_name),
        "\n  Milestone:", ifelse(is.null(milestone_name), "All", milestone_name),
        "\n  Items:", ncol(wide_data),
        "\n  Complete cases:", nrow(wide_data)
      ))
    }
    
    message("Successfully computed Pearson correlations")
  }
  
  # ===========================================================================
  # STEP 4: Check multicollinearity
  # Document high correlations but DO NOT remove items
  # Per project domain: High correlations expected in best practice assessments
  # ===========================================================================
  
  message("\n--- Step 4: Diagnosing multicollinearity ---")
  
  cor_matrix_check <- cor_matrix
  diag(cor_matrix_check) <- 0  # Exclude self-correlations
  
  # Find high correlation pairs
  high_cor_indices <- which(
    abs(cor_matrix_check) > multicollinearity_threshold,
    arr.ind = TRUE
  )
  
  # Remove duplicates (matrix is symmetric)
  if (nrow(high_cor_indices) > 0) {
    high_cor_indices <- high_cor_indices[
      high_cor_indices[, 1] < high_cor_indices[, 2],
      ,
      drop = FALSE
    ]
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
    
    # Show first 5 pairs
    n_to_show <- min(5, nrow(pair_data))
    for (i in 1:n_to_show) {
      message(sprintf(
        "  %s × %s: r = %.3f",
        pair_data$item1[i], pair_data$item2[i], pair_data$correlation[i]
      ))
    }
    
    if (nrow(pair_data) > 5) {
      message(sprintf("  ... and %d more pair(s)", nrow(pair_data) - 5))
    }
    
    message(paste(
      "\nNote: Per your project domain (best practice assessment):",
      "\nHigh multicollinearity is EXPECTED.",
      "\nRelated best practices naturally correlate.",
      "\nItems are DOCUMENTED but NOT removed.",
      "\nWill use ridge-regularized FA to handle this (correct > 0 in fa.poly)."
    ))
    
    multicollinear_pairs <- pair_data
  } else {
    message("No problematic multicollinearity detected")
  }
  
  # ===========================================================================
  # STEP 5: Check correlation matrix properties
  # Determinant and invertibility
  # ===========================================================================
  
  message("\n--- Step 5: Checking correlation matrix properties ---")
  
  cor_det <- det(cor_matrix)
  
  # Defensive check
  if (is.na(cor_det)) {
    stop(paste(
      "Correlation matrix determinant is NA.",
      "\nThis should not occur after all filtering and validation.",
      "\nPlease report as a bug with your data context:",
      "\n  Program:", ifelse(is.null(program_name), "All", program_name),
      "\n  Milestone:", ifelse(is.null(milestone_name), "All", milestone_name),
      "\n  Items:", ncol(wide_data),
      "\n  Complete cases:", nrow(wide_data)
    ))
  }
  
  message(paste("Determinant:", format(cor_det, scientific = TRUE)))
  
  can_invert <- TRUE
  if (cor_det < 1e-10) {
    message("WARNING: Determinant very small (near-singular matrix)")
    message("This indicates substantial multicollinearity")
    message("Recommendation: Use ridge-regularized FA (correct > 0 in fa.poly)")
    can_invert <- FALSE
  } else {
    message("Matrix determinant is acceptable for inversion")
  }
  
  # ===========================================================================
  # STEP 6: Compile results
  # Complete results structure with all diagnostic information
  # ===========================================================================
  
  message("\n--- Summary ---")
  message(sprintf("Items analyzed: %d", ncol(wide_data)))
  message(sprintf("Complete cases: %d", nrow(wide_data)))
  message(sprintf("Correlation method: %s", correlation_method))
  message(sprintf("Items filtered:"))
  message(sprintf("  - Context-specific: %d", length(context_specific_items)))
  message(sprintf("  - Low variability: %d", length(low_var_items)))
  message(sprintf("  - Total retained: %d of %d original", ncol(wide_data), n_questions_original))
  
  # Construct removed items list
  all_removed_items <- list(
    context_specific = if (length(context_specific_items) > 0) {
      context_specific_items
    } else {
      NULL
    },
    low_variability = if (length(low_var_items) > 0) {
      low_var_items
    } else {
      NULL
    }
  )
  
  # Determine recommendation
  proceed_with_fa <- (ncol(wide_data) >= 3 && nrow(wide_data) >= 10)
  use_ridge <- (cor_det < 1e-10 || nrow(high_cor_indices) > 0)
  
  suggested_ridge <- if (cor_det < 1e-10) {
    0.1  # Strong regularization for near-singular matrix
  } else if (nrow(high_cor_indices) > 10) {
    0.05  # Moderate regularization for many high correlations
  } else if (nrow(high_cor_indices) > 0) {
    0.01  # Mild regularization for some high correlations
  } else {
    0     # No regularization needed
  }
  
  # Generate rationale
  rationale <- dplyr::case_when(
    ncol(wide_data) < 3 ~ "Insufficient items for factor analysis",
    nrow(wide_data) < 10 ~ "Insufficient sample size",
    nrow(wide_data) < 100 ~ "Sample size below recommended N=100 minimum",
    nrow(wide_data) < 200 && correlation_method %in% c("polychoric", "tetrachoric") ~ 
      "Sample below recommended N=200 for polychoric/tetrachoric FA, but may proceed with caution",
    !can_invert ~ "Use ridge-regularized FA due to near-singular correlation matrix",
    nrow(high_cor_indices) > 0 ~ 
      "Proceed with ridge-regularized FA to handle multicollinearity",
    TRUE ~ "Data are suitable for factor analysis"
  )
  
  # Generate notes if needed
  notes <- if (correlation_method == "pearson") {
    paste(
      "Using Pearson correlations instead of polychoric/tetrachoric.",
      "Per Holgado-Tello et al. (2010), polychoric is preferred for ordinal data.",
      "Consider investigating why polychoric/tetrachoric failed."
    )
  } else if (length(context_specific_items) > 0 || length(low_var_items) > 0) {
    paste(
      "Analysis focuses on", ncol(wide_data), "core items",
      "with sufficient coverage and variability.",
      "\nFiltered:", length(context_specific_items), "context-specific,",
      length(low_var_items), "low-variability.",
      "\nPer Little & Rubin (2019), this is appropriate for structural missingness."
    )
  } else {
    NULL
  }
  
  # Compile results
  results <- list(
    context = list(
      program = ifelse(is.null(program_name), "All", program_name),
      milestone = ifelse(is.null(milestone_name), "All", milestone_name),
      label = context_label
    ),
    sample = list(
      n_questions_original = n_questions_original,
      n_questions_analyzed = ncol(wide_data),
      n_observations_original = n_observations_original,
      n_observations_complete = nrow(wide_data),
      pct_complete = 100 * nrow(wide_data) / n_observations_original
    ),
    filtering = list(
      n_context_specific = length(context_specific_items),
      n_low_variability = length(low_var_items),
      n_total_removed = length(context_specific_items) + length(low_var_items),
      removed_items = all_removed_items,
      thresholds = list(
        max_missing_pct = max_missing_pct,
        max_endorsement_pct = max_endorsement_pct
      )
    ),
    correlations = list(
      method = correlation_method,
      matrix = cor_matrix,
      tau = tau,  # Thresholds for ordinal categories (NULL if Pearson)
      determinant = cor_det,
      can_invert = can_invert
    ),
    multicollinearity = list(
      n_pairs_high = nrow(high_cor_indices),
      threshold = multicollinearity_threshold,
      high_cor_pairs = multicollinear_pairs
    ),
    recommendation = list(
      proceed_with_fa = proceed_with_fa,
      use_ridge = use_ridge,
      suggested_ridge = suggested_ridge,
      rationale = rationale,
      notes = notes
    )
  )
  
  # Print recommendation
  message(paste("\n=== RECOMMENDATION ==="))
  message(rationale)
  
  if (use_ridge) {
    message(sprintf(
      "\nSuggested ridge correction: %.2f",
      suggested_ridge
    ))
    message("(Use 'correct' parameter in psych::fa.poly or psych::fa)")
  }
  
  if (!is.null(notes)) {
    message(paste("\nNOTE:", notes))
  }
  
  return(results)
}