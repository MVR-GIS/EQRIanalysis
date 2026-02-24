#' @title Determine Optimal Number of Factors
#' @description Uses multiple psychometric criteria to determine the optimal
#'   number of factors for exploratory factor analysis, including parallel
#'   analysis (Horn, 1965), scree plot, VSS (Revelle & Rocklin, 1979), and
#'   MAP test (Velicer, 1976).
#' 
#' @param responses_df data.frame; Output from get_responses_df()
#' @param program_name character; Program type (e.g., "Military", "Civil Works")
#' @param milestone_name character; Milestone (e.g., "100% (Corrected Final Design)")
#' @param n.iter integer; Number of simulated datasets for parallel analysis 
#'   (default 20 per psych::fa.parallel documentation)
#' @param fm character; Factor method: "minres" (default), "ml", "pa", etc.
#'   Per psych::fa() documentation
#' @param max_factors integer; Maximum number of factors to test (default 10)
#' 
#' @returns list containing:
#'   \item{context}{Program and milestone information}
#'   \item{sample}{Sample size and number of items}
#'   \item{parallel_analysis}{Results from Horn's parallel analysis}
#'   \item{vss}{Very Simple Structure results}
#'   \item{map_test}{Minimum Average Partial results}
#'   \item{scree_plot}{ggplot2 scree plot object}
#'   \item{recommendations}{Summary of recommended number of factors}
#' 
#' @export
#' 
#' @section References:
#'   Horn, J. L. (1965). A rationale and test for the number of factors in 
#'   factor analysis. Psychometrika, 30(2), 179-185.
#'   
#'   Revelle, W., & Rocklin, T. (1979). Very simple structure: An alternative 
#'   procedure for estimating the optimal number of interpretable factors. 
#'   Multivariate Behavioral Research, 14(4), 403-414.
#'   
#'   Velicer, W. F. (1976). Determining the number of components from the 
#'   matrix of partial correlations. Psychometrika, 41(3), 321-327.
#'   
#'   Hayton, J. C., Allen, D. G., & Scarpello, V. (2004). Factor retention 
#'   decisions in exploratory factor analysis: A tutorial on parallel analysis. 
#'   Organizational Research Methods, 7(2), 191-205.
#' 
#' @importFrom psych fa.parallel VSS nfactors
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline
#'   labs theme_minimal theme element_text
#' 
determine_n_factors <- function(responses_df,
                                program_name,
                                milestone_name,
                                n.iter = 20,
                                fm = "minres",
                                max_factors = 10) {
  
  # Get wide format data
  wide_data <- get_wide_responses(
    responses_df,
    program_name,
    milestone_name
  )
  
  # Remove zero-variance items (same preprocessing as reliability functions)
  item_variances <- sapply(wide_data, var, na.rm = TRUE)
  zero_var_items <- names(item_variances)[item_variances == 0 | is.na(item_variances)]
  
  if (length(zero_var_items) > 0) {
    message(paste(
      "\n[Factor Analysis] Removing", length(zero_var_items),
      "zero-variance item(s):", paste(zero_var_items, collapse = ", ")
    ))
    wide_data <- wide_data[, item_variances > 0 & !is.na(item_variances), drop = FALSE]
  }
  
  # Check minimum requirements
  n_items <- ncol(wide_data)
  n_obs <- nrow(wide_data)
  
  if (n_items < 3) {
    stop(paste(
      "Factor analysis requires at least 3 items.",
      "Context has", n_items, "items after removing zero-variance questions."
    ))
  }
  
  # Adjust max_factors if necessary
  max_factors <- min(max_factors, n_items - 1, floor(n_obs / 5))
  
  message(paste(
    "\nDetermining optimal number of factors for:",
    program_name, "×", milestone_name,
    "\nItems:", n_items,
    "\nObservations:", n_obs,
    "\nTesting up to", max_factors, "factors\n"
  ))
  
  # 1. PARALLEL ANALYSIS (Horn, 1965) - PRIMARY METHOD
  message("Running parallel analysis (Horn, 1965)...")
  
  pa_result <- tryCatch({
    psych::fa.parallel(
      wide_data,
      fm = fm,
      fa = "fa",        # Factor analysis (not PCA)
      n.iter = n.iter,
      main = paste(program_name, "×", milestone_name),
      show.legend = TRUE,
      error.bars = FALSE
    )
  }, error = function(e) {
    warning(paste("Parallel analysis failed:", e$message))
    NULL
  })
  
  # 2. VSS (Revelle & Rocklin, 1979)
  message("Running Very Simple Structure analysis...")
  
  vss_result <- tryCatch({
    psych::VSS(
      wide_data,
      n = max_factors,
      fm = fm,
      plot = FALSE
    )
  }, error = function(e) {
    warning(paste("VSS analysis failed:", e$message))
    NULL
  })
  
  # 3. COMPREHENSIVE CRITERIA via nfactors()
  message("Running comprehensive factor criteria (MAP, BIC, SABIC)...")
  
  nfac_result <- tryCatch({
    psych::nfactors(
      wide_data,
      n = max_factors,
      fm = fm
    )
  }, error = function(e) {
    warning(paste("nfactors analysis failed:", e$message))
    NULL
  })
  
  # 4. CREATE SCREE PLOT
  scree_data <- if (!is.null(pa_result)) {
    data.frame(
      factor = 1:length(pa_result$fa.values),
      observed = pa_result$fa.values,
      simulated_mean = pa_result$fa.sim,
      simulated_pct95 = pa_result$fa.simr
    )
  } else {
    NULL
  }
  
  scree_plot <- if (!is.null(scree_data)) {
    ggplot(scree_data, aes(x = factor)) +
      geom_line(aes(y = observed, color = "Observed Data"), 
                linewidth = 1) +
      geom_point(aes(y = observed, color = "Observed Data"), 
                 size = 3) +
      geom_line(aes(y = simulated_mean, color = "Simulated Data (Mean)"), 
                linewidth = 0.8, linetype = "dashed") +
      geom_line(aes(y = simulated_pct95, color = "Simulated Data (95th %ile)"),
                linewidth = 0.8, linetype = "dotted") +
      geom_hline(yintercept = 1.0, linetype = "dotdash", 
                 color = "gray50", linewidth = 0.5) +
      scale_color_manual(
        values = c(
          "Observed Data" = "#2E86AB",
          "Simulated Data (Mean)" = "#A23B72",
          "Simulated Data (95th %ile)" = "#F18F01"
        ),
        name = NULL
      ) +
      labs(
        title = paste("Scree Plot with Parallel Analysis"),
        subtitle = paste(program_name, "×", milestone_name),
        x = "Factor Number",
        y = "Eigenvalue",
        caption = "Retain factors where observed eigenvalues > simulated eigenvalues"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
      )
  } else {
    NULL
  }
  
  # 5. COMPILE RECOMMENDATIONS
  recommendations <- list()
  
  # Parallel analysis recommendation
  if (!is.null(pa_result)) {
    recommendations$parallel_analysis <- pa_result$nfact
    recommendations$parallel_analysis_note <- 
      "Horn's parallel analysis (1965) - gold standard method"
  }
  
  # VSS recommendation
  if (!is.null(vss_result)) {
    vss_complexity_1 <- which.max(vss_result$vss.stats$VSS.complexity.1)
    vss_complexity_2 <- which.max(vss_result$vss.stats$VSS.complexity.2)
    recommendations$vss_complexity1 <- vss_complexity_1
    recommendations$vss_complexity2 <- vss_complexity_2
    recommendations$vss_note <- 
      "VSS (Revelle & Rocklin, 1979) - very simple structure"
  }
  
  # MAP test recommendation
  if (!is.null(nfac_result)) {
    recommendations$map_test <- which.min(nfac_result$map)
    recommendations$map_note <- 
      "MAP test (Velicer, 1976) - minimum average partial"
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
      max_factors_tested = max_factors
    ),
    parallel_analysis = if (!is.null(pa_result)) {
      list(
        n_factors_suggested = pa_result$nfact,
        eigenvalues_observed = pa_result$fa.values,
        eigenvalues_simulated = pa_result$fa.sim,
        n_iterations = n.iter
      )
    } else NULL,
    vss = if (!is.null(vss_result)) {
      list(
        vss_stats = vss_result$vss.stats,
        map_values = vss_result$map,
        complexity_1_max = which.max(vss_result$vss.stats$VSS.complexity.1),
        complexity_2_max = which.max(vss_result$vss.stats$VSS.complexity.2)
      )
    } else NULL,
    nfactors = nfac_result,
    scree_plot = scree_plot,
    recommendations = recommendations
  )
  
  # Print summary
  message("\n=== FACTOR NUMBER RECOMMENDATIONS ===")
  message(sprintf("Context: %s × %s", program_name, milestone_name))
  message(sprintf("Sample: n=%d, items=%d\n", n_obs, n_items))
  
  if (!is.null(recommendations$parallel_analysis)) {
    message(sprintf("Parallel Analysis: %d factors (Horn, 1965)",
                    recommendations$parallel_analysis))
  }
  if (!is.null(recommendations$vss_complexity1)) {
    message(sprintf("VSS Complexity 1: %d factors",
                    recommendations$vss_complexity1))
  }
  if (!is.null(recommendations$vss_complexity2)) {
    message(sprintf("VSS Complexity 2: %d factors",
                    recommendations$vss_complexity2))
  }
  if (!is.null(recommendations$map_test)) {
    message(sprintf("MAP Test: %d factors (Velicer, 1976)",
                    recommendations$map_test))
  }
  
  message("\n======================================\n")
  
  return(results)
}