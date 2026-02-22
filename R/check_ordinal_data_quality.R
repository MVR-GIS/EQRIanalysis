#' @title Check Data Quality for Polychoric Correlations
#' @description Diagnostic function to assess whether data meet requirements for
#'   stable polychoric correlation estimation. Implements checks based on
#'   Flora & Curran (2004) sample size recommendations.
#' @param wide_data data.frame; Wide format data (output from get_wide_responses)
#' @returns list with diagnostic information, item-level details, and recommendations
#' @export
#' @references
#'   Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of 
#'   alternative methods of estimation for confirmatory factor analysis 
#'   with ordinal data. Psychological Methods, 9(4), 466-491.
#'   https://doi.org/10.1037/1082-989X.9.4.466
#'   Table 2 (page 475) provides sample size recommendations by number of
#'   response categories.
#'   
#' @section Interpretation:
#'   Per Flora & Curran (2004), minimum sample sizes for polychoric correlations:
#'   - 2 categories (dichotomous): N = 500
#'   - 3 categories (trichotomous): N = 400
#'   - 4 categories: N = 300 (interpolated)
#'   - 5+ categories: N = 200
#'   
#'   Additionally, all response categories should have minimum cell count >= 5
#'   to avoid sparse contingency tables.
#'   
#' @section Development Notes:
#'   This function was developed 2026-02-22 to provide proactive diagnostics
#'   before attempting polychoric correlation estimation.
#'   See `dev/sessions/2026-02-22.md` for complete development context.
#' 
#' @importFrom dplyr case_when
#' 
check_ordinal_data_quality <- function(wide_data) {
  
  if (!is.data.frame(wide_data)) {
    stop("wide_data must be a data.frame")
  }
  
  n_obs <- nrow(wide_data)
  n_items <- ncol(wide_data)
  
  message(paste(
    "\n=== Checking Ordinal Data Quality ===",
    "\nObservations:", n_obs,
    "\nItems:", n_items
  ))
  
  # Check response categories per item
  item_diagnostics <- lapply(names(wide_data), function(item_name) {
    item_data <- wide_data[[item_name]]
    
    # Count unique values (excluding NA)
    unique_vals <- unique(item_data[!is.na(item_data)])
    n_categories <- length(unique_vals)
    
    # Get frequency table
    category_table <- table(item_data, useNA = "no")
    min_cell_count <- if (length(category_table) > 0) min(category_table) else 0
    max_cell_count <- if (length(category_table) > 0) max(category_table) else 0
    
    # Required N per Flora & Curran (2004) Table 2
    required_n <- case_when(
      n_categories <= 2 ~ 500,
      n_categories == 3 ~ 400,
      n_categories == 4 ~ 300,
      n_categories >= 5 ~ 200,
      TRUE ~ 300  # Fallback
    )
    
    # Check if item meets criteria
    sufficient_n <- (n_obs >= required_n)
    sufficient_cells <- (min_cell_count >= 5)
    
    list(
      item = item_name,
      n_categories = n_categories,
      min_cell_count = min_cell_count,
      max_cell_count = max_cell_count,
      required_n = required_n,
      sufficient_n = sufficient_n,
      sufficient_cells = sufficient_cells,
      overall_sufficient = (sufficient_n && sufficient_cells)
    )
  })
  
  # Compile to data frame
  diagnostics_df <- do.call(rbind, lapply(item_diagnostics, as.data.frame))
  
  # Overall assessment
  n_insufficient_n <- sum(!diagnostics_df$sufficient_n)
  n_insufficient_cells <- sum(!diagnostics_df$sufficient_cells)
  n_overall_insufficient <- sum(!diagnostics_df$overall_sufficient)
  
  # Determine overall recommendation
  if (n_overall_insufficient == 0) {
    recommendation <- "Data appear suitable for polychoric correlation estimation."
    overall_sufficient <- TRUE
  } else {
    issues <- character()
    if (n_insufficient_n > 0) {
      issues <- c(issues, paste(n_insufficient_n, "items have insufficient sample size"))
    }
    if (n_insufficient_cells > 0) {
      issues <- c(issues, paste(n_insufficient_cells, "items have sparse categories (min count < 5)"))
    }
    
    recommendation <- paste(
      paste(issues, collapse = " and "), ".",
      "\nPer Flora & Curran (2004), consider:",
      "\n  1. Aggregating across more contexts to increase N",
      "\n  2. Collapsing response categories (e.g., merge rarely-used categories)",
      "\n  3. Using Pearson correlations instead (less optimal for ordinal data)"
    )
    overall_sufficient <- FALSE
  }
  
  # Print summary
  message("\n--- Quality Diagnostic Summary ---")
  message(paste("Items meeting criteria:", n_items - n_overall_insufficient, "/", n_items))
  if (n_insufficient_n > 0) {
    message(paste("  Insufficient sample size:", n_insufficient_n))
  }
  if (n_insufficient_cells > 0) {
    message(paste("  Sparse categories:", n_insufficient_cells))
  }
  message(paste("\n", recommendation))
  
  # Return results
  list(
    n_observations = n_obs,
    n_items = n_items,
    item_diagnostics = diagnostics_df,
    n_items_insufficient_n = n_insufficient_n,
    n_items_insufficient_cells = n_insufficient_cells,
    n_items_overall_insufficient = n_overall_insufficient,
    overall_sufficient = overall_sufficient,
    recommendation = recommendation
  )
}