#' @title Calculate Cronbach's Alpha for Specific Context
#' @description Compute Cronbach's alpha reliability coefficient for questions
#'   within a specific indicator, program type, and milestone combination.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param indicator_name character; Indicator to analyze
#' @param program_name character; Program type ("Military" or "Civil Works")
#' @param milestone_name character; Project milestone
#' @param check.keys logical; Auto-reverse negatively correlated items? Default TRUE.
#' @returns list containing alpha statistics and interpretation
#' @export
#' @importFrom psych alpha
#' @importFrom dplyr case_when
#' 
calculate_cronbach_alpha <- function(responses_df, 
                                     indicator_name,
                                     program_name,
                                     milestone_name,
                                     check.keys = TRUE) {
  
  # Get wide format data for this specific context
  wide_data <- get_wide_responses(
    responses_df, 
    indicator_name,
    program_name,
    milestone_name
  )
  
  # Check for sufficient data
  if (ncol(wide_data) < 2) {
    stop(paste("Indicator", indicator_name, "has fewer than 2 items in this context. Alpha requires at least 2 items."))
  }
  
  if (nrow(wide_data) < 10) {
    warning(paste("Small sample size (n =", nrow(wide_data), 
                  "). Results may be unstable."))
  }
  
  # Calculate Cronbach's alpha
  alpha_result <- psych::alpha(wide_data, 
                               check.keys = check.keys,
                               warnings = TRUE)
  
  # Extract key results
  results <- list(
    indicator = indicator_name,
    program = program_name,
    milestone = milestone_name,
    n_items = ncol(wide_data),
    n_observations = nrow(wide_data),
    alpha_raw = alpha_result$total$raw_alpha,
    alpha_std = alpha_result$total$std.alpha,
    average_r = alpha_result$total$average_r,
    item_statistics = alpha_result$item.stats,
    alpha_if_dropped = alpha_result$alpha.drop,
    full_output = alpha_result
  )
  
  # Add interpretation
  results$interpretation <- case_when(
    results$alpha_std >= 0.90 ~ "Excellent",
    results$alpha_std >= 0.80 ~ "Good",
    results$alpha_std >= 0.70 ~ "Acceptable",
    results$alpha_std >= 0.60 ~ "Questionable",
    TRUE ~ "Poor"
  )
  
  return(results)
}