#' @title Create Summary Table of Factor Number Recommendations
#' @description Formats factor number analysis results into a summary table
#' 
#' @param factor_results list; Output from run_factor_number_analysis()
#' 
#' @returns data.frame with recommendations by context
#' @export
#' 
#' @importFrom dplyr bind_rows
#' 
create_factor_number_table <- function(factor_results) {
  
  rows <- list()
  
  for (context_name in names(factor_results)) {
    result <- factor_results[[context_name]]
    
    if (is.null(result)) next
    
    row <- data.frame(
      Context = context_name,
      N = result$sample$n_observations,
      Items = result$sample$n_items,
      `Parallel Analysis` = ifelse(
        !is.null(result$recommendations$parallel_analysis),
        result$recommendations$parallel_analysis,
        NA
      ),
      `VSS-1` = ifelse(
        !is.null(result$recommendations$vss_complexity1),
        result$recommendations$vss_complexity1,
        NA
      ),
      `MAP Test` = ifelse(
        !is.null(result$recommendations$map_test),
        result$recommendations$map_test,
        NA
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    rows[[context_name]] <- row
  }
  
  bind_rows(rows)
}