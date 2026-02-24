#' @title Compare EFA Across Multiple Methods
#' @description Run EFA with different extraction and rotation methods to
#'   assess robustness of factor structure.
#' 
#' @param responses_df data.frame; Output from get_responses_df()
#' @param program_name character; Program type
#' @param milestone_name character; Milestone
#' @param nfactors integer; Number of factors (if NULL, uses parallel analysis)
#' @param methods data.frame; Combinations of fm and rotate to test. If NULL,
#'   uses default combinations recommended by Fabrigar et al. (1999)
#' 
#' @returns list of EFA results, one per method combination
#' @export
#' 
#' @section References:
#'   Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J. (1999). 
#'   Evaluating the use of exploratory factor analysis in psychological research. 
#'   Psychological Methods, 4(3), 272-299.
#'   
#'   Per Fabrigar et al., testing multiple extraction/rotation combinations
#'   helps assess stability of factor structure.
#' 
#' @importFrom dplyr %>% bind_rows
#' 
compare_efa_methods <- function(responses_df,
                                program_name,
                                milestone_name,
                                nfactors = NULL,
                                methods = NULL) {
  
  # Default method combinations per Fabrigar et al. (1999)
  if (is.null(methods)) {
    methods <- data.frame(
      fm = c("minres", "ml", "pa", "minres", "ml"),
      rotate = c("oblimin", "oblimin", "oblimin", "promax", "promax"),
      stringsAsFactors = FALSE
    )
  }
  
  message(paste(
    "\nComparing", nrow(methods), "EFA method combinations for",
    program_name, "×", milestone_name, "\n"
  ))
  
  results_list <- list()
  
  for (i in 1:nrow(methods)) {
    fm_method <- methods$fm[i]
    rotate_method <- methods$rotate[i]
    method_name <- paste(fm_method, rotate_method, sep = "_")
    
    message(sprintf("\n[%d/%d] Running: %s extraction with %s rotation",
                    i, nrow(methods), fm_method, rotate_method))
    
    result <- tryCatch({
      run_efa(
        responses_df,
        program_name = program_name,
        milestone_name = milestone_name,
        nfactors = nfactors,
        fm = fm_method,
        rotate = rotate_method
      )
    }, error = function(e) {
      warning(paste("Method", method_name, "failed:", e$message))
      NULL
    })
    
    results_list[[method_name]] <- result
  }
  
  message("\n=== METHOD COMPARISON COMPLETE ===\n")
  
  return(results_list)
}