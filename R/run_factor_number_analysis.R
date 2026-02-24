#' @title Run Factor Number Analysis Across Multiple Contexts
#' @description Determines optimal number of factors for specified contexts
#'   using parallel analysis and complementary methods.
#' 
#' @param responses_df data.frame; Output from get_responses_df()
#' @param contexts data.frame; Must have columns 'program' and 'milestone'
#' @param n.iter integer; Number of parallel analysis iterations (default 20)
#' @param fm character; Factor method (default "minres")
#' 
#' @returns list of results, one per context
#' @export
#' 
#' @importFrom dplyr %>%
#' @importFrom purrr map2
#' 
run_factor_number_analysis <- function(responses_df,
                                       contexts,
                                       n.iter = 20,
                                       fm = "minres") {
  
  if (!all(c("program", "milestone") %in% names(contexts))) {
    stop("contexts must have 'program' and 'milestone' columns")
  }
  
  message(paste("Analyzing", nrow(contexts), "contexts...\n"))
  
  results <- list()
  
  for (i in 1:nrow(contexts)) {
    program <- contexts$program[i]
    milestone <- contexts$milestone[i]
    
    message(paste("\n### Context", i, "of", nrow(contexts), "###"))
    
    result <- tryCatch({
      determine_n_factors(
        responses_df,
        program_name = program,
        milestone_name = milestone,
        n.iter = n.iter,
        fm = fm
      )
    }, error = function(e) {
      warning(paste("Analysis failed for", program, "×", milestone, ":", e$message))
      NULL
    })
    
    results[[paste(program, milestone, sep = " × ")]] <- result
  }
  
  message("\nFactor number analysis complete!")
  
  return(results)
}