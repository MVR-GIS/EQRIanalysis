#' @title Create Reliability Summary Table
#' @description Generate a formatted summary table of reliability metrics
#'   suitable for reporting or publication.
#' @param reliability_df data.frame; Output from run_reliability_analysis()
#' @param include_omega logical; Include omega columns? Default TRUE.
#' @returns data.frame formatted for display
#' @export
#' 
#' @section Development Notes:
#'   This function was developed with AI assistance (GitHub Copilot, 2026-02-20).
#'   Human direction and oversight was provided at each implementation step. 
#'   See `dev/sessions/2026-02-20.md` for complete development context.
#' 
#' @importFrom dplyr %>% select mutate arrange
#' 
create_reliability_table <- function(reliability_df, include_omega = TRUE) {
  
  if (include_omega) {
    table_df <- reliability_df %>%
      select(
        Program = program,
        Milestone = milestone,
        `n` = n_observations,
        `Questions` = n_questions_analyzed,
        `Alpha` = alpha_std,
        `Omega` = omega_total,
        `Interpretation` = alpha_interpretation
      ) %>%
      mutate(
        Alpha = round(Alpha, 3),
        Omega = round(Omega, 3)
      ) %>%
      arrange(Program, Milestone)
  } else {
    table_df <- reliability_df %>%
      select(
        Program = program,
        Milestone = milestone,
        `n` = n_observations,
        `Questions` = n_questions_analyzed,
        `Alpha` = alpha_std,
        `Interpretation` = alpha_interpretation
      ) %>%
      mutate(
        Alpha = round(Alpha, 3)
      ) %>%
      arrange(Program, Milestone)
  }
  
  return(table_df)
}