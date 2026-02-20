#' @title Plot Sample Sizes Across Contexts
#' @description Visualize the number of questionnaire events (observations)
#'   and questions analyzed in each program type × milestone context.
#' @param reliability_df data.frame; Output from run_reliability_analysis()
#' @returns ggplot2 object
#' @export
#' 
#' @section Development Notes:
#'   This function was developed with AI assistance (GitHub Copilot, 2026-02-20).
#'   Human direction and oversight was provided at each implementation step. 
#'   See `dev/sessions/2026-02-20.md` for complete development context.
#' 
#' @importFrom ggplot2 ggplot aes geom_col geom_text facet_grid labs
#'   theme_minimal theme element_text coord_flip
#' @importFrom dplyr %>%
#' 
plot_sample_sizes <- function(reliability_df) {
  
  p <- ggplot(reliability_df, aes(x = milestone, y = n_observations)) +
    geom_col(fill = "#4682B4", color = "darkgrey", linewidth = 0.3) +
    geom_text(aes(label = n_observations), vjust = -0.5, size = 3) +
    geom_hline(yintercept = 100, linetype = "dashed", 
               color = "darkgreen", linewidth = 0.5) +
    geom_hline(yintercept = 30, linetype = "dashed", 
               color = "orange", linewidth = 0.5) +
    facet_grid(program ~ .) +
    labs(
      title = "Sample Sizes Across Contexts",
      subtitle = "Green line: n=100 (excellent)\nOrange line: n=30 (minimum adequate)",
      x = "Milestone",
      y = "Number of Questionnaire Events (n)",
      caption = "Larger samples produce more stable reliability estimates"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    ) +
    coord_flip()
  
  return(p)
}