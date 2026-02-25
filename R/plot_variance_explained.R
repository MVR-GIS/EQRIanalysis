#' @title Plot Variance Explained by Factors
#' @description Visualize proportion of variance explained by each factor and
#'   cumulative variance explained. Helps assess adequacy of factor solution.
#' 
#' @param efa_result list; Output from run_efa()
#' 
#' @returns ggplot2 object
#' @export
#' 
#' @section Interpretation:
#'   Per psychometric standards, factors should explain:
#'   - Cumulative variance > 50%: Adequate
#'   - Cumulative variance > 60%: Good
#'   - Individual factors > 10%: Meaningful
#' 
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point geom_text
#'   scale_y_continuous labs theme_minimal theme element_text
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr pivot_longer
#' 
plot_variance_explained <- function(efa_result) {
  
  # Extract variance accounted for
  variance_mat <- efa_result$variance_explained
  
  # Get proportion of variance row
  prop_var <- variance_mat["Proportion Var", ]
  cum_var <- variance_mat["Cumulative Var", ]
  
  # Create data frame
  var_df <- data.frame(
    Factor = paste0("MR", 1:length(prop_var)),
    Proportion = as.numeric(prop_var),
    Cumulative = as.numeric(cum_var)
  ) %>%
    mutate(
      Factor = factor(Factor, levels = Factor),  # Preserve order
      Proportion_pct = Proportion * 100,
      Cumulative_pct = Cumulative * 100
    )
  
  # Create plot
  p <- ggplot(var_df, aes(x = Factor)) +
    # Bars for individual variance
    geom_col(aes(y = Proportion_pct, fill = "Individual Factor"),
             alpha = 0.7, width = 0.6) +
    # Line for cumulative variance
    geom_line(aes(y = Cumulative_pct, color = "Cumulative", group = 1),
              linewidth = 1) +
    geom_point(aes(y = Cumulative_pct, color = "Cumulative"),
               size = 3) +
    # Add value labels
    geom_text(aes(y = Proportion_pct, label = sprintf("%.1f%%", Proportion_pct)),
              vjust = -0.5, size = 3) +
    geom_text(aes(y = Cumulative_pct, label = sprintf("%.1f%%", Cumulative_pct)),
              vjust = -1, size = 3, fontface = "bold") +
    # Scales
    scale_y_continuous(
      limits = c(0, max(var_df$Cumulative_pct) * 1.15),
      name = "Variance Explained (%)"
    ) +
    scale_fill_manual(
      values = c("Individual Factor" = "#2E86AB"),
      name = NULL
    ) +
    scale_color_manual(
      values = c("Cumulative" = "#A23B72"),
      name = NULL
    ) +
    # Labels
    labs(
      title = "Variance Explained by Factors",
      subtitle = paste(
        efa_result$context$program, "×", efa_result$context$milestone
      ),
      x = "Factor",
      caption = paste(
        "Total variance explained:",
        sprintf("%.1f%%", max(var_df$Cumulative_pct))
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}