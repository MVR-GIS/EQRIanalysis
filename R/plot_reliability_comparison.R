#' @title Plot Reliability Comparison Across Contexts
#' @description Create a comparative visualization of Cronbach's alpha and
#'   McDonald's omega across all program type × milestone combinations.
#' @param reliability_df data.frame; Output from run_reliability_analysis()
#' @param metric character; Which metric to plot? One of "alpha_std" (default),
#'   "omega_total", "both", or "difference"
#' @returns ggplot2 object
#' @export
#' 
#' @section Development Notes:
#'   This function was developed with AI assistance (GitHub Copilot, 2026-02-20).
#'   Human direction and oversight was provided at each implementation step. 
#'   See `dev/sessions/2026-02-20.md` for complete development context.
#' 
#' @importFrom ggplot2 ggplot aes geom_col geom_point geom_hline geom_text
#'   facet_grid scale_fill_manual scale_color_manual labs theme_minimal
#'   theme element_text element_blank coord_flip
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% filter mutate
#' @importFrom NatParksPalettes natparks.pals
#' 
plot_reliability_comparison <- function(reliability_df, metric = "alpha_std") {
  
  # Validate metric
  valid_metrics <- c("alpha_std", "omega_total", "both", "difference")
  if (!metric %in% valid_metrics) {
    stop(paste("metric must be one of:", paste(valid_metrics, collapse = ", ")))
  }
  
  # Define interpretation colors (following your NatParksPalettes convention)
  interpretation_colors <- c(
    "Poor" = "#8B0000",           # Dark red
    "Questionable" = "#FF6347",   # Tomato
    "Acceptable" = "#FFD700",     # Gold
    "Good" = "#90EE90",           # Light green
    "Excellent" = "#228B22"       # Forest green
  )
  
  # Create plot based on metric choice
  if (metric == "alpha_std") {
    # Cronbach's Alpha only
    p <- ggplot(reliability_df, aes(x = milestone, y = alpha_std, fill = alpha_interpretation)) +
      geom_col(color = "darkgrey", linewidth = 0.3) +
      geom_hline(yintercept = 0.70, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_text(aes(label = round(alpha_std, 2)), 
                vjust = -0.5, size = 3) +
      scale_fill_manual(
        values = interpretation_colors,
        name = "Interpretation",
        drop = FALSE
      ) +
      facet_grid(program ~ .) +
      labs(
        title = "Cronbach's Alpha Across Contexts",
        subtitle = "Dashed line at 0.70 = minimum acceptable threshold",
        x = "Milestone",
        y = "Cronbach's Alpha (Standardized)",
        caption = "Source: Questionnaire Reliability Analysis"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      ) +
      coord_flip()
    
  } else if (metric == "omega_total") {
    # McDonald's Omega only
    reliability_df_omega <- reliability_df %>%
      filter(!is.na(omega_total))
    
    p <- ggplot(reliability_df_omega, aes(x = milestone, y = omega_total, fill = alpha_interpretation)) +
      geom_col(color = "darkgrey", linewidth = 0.3) +
      geom_hline(yintercept = 0.70, linetype = "dashed", color = "black", linewidth = 0.5) +
      geom_text(aes(label = round(omega_total, 2)), 
                vjust = -0.5, size = 3) +
      scale_fill_manual(
        values = interpretation_colors,
        name = "Interpretation",
        drop = FALSE
      ) +
      facet_grid(program ~ .) +
      labs(
        title = "McDonald's Omega Across Contexts",
        subtitle = "Dashed line at 0.70 = minimum acceptable threshold",
        x = "Milestone",
        y = "McDonald's Omega (Total)",
        caption = "Source: Questionnaire Reliability Analysis"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      ) +
      coord_flip()
    
  } else if (metric == "both") {
    # Both Alpha and Omega side-by-side
    reliability_long <- reliability_df %>%
      filter(!is.na(omega_total)) %>%
      pivot_longer(
        cols = c(alpha_std, omega_total),
        names_to = "metric_type",
        values_to = "value"
      ) %>%
      mutate(
        metric_type = factor(
          metric_type,
          levels = c("alpha_std", "omega_total"),
          labels = c("Alpha", "Omega")
        )
      )
    
    p <- ggplot(reliability_long, aes(x = milestone, y = value, fill = metric_type)) +
      geom_col(position = "dodge", color = "darkgrey", linewidth = 0.3) +
      geom_hline(yintercept = 0.70, linetype = "dashed", color = "black", linewidth = 0.5) +
      scale_fill_manual(
        values = c("Alpha" = "#4682B4", "Omega" = "#CD853F"),
        name = "Metric"
      ) +
      facet_grid(program ~ .) +
      labs(
        title = "Alpha vs. Omega Comparison Across Contexts",
        subtitle = "Dashed line at 0.70 = minimum acceptable threshold",
        x = "Milestone",
        y = "Reliability Coefficient",
        caption = "Source: Questionnaire Reliability Analysis"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      ) +
      coord_flip()
    
  } else if (metric == "difference") {
    # Omega - Alpha difference (shows where alpha underestimates)
    reliability_df_diff <- reliability_df %>%
      filter(!is.na(omega_alpha_diff))
    
    p <- ggplot(reliability_df_diff, aes(x = milestone, y = omega_alpha_diff)) +
      geom_col(aes(fill = omega_alpha_diff > 0.05), 
               color = "darkgrey", linewidth = 0.3) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
      geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.5) +
      geom_text(aes(label = round(omega_alpha_diff, 3)), 
                vjust = -0.5, size = 3) +
      scale_fill_manual(
        values = c("FALSE" = "#D3D3D3", "TRUE" = "#FF6347"),
        name = "Substantial\nDifference",
        labels = c("No (≤0.05)", "Yes (>0.05)")
      ) +
      facet_grid(program ~ .) +
      labs(
        title = "Omega - Alpha Difference Across Contexts",
        subtitle = "Positive values indicate alpha underestimates reliability\nDashed line at 0.05 = threshold for meaningful difference",
        x = "Milestone",
        y = "Omega - Alpha Difference",
        caption = "Source: Questionnaire Reliability Analysis"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      ) +
      coord_flip()
  }
  
  return(p)
}