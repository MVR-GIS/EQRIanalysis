#' @title Plot Factor Correlations
#' @description Visualize correlations between factors (only applicable for
#'   oblique rotations like oblimin or promax).
#' 
#' @param efa_result list; Output from run_efa() with oblique rotation
#' 
#' @returns ggplot2 object or NULL if orthogonal rotation
#' @export
#' 
#' @section Interpretation:
#'   High correlations between factors (|r| > 0.30) suggest they may represent
#'   related constructs. Very high correlations (|r| > 0.70) may indicate
#'   factors should be combined.
#' 
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2
#'   labs theme_minimal theme element_text coord_fixed
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate
#' 
plot_factor_correlations <- function(efa_result) {
  
  # Check if factor correlations exist
  if (is.null(efa_result$factor_correlations)) {
    message(paste(
      "No factor correlations available.",
      "Rotation method:", efa_result$extraction_info$rotation,
      "- Factor correlations only available for oblique rotations (oblimin, promax)."
    ))
    return(NULL)
  }
  
  # Extract correlation matrix
  phi <- efa_result$factor_correlations
  
  # Convert to long format
  phi_long <- as.data.frame(phi) %>%
    mutate(Factor1 = rownames(.)) %>%
    pivot_longer(
      cols = -Factor1,
      names_to = "Factor2",
      values_to = "Correlation"
    )
  
  # Create heatmap
  p <- ggplot(phi_long, aes(x = Factor2, y = Factor1, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.2f", Correlation)),
              size = 4, color = "black") +
    scale_fill_gradient2(
      low = "#A23B72",
      mid = "#FFFFFF",
      high = "#2E86AB",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    labs(
      title = "Factor Correlations",
      subtitle = paste(
        efa_result$context$program, "×", efa_result$context$milestone,
        "|", efa_result$extraction_info$rotation, "rotation"
      ),
      x = NULL,
      y = NULL,
      caption = "|r| > 0.30: Moderately related | |r| > 0.70: Highly related"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    coord_fixed()
  
  return(p)
}