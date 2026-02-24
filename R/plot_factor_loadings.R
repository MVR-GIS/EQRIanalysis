#' @title Plot Factor Loading Heatmap
#' @description Visualize factor loadings as a heatmap to identify which
#'   questions load on which factors.
#' 
#' @param efa_result list; Output from run_efa()
#' @param use_clean logical; If TRUE, uses loadings_clean (suppresses small values)
#' @param color_scale character; "diverging" (default) or "sequential"
#' 
#' @returns ggplot2 object
#' @export
#' 
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2
#'   labs theme_minimal theme element_text coord_fixed
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' 
plot_factor_loadings <- function(efa_result,
                                 use_clean = TRUE,
                                 color_scale = "diverging") {
  
  # Extract loadings
  loadings_data <- if (use_clean) {
    efa_result$loadings_clean
  } else {
    efa_result$loadings
  }
  
  # Convert to long format for plotting
  loadings_long <- loadings_data %>%
    as.data.frame() %>%
    mutate(Item = rownames(.)) %>%
    pivot_longer(
      cols = -Item,
      names_to = "Factor",
      values_to = "Loading"
    )
  
  # Create plot
  p <- ggplot(loadings_long, aes(x = Factor, y = Item, fill = Loading)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = ifelse(!is.na(Loading), sprintf("%.2f", Loading), "")),
              size = 3, color = "black") +
    labs(
      title = "Factor Loading Matrix",
      subtitle = paste(
        efa_result$context$program, "×", efa_result$context$milestone,
        "|", efa_result$extraction_info$method, "extraction,",
        efa_result$extraction_info$rotation, "rotation"
      ),
      x = "Factor",
      y = "Item",
      fill = "Loading",
      caption = if (use_clean) {
        paste("Loadings <", efa_result$extraction_info$loading_threshold, "suppressed")
      } else {
        "All loadings shown"
      }
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    coord_fixed(ratio = 0.5)
  
  # Apply color scale
  if (color_scale == "diverging") {
    p <- p + scale_fill_gradient2(
      low = "#A23B72",     # Negative loadings (purple)
      mid = "#F0F0F0",     # Zero (light gray)
      high = "#2E86AB",    # Positive loadings (blue)
      midpoint = 0,
      na.value = "white",
      limits = c(-1, 1)
    )
  } else {
    p <- p + scale_fill_gradient(
      low = "#FFFFFF",
      high = "#2E86AB",
      na.value = "white"
    )
  }
  
  return(p)
}