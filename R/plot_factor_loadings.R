#' @title Plot Factor Loading Heatmap
#' @description Visualize factor loadings as a heatmap showing which questions
#'   load on which factors. Follows best practice from Costello & Osborne (2005)
#'   of suppressing loadings below 0.40 for clarity.
#' 
#' @param efa_result list; Output from run_efa()
#' @param use_clean logical; If TRUE (default), uses loadings_clean (suppresses
#'   small values per loading_threshold). If FALSE, shows all loadings.
#' @param color_scale character; "diverging" (default) for bipolar loadings, or
#'   "sequential" for magnitude only
#' @param text_size numeric; Size of loading text labels (default 3)
#' @param show_values logical; If TRUE (default), display numeric values on tiles
#' 
#' @returns ggplot2 object
#' @export
#' 
#' @section References:
#'   Costello, A. B., & Osborne, J. (2005). Best practices in exploratory 
#'   factor analysis: Four recommendations for getting the most from your analysis. 
#'   Practical Assessment, Research, and Evaluation, 10(7), 1-9.
#'   
#'   Revelle, W. (2024). psych: Procedures for Psychological, Psychometric, 
#'   and Personality Research. R package.
#' 
#' @section Interpretation:
#'   - **Color intensity**: Indicates strength of loading
#'   - **Blue/positive**: Item positively related to factor
#'   - **Purple/negative**: Item negatively related to factor (if using diverging scale)
#'   - **White/blank**: Loading below threshold (not meaningful)
#'   - **Loadings > 0.40**: Meaningful per Costello & Osborne (2005)
#'   - **Loadings > 0.70**: Excellent per psychometric standards
#' 
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2
#'   scale_fill_gradient labs theme_minimal theme element_text coord_fixed
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate
#' 
plot_factor_loadings <- function(efa_result,
                                 use_clean = TRUE,
                                 color_scale = "diverging",
                                 text_size = 3,
                                 show_values = TRUE) {
  
  # Validate inputs
  if (!is.list(efa_result)) {
    stop("efa_result must be a list (output from run_efa())")
  }
  
  if (!all(c("loadings", "loadings_clean") %in% names(efa_result))) {
    stop("efa_result must contain 'loadings' and 'loadings_clean' elements")
  }
  
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
  
  # Create base plot
  p <- ggplot(loadings_long, aes(x = Factor, y = Item, fill = Loading)) +
    geom_tile(color = "white", linewidth = 0.5)
  
  # Add text labels if requested
  if (show_values) {
    p <- p + geom_text(
      aes(label = ifelse(!is.na(Loading), sprintf("%.2f", Loading), "")),
      size = text_size,
      color = "black"
    )
  }
  
  # Apply color scale
  if (color_scale == "diverging") {
    # Diverging scale for positive/negative loadings
    p <- p + scale_fill_gradient2(
      low = "#A23B72",     # Purple for negative loadings
      mid = "#F0F0F0",     # Light gray for zero
      high = "#2E86AB",    # Blue for positive loadings
      midpoint = 0,
      na.value = "white",
      limits = c(-1, 1),
      name = "Loading"
    )
  } else {
    # Sequential scale (magnitude only)
    p <- p + scale_fill_gradient(
      low = "#FFFFFF",
      high = "#2E86AB",
      na.value = "white",
      limits = c(0, 1),
      name = "Loading"
    )
  }
  
  # Add labels and theme
  p <- p + labs(
    title = "Factor Loading Matrix",
    subtitle = paste(
      efa_result$context$program, "×", efa_result$context$milestone,
      "|", toupper(efa_result$extraction_info$method), "extraction,",
      efa_result$extraction_info$rotation, "rotation"
    ),
    x = "Factor",
    y = "Item",
    caption = if (use_clean) {
      paste("Loadings below", efa_result$extraction_info$loading_threshold, 
            "suppressed per Costello & Osborne (2005)")
    } else {
      "All loadings shown"
    }
  ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 10),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    coord_fixed(ratio = 0.5)  # Compress vertically for readability
  
  return(p)
}