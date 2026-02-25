#' @title Compare Factor Loadings Across Methods
#' @description Create a faceted plot comparing factor loadings from different
#'   extraction or rotation methods to assess robustness of factor structure.
#' 
#' @param efa_results list; Named list of EFA results from compare_efa_methods()
#' @param loading_threshold numeric; Minimum loading to display (default 0.40)
#' 
#' @returns ggplot2 object with faceted heatmaps
#' @export
#' 
#' @section References:
#'   Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J. (1999). 
#'   Evaluating the use of exploratory factor analysis in psychological research. 
#'   Psychological Methods, 4(3), 272-299.
#'   
#'   Per Fabrigar et al., testing multiple methods assesses robustness of 
#'   factor structure.
#' 
#' @importFrom ggplot2 ggplot aes geom_tile geom_text facet_wrap
#'   scale_fill_gradient2 labs theme_minimal theme element_text
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% mutate bind_rows
#' @importFrom purrr map_dfr
#' 
plot_efa_comparison <- function(efa_results,
                                loading_threshold = 0.40) {
  
  # Validate input
  if (!is.list(efa_results) || length(efa_results) == 0) {
    stop("efa_results must be a non-empty list of EFA results")
  }
  
  # Extract loadings from each method
  loadings_list <- lapply(names(efa_results), function(method_name) {
    result <- efa_results[[method_name]]
    
    if (is.null(result)) return(NULL)
    
    # Get loadings
    loadings_df <- result$loadings_clean %>%
      as.data.frame() %>%
      mutate(
        Item = rownames(.),
        Method = method_name
      ) %>%
      pivot_longer(
        cols = -c(Item, Method),
        names_to = "Factor",
        values_to = "Loading"
      )
    
    return(loadings_df)
  })
  
  # Combine all loadings
  all_loadings <- bind_rows(loadings_list[!sapply(loadings_list, is.null)])
  
  # Create faceted plot
  p <- ggplot(all_loadings, aes(x = Factor, y = Item, fill = Loading)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(
      aes(label = ifelse(!is.na(Loading), sprintf("%.2f", Loading), "")),
      size = 2,
      color = "black"
    ) +
    facet_wrap(~ Method, ncol = 2) +
    scale_fill_gradient2(
      low = "#A23B72",
      mid = "#F0F0F0",
      high = "#2E86AB",
      midpoint = 0,
      na.value = "white",
      limits = c(-1, 1),
      name = "Loading"
    ) +
    labs(
      title = "Factor Loading Comparison Across Methods",
      subtitle = paste("Loadings below", loading_threshold, "suppressed"),
      x = "Factor",
      y = "Item",
      caption = "Per Fabrigar et al. (1999): Consistent patterns indicate robust structure"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 6),
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  return(p)
}