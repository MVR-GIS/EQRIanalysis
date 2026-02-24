#' @title Plot Detailed Question-to-Indicator Mapping with Counts
#' @description Visualize question-indicator mapping with response counts shown.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param show_counts logical; If TRUE (default), display response counts in cells
#' @param min_count integer; Minimum responses to display count label (default 10)
#' @returns ggplot2 object
#' @export
#' 
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient labs 
#'   theme_minimal theme element_text
#' @importFrom dplyr %>% group_by summarize arrange
#' @importFrom scales comma
#' 
plot_question_indicator_detailed <- function(responses_df, 
                                            show_counts = TRUE,
                                            min_count = 10) {
  
  # Calculate question-indicator mapping with counts
  question_indicator <- responses_df %>%
    group_by(QUESTION_NUMBER, INDICATOR) %>%
    summarize(
      n_responses = n(),
      .groups = "drop"
    )
  
  # Order questions by their primary indicator, then by question number
  question_order <- responses_df %>%
    group_by(QUESTION_NUMBER) %>%
    summarize(
      primary_indicator = first(INDICATOR),
      .groups = "drop"
    ) %>%
    arrange(primary_indicator, as.numeric(QUESTION_NUMBER)) %>%
    pull(QUESTION_NUMBER)
  
  # Prepare plot data
  plot_data <- question_indicator %>%
    mutate(
      QUESTION_NUMBER = factor(QUESTION_NUMBER, levels = rev(question_order)),
      count_label = ifelse(n_responses >= min_count, 
                          scales::comma(n_responses), 
                          "")
    )
  
  # Create heatmap with gradient
  p <- ggplot(plot_data, aes(x = INDICATOR, y = QUESTION_NUMBER)) +
    geom_tile(aes(fill = n_responses), color = "white", linewidth = 0.5) +
    scale_fill_gradient(
      low = "#e0f3ff",
      high = "#2E86AB",
      name = "Response\nCount",
      labels = scales::comma
    ) +
    labs(
      title = "Question Contribution to Indicators",
      subtitle = "Response counts show how frequently each question informs each indicator",
      x = "Indicator",
      y = "Question Number"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  # Add counts if requested
  if (show_counts) {
    p <- p + geom_text(aes(label = count_label), size = 2.5, color = "white")
  }
  
  return(p)
}