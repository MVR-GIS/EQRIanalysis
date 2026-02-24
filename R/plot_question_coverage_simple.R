#' @title Plot Question Coverage by Program Type
#' @description Simplified heatmap showing question coverage, separated by 
#'   program type for easier interpretation.
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param core_threshold integer; Minimum number of contexts for "core" 
#'   classification. Default 8.
#' @returns ggplot2 object
#' @export
#' 
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual labs 
#'   theme_minimal theme element_text facet_wrap
#' @importFrom dplyr %>% group_by summarize mutate case_when
#' @importFrom tidyr complete
#' 
plot_question_coverage_simple <- function(responses_df, core_threshold = 8) {
  
  # Calculate coverage
  question_context <- responses_df %>%
    group_by(QUESTION_NUMBER, PROGRAMTYPE_NAME, MILESTONE_DESC) %>%
    summarize(
      n_responses = n(),
      .groups = "drop"
    ) %>%
    complete(
      QUESTION_NUMBER = unique(responses_df$QUESTION_NUMBER),
      PROGRAMTYPE_NAME = unique(responses_df$PROGRAMTYPE_NAME),
      MILESTONE_DESC = unique(responses_df$MILESTONE_DESC),
      fill = list(n_responses = 0)
    ) %>%
    mutate(
      status = ifelse(n_responses > 0, "Asked", "Not Asked")
    )
  
  # Calculate core vs. context-specific
  question_summary <- question_context %>%
    group_by(QUESTION_NUMBER) %>%
    summarize(
      n_contexts = sum(n_responses > 0),
      .groups = "drop"
    ) %>%
    mutate(
      question_type = ifelse(n_contexts >= core_threshold, "Core", "Context-specific")
    )
  
  # Join and prepare for plotting
  plot_data <- question_context %>%
    left_join(question_summary, by = "QUESTION_NUMBER") %>%
    mutate(
      QUESTION_NUMBER = factor(QUESTION_NUMBER, 
                              levels = rev(sort(unique(QUESTION_NUMBER)))),
      # Shorten milestone names for display
      milestone_short = gsub("\\(.*\\)", "", MILESTONE_DESC)
    )
  
  # Create heatmap
  p <- ggplot(plot_data, aes(x = milestone_short, y = QUESTION_NUMBER, 
                             fill = status, alpha = question_type)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_manual(
      values = c("Asked" = "#2E86AB", "Not Asked" = "#f0f0f0"),
      name = "Question Status"
    ) +
    scale_alpha_manual(
      values = c("Core" = 1.0, "Context-specific" = 0.6),
      name = "Item Type"
    ) +
    facet_wrap(~ PROGRAMTYPE_NAME, ncol = 2) +
    labs(
      title = "Question Coverage by Context",
      subtitle = paste0("Core items appear in ≥", core_threshold, " contexts; context-specific items shown with transparency"),
      x = "Milestone",
      y = "Question Number"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid = element_blank()
    )
  
  return(p)
}