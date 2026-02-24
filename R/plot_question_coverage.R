#' @title Plot Question Coverage Across Contexts
#' @description Visualize which questions are asked in each program type × 
#'   milestone context using a heatmap. This helps identify core items (asked 
#'   broadly) vs. context-specific items (asked selectively).
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param core_threshold integer; Minimum number of contexts for "core" 
#'   classification. Default 8 (per Schafer & Graham, 2002: <20% missing)
#' @returns ggplot2 object
#' @export
#' 
#' @section Interpretation:
#'   - **Dark cells**: Question asked in that context (data present)
#'   - **Light/white cells**: Question not asked in that context (missing by design)
#'   - **Horizontal patterns**: Core items (asked broadly across contexts)
#'   - **Sparse patterns**: Context-specific items
#'   
#' @references
#'   Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of the 
#'   state of the art. Psychological Methods, 7(2), 147-177.
#'   
#'   Little, R. J. A., & Rubin, D. B. (2019). Statistical Analysis with 
#'   Missing Data (3rd ed.). Wiley. Chapter 1.4.3: Structural missingness.
#'   
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient labs 
#'   theme_minimal theme element_text coord_flip facet_grid unit
#' @importFrom dplyr %>% group_by summarize n_distinct mutate case_when ungroup
#' @importFrom tidyr complete
#' 
plot_question_coverage <- function(responses_df, core_threshold = 8) {
  
  # Calculate coverage: which questions appear in which contexts
  question_context <- responses_df %>%
    group_by(QUESTION_NUMBER, PROGRAMTYPE_NAME, MILESTONE_DESC) %>%
    summarize(
      n_responses = n(),
      .groups = "drop"
    ) %>%
    # Complete the grid to show all question × context combinations
    complete(
      QUESTION_NUMBER = unique(responses_df$QUESTION_NUMBER),
      PROGRAMTYPE_NAME = unique(responses_df$PROGRAMTYPE_NAME),
      MILESTONE_DESC = unique(responses_df$MILESTONE_DESC),
      fill = list(n_responses = 0)
    ) %>%
    mutate(
      present = ifelse(n_responses > 0, 1, 0)
    )
  
  # Calculate total contexts per question for classification
  question_summary <- question_context %>%
    group_by(QUESTION_NUMBER) %>%
    summarize(
      n_contexts = sum(present),
      .groups = "drop"
    ) %>%
    mutate(
      question_type = case_when(
        n_contexts >= core_threshold ~ "Core",
        n_contexts > 0 ~ "Context-specific",
        TRUE ~ "Not used"
      )
    )
  
  # Join back to main data
  plot_data <- question_context %>%
    left_join(question_summary, by = "QUESTION_NUMBER") %>%
    mutate(
      # Create context label
      context = paste(PROGRAMTYPE_NAME, MILESTONE_DESC, sep = "\n"),
      # Factor for ordering
      QUESTION_NUMBER = factor(QUESTION_NUMBER, 
                              levels = rev(sort(unique(QUESTION_NUMBER))))
    )
  
  # Create heatmap  
  p <- ggplot(plot_data, aes(x = context, y = QUESTION_NUMBER, fill = present)) +
  geom_tile(color = "white", linewidth = 0.3, height = 0.8) +  # Add height parameter
  scale_fill_gradient(
    low = "#f0f0f0",
    high = "#2E86AB",
    breaks = c(0, 1),
    labels = c("Not Asked", "Asked"),
    name = "Status"
  ) +
  facet_grid(
    rows = vars(question_type),
    scales = "free_y",
    space = "free_y"  # This maintains proportional spacing
  ) +
  labs(
    title = "Question Coverage Across Contexts",
    subtitle = paste0("Core items (≥", core_threshold, " of 10 contexts) vs. Context-specific items"),
    x = "Context (Program Type × Milestone)",
    y = "Question Number",
    caption = "Per Little & Rubin (2019): Structural missingness by design"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 6),  # Reduce from 7
    plot.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    panel.grid = element_blank(),
    panel.spacing.y = unit(0.5, "lines")  # Reduce space between facets
  )
  
  return(p)
}