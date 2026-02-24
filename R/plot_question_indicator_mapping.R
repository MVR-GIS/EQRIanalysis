#' @title Plot Question-to-Indicator Mapping
#' @description Visualize which questions contribute to each indicator score.
#'   Shows the mapping between individual questions and the seven EQRI indicators
#'   (Confidence, Cost, QA, QC, Schedule, Scope, Team).
#' @param responses_df data.frame; Responses from get_responses_df()
#' @param sort_by character; How to sort questions. Options: "indicator" 
#'   (group by indicator, default), "question" (numeric order), "count" 
#'   (by frequency of use)
#' @returns ggplot2 object
#' @export
#' 
#' @section Interpretation:
#'   - Each row represents a question
#'   - Each column represents an indicator
#'   - Colored cells show which questions contribute to which indicators
#'   - The count shows how many responses exist for that question-indicator pair
#'   
#' @section Use Cases:
#'   This visualization helps identify:
#'   - Which questions define each indicator
#'   - Questions that contribute to multiple indicators (if any)
#'   - The relative contribution of each question to indicator scores
#'   
#' @references
#'   Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. 
#'   Springer-Verlag New York. https://ggplot2.tidyverse.org
#'   
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_manual labs 
#'   theme_minimal theme element_text coord_flip
#' @importFrom dplyr %>% group_by summarize n_distinct mutate arrange desc
#' @importFrom tidyr complete
#' 
plot_question_indicator_mapping <- function(responses_df, sort_by = "indicator") {
  
  # Calculate question-indicator mapping
  question_indicator <- responses_df %>%
    group_by(QUESTION_NUMBER, INDICATOR) %>%
    summarize(
      n_responses = n(),
      .groups = "drop"
    ) %>%
    # Complete the grid to show all combinations
    complete(
      QUESTION_NUMBER = unique(responses_df$QUESTION_NUMBER),
      INDICATOR = unique(responses_df$INDICATOR),
      fill = list(n_responses = 0)
    ) %>%
    mutate(
      mapped = ifelse(n_responses > 0, "Yes", "No")
    )
  
  # Calculate question summary for sorting
  question_summary <- responses_df %>%
    group_by(QUESTION_NUMBER) %>%
    summarize(
      indicator = first(INDICATOR),
      total_responses = n(),
      .groups = "drop"
    )
  
  # Join and apply sorting
  plot_data <- question_indicator %>%
    left_join(question_summary %>% select(QUESTION_NUMBER, indicator, total_responses),
              by = "QUESTION_NUMBER") %>%
    mutate(
      # Create ordering variable based on sort_by parameter
      question_order = case_when(
        sort_by == "indicator" ~ paste(indicator, 
                                      sprintf("%03d", as.numeric(QUESTION_NUMBER))),
        sort_by == "count" ~ sprintf("%010d", desc(total_responses)),
        TRUE ~ sprintf("%03d", as.numeric(QUESTION_NUMBER))
      ),
      # Factor for plotting
      QUESTION_NUMBER = factor(QUESTION_NUMBER, 
                              levels = unique(QUESTION_NUMBER[order(question_order)]))
    )
  
  # Count questions per indicator for annotation
  indicator_counts <- responses_df %>%
    group_by(INDICATOR) %>%
    summarize(
      n_questions = n_distinct(QUESTION_NUMBER),
      .groups = "drop"
    )
  
  # Create heatmap
  p <- ggplot(plot_data, aes(x = INDICATOR, y = QUESTION_NUMBER, fill = mapped)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_manual(
      values = c("No" = "#f0f0f0", "Yes" = "#2E86AB"),
      name = "Contributes to\nIndicator",
      breaks = c("No", "Yes")
    ) +
    labs(
      title = "Question-to-Indicator Mapping",
      subtitle = "Which questions contribute to each of the 7 EQRI indicators",
      x = "Indicator",
      y = "Question Number",
      caption = paste0("Sorted by: ", sort_by)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "right",
      panel.grid = element_blank()
    )
  
  return(p)
}