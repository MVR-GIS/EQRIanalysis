#' @title Plot Alluvial Diagram for Specific Indicator-Program-Milestone Combo
#' @description Shows flow from individual question responses to indicator
#'   score for a specific combination of indicator, program, and project
#'   milestone.
#' @param indicator_name character; Questionnaire indicator name. One of:
#'                       `levels(get_indicators_df()$INDICATOR)`
#' @param program_name   character; Program name. One of:
#'                       `levels(get_indicators_df()$PROGRAMTYPE_NAME)`
#' @param milestone_name character; Milestone name. One of:
#'                       `levels(get_indicators_df()$MILESTONE_DESC)`
#' @returns A ggplot2 alluvial plot object
#' @export
#' @importFrom dplyr %>% filter mutate across select
#' @importFrom forcats fct_drop
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#' @importFrom easyalluvial alluvial_wide
#' @importFrom ggplot2 scale_x_discrete theme element_text labs
#' @importFrom stringr str_replace_all str_to_title str_wrap
#' @importFrom NatParksPalettes scale_fill_natparks_d
plot_indicator_alluvial <- function(
  indicator_name,
  program_name,
  milestone_name
) {
  # Get data
  questions_df <- get_questions_df()
  alluvial_df <- get_alluvial_df()

  # Filter for this combo
  alluvial_filtered_df <- alluvial_df %>%
    filter(
      INDICATOR == indicator_name,
      PROGRAMTYPE_NAME == program_name,
      MILESTONE_DESC == milestone_name
    ) %>%
    mutate(across(where(is.factor), fct_drop))

  questions_filtered_df <- questions_df %>%
    filter(QUESTION_SHORT %in%
           levels(alluvial_filtered_df$QUESTION_SHORT)) %>%
    mutate(across(where(is.factor), fct_drop))

  # Convert data to wide format (one questionnaireevent_id per row)
  alluvial_df_wide <- alluvial_filtered_df %>%
    select(
      QUESTIONNAIREEVENT_ID,
      PROGRAMTYPE_NAME,
      MILESTONE_DESC,
      QUESTION_SHORT,
      RESPONSE,
      INDICATOR_SCORE_BIN
    ) %>%
    tidyr::pivot_wider(
      names_from = QUESTION_SHORT,
      values_from = RESPONSE,
      id_cols = c(QUESTIONNAIREEVENT_ID, INDICATOR_SCORE_BIN)
    )
     
  # Generate hex codes from the palette to feed to alluvial_wide
  score_colors <- natparks.pals(
    name = "Arches",
    n = length(levels(alluvial_df$INDICATOR_SCORE_BIN)),
    type = "discrete",
    direction = -1
  )

  # Create a filterable dataframe of colors by indicator score levels
  colors_df <- tibble(
      score_colors = as.vector(score_colors),
      score_levels = levels(alluvial_df$INDICATOR_SCORE_BIN)
    ) %>% 
    filter(score_levels %in% 
      levels(alluvial_df_wide$INDICATOR_SCORE_BIN))

  # Create alluvial plot using easyalluvial
  p <- alluvial_wide(
    data = alluvial_df_wide,
    id = QUESTIONNAIREEVENT_ID,
    fill_by = "first_variable",
    col_vector_flow = colors_df$score_colors,
    NA_label = "NA",
    stratum_label_size = 3,
    auto_rotate_xlabs = FALSE
  ) +
  scale_x_discrete(labels = function(x) {
    x %>%
      str_replace_all("_", " ") %>%
      str_to_title() %>%    
      str_wrap(width = 0.7)
    }) +
  theme(axis.text.x = element_text(size = 8,
                                   lineheight = 0.8)) +
  labs(caption = NULL)         # Removes alluvial plot stats
  p
  return(p)
}
