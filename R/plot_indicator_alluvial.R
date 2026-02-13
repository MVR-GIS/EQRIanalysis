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
#' @importFrom dplyr %>% select
#' @importFrom tidyr pivot_wider
#' @importFrom easyalluvial alluvial_wide
#' @importFrom ggplot2 theme labs
#' @importFrom NatParksPalettes scale_fill_natparks_d
plot_indicator_alluvial <- function(
  indicator_name,
  program_name,
  milestone_name
) {
  # Get alluvial data for this combo
  alluvial_df <- get_alluvial_df() %>%
    filter(
      INDICATOR == indicator_name,
      PROGRAMTYPE_NAME == program_name,
      MILESTONE_DESC == milestone_name
    )

  # Prepare data in wide format (one row per event)
  alluvial_df_wide <- alluvial_df %>%
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
  my_colors <- natparks.pals(
    name = "Arches", 
    n = length(levels(alluvial_df_wide$INDICATOR_SCORE_BIN)), 
    type = "discrete",
    direction = -1
  )

  # Create alluvial plot using easyalluvial
  p <- alluvial_wide(
    data = alluvial_df_wide,
    id = QUESTIONNAIREEVENT_ID,
    bin_labels = levels(alluvial_df_wide$INDICATOR_SCORE_BIN),
    fill_by = "first_variable",
    col_vector_flow = my_colors, 
    col_vector_value = my_colors,
    NA_label = "NA",
    stratum_label_size = 3,
    auto_rotate_xlabs = FALSE
  ) +
    labs(caption = NULL) # Removes alluvial plot stats
  p
  return(p)
}
