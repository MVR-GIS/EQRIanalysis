#' @title Plot Alluvial Diagram for Indicator Questions
#' @description Shows flow from individual question responses to indicator score
#' @param indicator_name Character; one of the indicator names
#' @returns A ggplot2 alluvial plot object
#' @export
#' @importFrom easyalluvial alluvial_wide
#' @importFrom dplyr %>% select
#' @importFrom tidyr pivot_wider
plot_indicator_alluvial <- function(indicator_name) {
  
  # Get alluvial data for this indicator
  data <- get_alluvial_df(indicator_filter = indicator_name)
  
  # Prepare data in wide format (one row per event)
  data_wide <- data %>%
    select(QUESTIONNAIREEVENT_ID, QUESTION_SHORT, RESPONSE, INDICATOR_SCORE_BIN) %>%
    tidyr::pivot_wider(
      names_from = QUESTION_SHORT,
      values_from = RESPONSE,
      id_cols = c(QUESTIONNAIREEVENT_ID, INDICATOR_SCORE_BIN)
    )
  
  # Create alluvial plot using easyalluvial
  p <- alluvial_wide(
    data = data_wide,
    id = QUESTIONNAIREEVENT_ID,
    col_vector_flow = names(data_wide)[3:(ncol(data_wide)-1)],  # Question columns
    col_vector_value = "INDICATOR_SCORE_BIN",
    fill_by = "last_variable",
    NA_label = "Missing",
    verbose = TRUE
  )
  p
  return(p)
}