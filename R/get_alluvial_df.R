#' @title Get Alluvial Data for Question-Indicator Flow
#' @description Prepare data showing how question responses flow to indicator scores
#' @param indicator_filter Optional filter for specific indicator(s)
#' @returns A data frame formatted for alluvial plotting
#' @export
#' @importFrom dplyr %>% select mutate group_by left_join
get_alluvial_df <- function(indicator_filter = NULL) {
  # Get base data
  responses_df <- get_responses_df()
  indicators_df <- get_indicators_df()

  # Create binned indicator scores (for categorical visualization)
  # indicator_value range is 0-1, divided into 5 equal bins
  indicators_binned <- indicators_df %>%
    mutate(
      INDICATOR_SCORE_BIN = cut(
        indicator_value,
        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
        labels = c("Very Low (0.0-0.2)", 
                   "Low (0.2-0.4)", 
                   "Moderate (0.4-0.6)", 
                   "High (0.6-0.8)", 
                   "Very High (0.8-1.0)"),
        include.lowest = TRUE,
        right = TRUE
      )
    )

  # Join responses with binned indicators
  alluvial_df <- responses_df %>%
    select(
      QUESTIONNAIREEVENT_ID,
      QUESTION_NUMBER,
      QUESTION_SHORT,
      INDICATOR,
      RESPONSE,
      RESPONSEVALUE
    ) %>%
    left_join(
      indicators_binned %>%
        select(QUESTIONNAIREEVENT_ID, INDICATOR, 
               indicator_value, INDICATOR_SCORE_BIN),
      by = c("QUESTIONNAIREEVENT_ID", "INDICATOR")
    )

  # Apply filter if specified
  if (!is.null(indicator_filter)) {
    alluvial_df <- alluvial_df %>%
      filter(INDICATOR %in% indicator_filter)
  }

  return(alluvial_df)
}
