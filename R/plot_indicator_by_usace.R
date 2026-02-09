#' @title Plot Indicator for all USACE
#' @description Plot indicator for all USACE.
#' @param indicators_df     data.frame; A data frame of questionnaire events
#'                         with indicator scores returned by 
#'                         `get_indicators_df`.
#' @param indicator_name   character; The indicator name to be ploted. One of
#'                         `levels(get_responses_df()$INDICATOR)`: 
#'                         "Confidence", "Cost", "QA", "QC", "Schedule", 
#'                         "Scope", "Team".
#' @returns a ggplot2 object
#' @export
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 ggplot aes after_stat geom_histogram
#'             labs theme_grey theme 
#' @importFrom NatParksPalettes scale_fill_natparks_c
#'
plot_indicator_by_usace <- function(indicators_df, indicator_name) {
  # Extract the current indicator
  current_indicator <- indicators_df %>%
    filter(INDICATOR == indicator_name)

  p <- ggplot(current_indicator, 
              aes(x = indicator_value,
                  fill = after_stat(x))) +
    geom_histogram(
      bins = 10, ,
      show.legend = FALSE) +
    scale_fill_natparks_c(name = "Arches", direction = -1) + 
    labs(x = "Score") +
    theme_grey(base_size = 10) +
    theme(
      legend.title = element_blank()
    )
  #p
  return(p)
}
