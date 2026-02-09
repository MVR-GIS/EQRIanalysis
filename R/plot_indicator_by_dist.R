#' @title Plot Indicator by USACE District
#' @description Plot indicator by USACE Division.
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
#' @importFrom forcats fct_drop
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes after_stat geom_histogram
#'             vars labs theme_grey theme
#' @importFrom ggh4x facet_grid2
#' @importFrom NatParksPalettes scale_fill_natparks_c
#' @importFrom patchwork wrap_plots
#'
plot_indicator_by_dist <- function(indicators_df, indicator_name) {
  # Extract the current indicator
  current_indicator <- indicators_df %>%
    filter(INDICATOR == indicator_name)

  # Determine the Divisions in the data
  divisions <- sort(levels(fct_drop(current_indicator$DIVISION)))

  # Create a list of plots
  plot_list <- map(divisions, function(division) {
    # Filter for the division
    current_indicator_div <- current_indicator %>%
      filter(DIVISION == division)
    
    ggplot(current_indicator_div, 
              aes(x = indicator_value,
                  fill = after_stat(x))) +
    geom_histogram(
      bins = 10, ,
      show.legend = FALSE) +
    scale_fill_natparks_c(name = "Arches", direction = -1) + 
    facet_grid2(
      rows = vars(DIVISION),
      cols = vars(DISTRICT),
      drop = TRUE,
      render_empty = TRUE
    ) +
    labs(x = "Score") +
    theme_grey(base_size = 8) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank()
    )
  })

  plot <- wrap_plots(
      plot_list, 
      nrow = length(divisions),
      guides = "auto"
    ) & 
    theme()
  plot  
  return(plot)
}
