#' @title Plot Alluvial Diagrams for an Indicator by Milestones
#' @description Plots a set of Alluvial Diagrams for all project milestones.
#' @param indicator_name character; Questionnaire indicator name. One of:
#'                       `levels(get_indicators_df()$INDICATOR)`
#' @param program_name   character; Program name. One of:
#'                       `levels(get_indicators_df()$PROGRAMTYPE_NAME)`
#' @returns a ggplot2 object
#' @export
#' @importFrom dplyr %>% filter
#' @importFrom forcats fct_drop
#' @importFrom purrr map
#' @importFrom ggplot2 labs
#' @importFrom patchwork wrap_plots
plot_indicator_prog_mile <- function(indicator_name, program_name) {
  # Get alluvial data for this combo
  alluvial_df <- get_alluvial_df() %>%
    filter(
      INDICATOR == indicator_name,
      PROGRAMTYPE_NAME == program_name,
    )

  # Determine the Milestones in the data
  milestones <- levels(fct_drop(alluvial_df$MILESTONE_DESC))

  # Create list of plots
  plot_list <- map(milestones, function(milestone) {
    alluvial_plot <- plot_indicator_alluvial(
      indicator_name,
      program_name,
      milestone
    ) +
      labs(title = paste0(milestone))
  })

  plot <- wrap_plots(
    plot_list,
    nrow = length(milestones),
    guides = "collect"
  )
  plot
  return(plot)
}
