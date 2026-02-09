#' @title Render Indicator
#' @description Render the markdown output for an indicator.
#' @param indicators_df     data.frame; A data frame of questionnaire events
#'                         with indicator scores returned by 
#'                         `get_indicators_df`.
#' @param indicator_name   character; The indicator name to be ploted. One of
#'                         `levels(get_responses_df()$INDICATOR)`: 
#'                         "Confidence", "Cost", "QA", "QC", "Schedule", 
#'                         "Scope", "Team".
#' @returns a markdown string representing the output for each indicator
#' @export
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 ggsave
#'
render_indicator <- function(indicators_df, indicator_name) {
  # Extract the current indicator
  current_indicator <- indicators_df %>%
    filter(INDICATOR == indicator_name)

  if (nrow(current_indicator) == 0) {
    stop("Indicator name not found in the indicator dataset.")
  }

  # Generate the USACE plot
  plot_usace_path <- paste0("plots/indicators/i_", indicator_name, "_usace.svg")
  ggsave(
    filename = plot_usace_path,
    plot = plot_indicator_by_usace(
      indicators_df,
      indicator_name
    ),
    width = 8,
    height = 2,
    units = "in",
    dpi = 150,
    create.dir = TRUE
  )
  # Generate the type plot
  plot_type_path <- paste0("plots/indicators/i_", indicator_name, "_type.svg")
  ggsave(
    filename = plot_type_path,
    plot = plot_indicator_by_type(
      indicators_df,
      indicator_name
    ),
    width = 8,
    height = 2,
    units = "in",
    dpi = 150,
    create.dir = TRUE
  )
  # Generate the type plot
  plot_div_path <- paste0("plots/indicators/i_", indicator_name, "_div.svg")
  ggsave(
    filename = plot_div_path,
    plot = plot_indicator_by_div(
      indicators_df,
      indicator_name
    ),
    width = 8,
    height = 2,
    units = "in",
    dpi = 150,
    create.dir = TRUE
  )

  # Create the Markdown string
  markdown <- c(
    paste0(
      "## ",
      indicator_name
    ),
    "",
    paste0("![](", plot_usace_path, ")"),
    "",
    '::: {.graph title="by USACE Program" collapse=true}',
    paste0("![](", plot_type_path, ")"),
    ":::",
    "",
    '::: {.graph title="by Division" collapse=true}',
    paste0("![](", plot_div_path, ")"),
    ":::",
    "",
    # '::: {.graph title="by District" collapse=true}',
    # paste0("![](", plot_dist_path, ")"),
    # ":::",
    # "",
    # '::: {.graph title="by Design Team" collapse=true}',
    # paste0("![](", plot_design_team_path, ")"),
    # ":::",
    # "",
    # '::: {.graph title="by Design Strategy" collapse=true}',
    # paste0("![](", plot_design_strat_path, ")"),
    # ":::",
    ""
  )

  return(markdown)
}