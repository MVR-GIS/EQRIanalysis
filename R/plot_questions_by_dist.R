#' @title Plot Question by USACE Districts
#' @description Plot question by USACE Districts.
#' @param questions_df     data.frame; A data frame of questions
#'                         returned by `get_questions_df`.
#' @param responses_df     data.frame; A data frame of responses
#'                         returned by `get_responses_df`.
#' @param question_number  integer; The question number to be ploted.
#' @returns a ggplot2 object
#' @export
#' @importFrom dplyr %>% filter mutate
#' @importFrom forcats fct_drop
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual vars
#'                     theme_grey theme element_blank
#'                     facet_wrap
#' @importFrom ggh4x facet_grid2
#' @importFrom NatParksPalettes natparks.pals
#' @importFrom patchwork wrap_plots
#'
plot_question_by_dist <- function(questions_df, responses_df, question_number) {
  # Extract the current question
  current_question <- questions_df %>%
    filter(QUESTION_NUMBER == question_number)

  # Filter responses for a single question
  question_responses <- responses_df %>%
    filter(QUESTION_NUMBER == current_question$QUESTION_NUMBER) %>%
    # Drop unused factor levels
    mutate(RESPONSE = fct_drop(RESPONSE))

  # Determine question number of responses
  response_levels <- nlevels(question_responses$RESPONSE)

  # Determine the Divisions in the data
  divisions <- sort(levels(fct_drop(question_responses$DIVISION)))

  # Create list of plots
  plot_list <- map(divisions, function(division) {
    # Filter for the division
    question_responses_div <- question_responses %>%
      filter(DIVISION == division)

    ggplot(question_responses_div, 
      aes(x = RESPONSE, fill = RESPONSE)) +
    geom_bar(
      color = "darkgrey",    # Set bar border color
      linewidth = 0.2,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = natparks.pals("Arches", n = response_levels, 
                             type = "continuous")
    ) +
    facet_grid2(
      rows = vars(DIVISION),
      cols = vars(DISTRICT),
      drop = TRUE,
      render_empty = TRUE
    ) +
    theme_grey(base_size = 8) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "bottom",
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
