#' @title Plot Question by Program Type
#' @description Plot question by program type (Civil Works
#'   or Military).
#' @param questions_df     data.frame; A data frame of questions
#'                         returned by `get_questions_df`.
#' @param responses_df     data.frame; A data frame of responses
#'                         returned by `get_responses_df`.
#' @param question_number  integer; The question number to be ploted.
#' @returns a ggplot2 object
#' @export
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual
#'                     facet_grid theme_grey theme element_blank
#' @importFrom forcats fct_drop
#' @importFrom NatParksPalettes natparks.pals
#'
plot_question_by_type <- function(questions_df, responses_df, question_number) {
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

  p <- ggplot(question_responses, aes(x = RESPONSE, fill = RESPONSE)) +
    geom_bar(
      color = "darkgrey",    # Set bar border color
      linewidth = 0.2,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = natparks.pals("Arches", n = response_levels, type = "continuous")
    ) +
    facet_grid(~PROGRAMTYPE_NAME) +
    theme_grey(base_size = 8) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )
  # p
  return(p)
}
