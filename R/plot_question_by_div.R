#' @title Plot Question by USACE Divisions
#' @description Plot question by USACE Divisions.
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
#' @importFrom NatParksPalettes natparks.pals
#'
plot_question_by_div <- function(questions_df, responses_df, question_number) {
  # Extract the current question
  current_question <- questions_df %>%
    filter(QUESTION_NUMBER == question_number)

  # Filter responses for a single question
  question_responses <- responses_df %>%
    filter(QUESTION_NUMBER == current_question$QUESTION_NUMBER)

  p <- ggplot(question_responses, aes(x = RESPONSE, fill = RESPONSE)) +
    geom_bar() +
    scale_fill_manual(values = natparks.pals("KingsCanyon", 6)) +
    facet_grid(~DIVISION) +
    theme_grey(base_size = 11) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank()
    )

  return(p)
}
