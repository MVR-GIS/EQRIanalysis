#' @title Render Question
#' @description Render the markdown output for a question.
#' @param questions_df     data.frame; A data frame of questions
#'                         returned by `get_questions_df`.
#' @param responses_df     data.frame; A data frame of responses
#'                         returned by `get_responses_df`.
#' @param question_number  integer; The question number to be ploted.
#' @returns a markdown string representing the output for each question
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggsave
#'
render_question <- function(questions_df, responses_df, question_number) {
  # Extract the current question
  current_question <- questions_df %>%
    filter(QUESTION_NUMBER == question_number)

  if (nrow(current_question) == 0) {
    stop("Question number not found in the questions dataset.")
  }

  # Generate the program type plot
  plot_usace_path <- paste0("plots/questions/q_usace_", question_number, ".svg")
  ggsave(
    filename = plot_usace_path,
    plot = plot_question_by_usace(
      questions_df,
      responses_df,
      current_question$QUESTION_NUMBER
    ),
    width = 8,
    height = 2,
    units = "in",
    dpi = 150,
    create.dir = TRUE
  )

  # Generate the program type plot
  plot_type_path <- paste0("plots/questions/q_type_", question_number, ".svg")
  ggsave(
    filename = plot_type_path,
    plot = plot_question_by_type(
      questions_df,
      responses_df,
      current_question$QUESTION_NUMBER
    ),
    width = 8,
    height = 2,
    units = "in",
    dpi = 150,
    create.dir = TRUE
  )

  # Generate the division plot
  plot_div_path <- paste0("plots/questions/q_div_", question_number, ".svg")
  ggsave(
    filename = plot_div_path,
    plot = plot_question_by_div(
      questions_df,
      responses_df,
      current_question$QUESTION_NUMBER
    ),
    width = 8,
    height = 2.5,      # Needs +0.5 in height for bottom legend
    units = "in",
    dpi = 150,
    create.dir = TRUE
  )

  # Generate the district plot
  plot_dist_path <- paste0("plots/questions/q_dist_", question_number, ".svg")
  ggsave(
    filename = plot_dist_path,
    plot = plot_question_by_dist(
      questions_df,
      responses_df,
      current_question$QUESTION_NUMBER
    ),
    width = 8,
    height = 8,
    units = "in",
    dpi = 150,
    create.dir = TRUE
  )
  # Generate the design team plot
  plot_design_team_path <- paste0("plots/questions/q_design_team_", question_number, ".svg")
  ggsave(
    filename = plot_design_team_path,
    plot = plot_question_by_design_team(
      questions_df,
      responses_df,
      current_question$QUESTION_NUMBER
    ),
    width = 8,
    height = 2,
    units = "in",
    dpi = 150,
    create.dir = TRUE
  )
  # Generate the design strategy plot
  plot_design_strat_path <- paste0("plots/questions/q_design_strat_", question_number, ".svg")
  ggsave(
    filename = plot_design_strat_path,
    plot = plot_question_by_design_strat(
      questions_df,
      responses_df,
      current_question$QUESTION_NUMBER
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
      "## Q",
      current_question$QUESTION_NUMBER,
      ": ",
      current_question$short_name
    ),
    "",
    current_question$QUESTION_TEXT,
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
    '::: {.graph title="by District" collapse=true}',
    paste0("![](", plot_dist_path, ")"),
    ":::",
    "",
    '::: {.graph title="by Design Team" collapse=true}',
    paste0("![](", plot_design_team_path, ")"),
    ":::",
    "",
    '::: {.graph title="by Design Strategy" collapse=true}',
    paste0("![](", plot_design_strat_path, ")"),
    ":::",
    ""
  )

  return(markdown)
}
