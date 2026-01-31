#' @title Get Questions Data Frame
#' @description Gets a data frame of question numbers and question text. 
#' @returns data frame of questions
#' @export
#' @importFrom here here
#' @importFrom dplyr select arrange distinct mutate across left_join
#' @importFrom tibble tribble
get_questions_df <- function() {
  # Get responses
  responses <- EQRIanalysis::responses

  # Get df of questions
  questions_df <- responses %>%
    dplyr::select(QUESTION_NUMBER, QUESTION_TEXT) %>%
    dplyr::arrange(QUESTION_NUMBER) %>%
    dplyr::distinct(QUESTION_NUMBER, .keep_all = TRUE)
  
  # Create question short names
  short_names_df <- tribble(
      ~QUESTION_NUMBER, ~short_name,
      1,   "Professional TL",
      2,   "PDT disciplines",
      3,   "QAQC disciplines",
      4,   "Design experience",
      5,   "PDT mentors",
      6,   "Member capacity",
      7,   "Engaged centers",
      8,   "Permitting authorities",
      9,   "Assumptions valid",
      10,  "Class 3 estimate",
      11,  "Updated TPCS",
      12,  "Deviation waiver",
      13,  "Lessons learned",
      14,  "Design expectations",
      15,  "Standard milestones",
      16,  "Design time",
      17,  "QAQC time",
      18,  "Survey time",
      19,  "Permitting time",
      20,  "Schedule alignment",
      21,  "Current QMP/RP",
      22,  "DQC prior",
      23,  "QMP/RP followed",
      24,  "Computations QC",
      25,  "Plans QC",
      26,  "Specs QC",
      27,  "Models QC",
      28,  "Compliant BCOES",
      29,  "Complete DQC, ITR/ATR, BCOES",
      30,  "Updated QMP/RP",
      31,  "Design adequacy",
      32,  "Deliveralbe expectations",
      33,  "Cost design maturity",
      34,  "Current Risk Register",
      35,  "35% CSRA",
      36,  "District PA",
      37,  "Team turnover",
      38,  "Design confidence"
    ) %>%
    mutate(
      across(QUESTION_NUMBER, as.integer),
      across(short_name, as.character))

  # Add short names to questions_df
  questions_df <- questions_df %>%
    left_join(y = short_names_df, by = "QUESTION_NUMBER")

  return(questions_df)
}
