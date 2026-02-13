#' @title Get Responses Data Frame
#' @description Gets a responses data frame.
#' @returns a data frame of responses
#' @export
#' @importFrom dplyr mutate %>% across
#' @importFrom forcats fct_relevel
get_responses_df <- function() {
  # Get responses
  responses_df <- EQRIanalysis::responses

  # Define factors
  response_levels <- c(
    "Yes",
    "High",
    "Somewhat High",
    "Marginal",
    "Moderate",
    "Somewhat Low",
    "Low",
    "No"
  )
  milestone_levels <- c(
    "15% (Project Initiation)", 
    "35% (Concept Design)", 
    "65% (Intermediate Design)", 
    "95% (Final Design)",
    "100% (Corrected Final Design)"
  )
  design_team_levels <- c(
    "In-House", 
    "Brokered with another District", 
    "Hybrid", 
    "AE"
  )
  design_strat_levels <- c(
    "Design-Build", "Design-Bid-Build"
  )

  responses_df <- responses_df %>%
    # Factor all character variables
    mutate(
      across(
        where(is.character), 
        factor)
    ) %>%
    # Factor project variables
    mutate(
      across(
        c(PROJECT_ID, SUBPROJECT_ID), 
        factor)
    ) %>%
    # Factor question variables
    mutate(
      across(
        c(RESPONSE_UID, QUESTIONNAIREEVENT_ID, QUESTION_NUMBER), 
        factor)
    ) %>%
    # Factor other dimensions
    mutate(
      across(
        c(PROGRAMTYPE_ID, PROGRAMTYPE_NAME, PROGRAM_NAME, PROGRAM_DESC, 
          DESIGN_TEAM, DESIGNSTRATEGY_DESC),
        factor)
    ) %>%
    # Relevel the factors
    mutate(
      RESPONSE = fct_relevel(RESPONSE, response_levels),
      MILESTONE_DESC = fct_relevel(MILESTONE_DESC, milestone_levels),
      DESIGN_TEAM = fct_relevel(DESIGN_TEAM, design_team_levels),
      DESIGNSTRATEGY_DESC = fct_relevel(DESIGNSTRATEGY_DESC, design_strat_levels)
    )
  
  return(responses_df)
}