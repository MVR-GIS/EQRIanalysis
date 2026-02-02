#' @title Get Responses Data Frame
#' @description Gets a responses data frame.
#' @returns a data frame of responses
#' @export
#' @importFrom dplyr mutate
#' @importFrom forcats as_factor fct_relevel
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

  responses_df <- responses_df %>%
    # Factor RESPONSE
    mutate(RESPONSE = as_factor(RESPONSE)) %>%
    mutate(RESPONSE = fct_relevel(RESPONSE, response_levels)) %>%
    # Factor org levels
    mutate(PROGRAMTYPE_NAME = as_factor(PROGRAMTYPE_NAME)) %>%
    mutate(DIVISION = as_factor(DIVISION)) %>%
    mutate(DISTRICT = as_factor(DISTRICT))


  return(responses_df)
}