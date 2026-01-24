#' @title Get Responses Data Frame
#' @description Gets a responses data frame.
#' @returns a data frame of responses
#' @export
#' @importFrom here here
get_responses_df <- function() {
  # Get responses
  responses_df <- EQRIanalysis::responses

  return(responses_df)
}