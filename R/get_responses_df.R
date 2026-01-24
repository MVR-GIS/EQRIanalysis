#' @title Get Responses Data Frame
#' @description Gets a responses data frame.
#' @returns a data frame of responses
#' @export
#' @importFrom here here
get_responses_df <- function() {
  # Get responses
  data("responses", package = "EQRIanalysis") 

  return(responses)
}