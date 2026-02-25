#' @title Create Question Label Table for EFA Results
#' @description Generate a formatted table mapping question numbers to their
#'   short labels for interpreting factor analysis results.
#' 
#' @param efa_result list; Output from run_efa()
#' @param responses_df data.frame; Output from get_responses_df()
#' 
#' @returns data.frame with question numbers, labels, and indicators
#' @export
#' 
#' @importFrom dplyr %>% select distinct arrange filter
#' 
create_question_label_table <- function(efa_result, responses_df) {
  
  # Extract item names from loadings
  items_in_analysis <- rownames(efa_result$loadings)
  
  # Extract question numbers (remove "Q" prefix)
  question_nums <- as.integer(gsub("Q", "", items_in_analysis))
  
  # Get labels from responses_df
  question_labels <- responses_df %>%
    filter(QUESTION_NUMBER %in% question_nums) %>%
    select(QUESTION_NUMBER, QUESTION_SHORT, INDICATOR) %>%
    distinct() %>%
    arrange(QUESTION_NUMBER)
  
  # Create display names
  question_labels <- question_labels %>%
    mutate(
      Item = paste0("Q", QUESTION_NUMBER),
      Label = QUESTION_SHORT,
      Indicator = INDICATOR
    ) %>%
    select(Item, Label, Indicator)
  
  return(question_labels)
}