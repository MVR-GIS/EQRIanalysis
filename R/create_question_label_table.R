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
#' @importFrom dplyr %>% select distinct arrange filter mutate
#' 
create_question_label_table <- function(efa_result, responses_df) {
  
  # Extract item names from loadings
  items_in_analysis <- rownames(efa_result$loadings)
  
  # Extract question numbers (remove "Q" prefix)
  question_nums <- as.integer(gsub("Q", "", items_in_analysis))
  
  # Get labels from responses_df
  # Per dplyr::distinct() documentation, we need to be careful about
  # which columns to keep distinct - a question may appear multiple times
  # with different indicators
  question_labels <- responses_df %>%
    filter(QUESTION_NUMBER %in% question_nums) %>%
    # Select only the columns we need
    select(QUESTION_NUMBER, QUESTION_SHORT, INDICATOR) %>%
    # Get unique combinations of question number and label
    # Since a question has one label but may map to multiple indicators,
    # we keep all indicator mappings
    distinct(QUESTION_NUMBER, QUESTION_SHORT, INDICATOR) %>%
    # If a question maps to multiple indicators, concatenate them
    group_by(QUESTION_NUMBER, QUESTION_SHORT) %>%
    summarize(
      Indicator = paste(unique(INDICATOR), collapse = ", "),
      .groups = "drop"
    ) %>%
    arrange(QUESTION_NUMBER) %>%
    # Create display names
    mutate(
      Item = paste0("Q", QUESTION_NUMBER),
      Label = QUESTION_SHORT
    ) %>%
    select(Item, Label, Indicator)
  
  return(question_labels)
}