#' @title Get Data From CSV Export
#' @description Get data from csv export.
#' @returns NULL saves data to `data/responses.rds`
get_data_csv <- function() {
  latest_export <- here::here("data/UR_QRI_RESPONSE_EDA_20260122.csv")
  responses_raw <- read_csv(latest_export)
  
  # Get column spec
  column_types <- readr::spec(responses_raw)
  # Adjust column types
  column_types$cols$PROJECT_ID <- readr::col_integer()
  column_types$cols$SUBPROJECT_ID <- readr::col_integer()
  column_types$cols$RESPONSE_UID <- readr::col_integer()
  column_types$cols$QUESTIONNAIREEVENT_ID <- readr::col_integer()
  column_types$cols$QUESTION_NUMBER <- readr::col_integer()
  column_types$cols$MILESTONE_ID <- readr::col_integer()
  column_types$cols$PROGRAMTYPE_ID <- readr::col_integer()

  # Clean responses
  responses_clean <- readr::read_csv(
    latest_export,
    col_types = column_types
  )

  responses <- responses_clean %>%
    dplyr::mutate(
        PROGRAMTYPE_NAME = dplyr::case_when(
            PROGRAMTYPE_ID == 1 ~ "Military",
            PROGRAMTYPE_ID == 2 ~ "Civil Works"
        )
    ) %>%
    dplyr::relocate(PROGRAMTYPE_NAME, .after = PROGRAMTYPE_ID)

  response_rds_file_path <- here::here("data/responses.rds")
  saveRDS(responses, response_rds_file_path)
  return(response_rds_file_path)
}
