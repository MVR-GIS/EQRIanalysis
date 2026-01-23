
library(here)
library(readr)
library(dplyr)

# setup-logging
log_file_path <- here::here("logs/get_data_log.txt")
if (file.exists(log_file_path)) {
  file.remove(log_file_path)
}
log_con <- file(log_file_path, open = "a")
sink(log_con, split = TRUE)  # Redirect console output
sink(log_con, type = "message", append = TRUE)  # Redirect messages

# where-are-we
message("Current working directory: ", getwd())

# get-csv
latest_export <- here("data/UR_QRI_RESPONSE_EDA_20260122.csv")

if (file.exists(latest_export)) {
  message("Lastest CSV export: ", latest_export)
} else {
  message("CSV file not found: ", latest_export)
}

# get-static-data
tryCatch({
    responses_raw <- read_csv(latest_export)
    message("CSV successfully read.")
}, error = function(e) {
    message("Failed to load data: ", e$message)
})

# clean-data
# Get column spec
column_types <- spec(responses_raw)
message("Retrieved column spec.")

# Adjust column types
column_types$cols$PROJECT_ID <- col_integer()
column_types$cols$SUBPROJECT_ID  <- col_integer()
column_types$cols$RESPONSE_UID  <- col_integer()
column_types$cols$QUESTIONNAIREEVENT_ID  <- col_integer()
column_types$cols$QUESTION_NUMBER  <- col_integer()
column_types$cols$MILESTONE_ID  <- col_integer()
column_types$cols$PROGRAMTYPE_ID  <- col_integer()
message("Adjusted column spec.")

# Clean responses
responses_clean <- readr::read_csv(
    latest_export,
    col_types = column_types)
message("Responses cleaned.")

# data-prep
responses <- responses_clean %>%
    mutate(
        PROGRAMTYPE_NAME = case_when(
            PROGRAMTYPE_ID == 1 ~ "Military",
            PROGRAMTYPE_ID == 2 ~ "Civil Works"
        )
    ) %>%
    relocate(PROGRAMTYPE_NAME, .after = PROGRAMTYPE_ID)
message("Responses wrangled.")

# save-data
response_rds_file_path <- here("data/responses.rds")

tryCatch({
    saveRDS(responses, response_rds_file_path)
    message("responses.rds saved: ", response_rds_file_path)
}, error = function(e) {
    message("Failed to save data: ", e$message)
})

if (!file.exists(response_rds_file_path)) {
    message("response.rds file not found: ", response_rds_file_path)
}

# session-info
message("\\n", "Session Info: ", paste0(capture.output(sessionInfo()), collapse = "\n"))
message("Environment Variables: ", Sys.getenv())

# close-log
sink(type = "message")
close(log_con)
