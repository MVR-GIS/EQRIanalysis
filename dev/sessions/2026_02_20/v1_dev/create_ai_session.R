# Helper to create AI session log file
# Source this in R console when starting a session

create_ai_session_log <- function(date = Sys.Date(), purpose = "") {
  filename <- file.path("dev", "sessions", paste0(date, ".md"))
  
  if (!dir.exists("dev/sessions")) {
    dir.create("dev/sessions", recursive = TRUE)
  }
  
  header <- sprintf(
    "# AI Session: %s\n\n**Purpose:** %s  \n**Started:** %s  \n**Commits:** (update at end)  \n\n---\n\n",
    date,
    purpose,
    format(Sys.time(), "%Y-%m-%d %H:%M")
  )
  
  if (!file.exists(filename)) {
    writeLines(header, filename)
    message("Created: ", filename)
  } else {
    message("Session log already exists: ", filename)
  }
  
  # Open in RStudio if available
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(filename)
  }
  
  invisible(filename)
}

# Quick alias
new_session <- function(purpose = "") {
  create_ai_session_log(purpose = purpose)
}