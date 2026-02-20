#!/usr/bin/env Rscript
# Create AI session log file for today

session_date <- Sys.Date()
session_file <- file.path("dev", "sessions", paste0(session_date, ".md"))

# Create directory if needed
dir.create("dev/sessions", recursive = TRUE, showWarnings = FALSE)

# Header template
header <- sprintf(
  "# AI Session: %s\n\n**Purpose:** \n**Started:** %s\n**Files:** \n\n---\n\n",
  session_date,
  format(Sys.time(), "%H:%M")
)

# Only create if doesn't exist
if (!file.exists(session_file)) {
  writeLines(header, session_file)
  cat("Created:", session_file, "\n")
} else {
  cat("Session log exists:", session_file, "\n")
}

# Open in Positron/VS Code if available
system(sprintf("code %s", shQuote(session_file)), wait = FALSE)