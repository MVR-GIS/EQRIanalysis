#' Extract GitHub Copilot Chat from Export Zip
#'
#' Extracts the main chat markdown file from a GitHub Copilot export zip
#' archive and saves it to dev/sessions/ with date-based naming. Supports
#' incremental updates throughout the day via backup and timestamp tracking.
#'
#' @param zip_path Character. Path to the exported Copilot chat .zip file
#' @param session_date Character or Date. Date for the session (default: today).
#'   Format: "YYYY-MM-DD" or Date object
#' @param topic Character. Optional topic suffix for filename (default: NULL)
#' @param sessions_dir Character. Path to sessions directory 
#'   (default: "dev/sessions")
#' @param backup Logical. Create timestamped backup before overwriting? 
#'   (default: TRUE)
#' @param quiet Logical. Suppress informational messages? (default: FALSE)
#'
#' @return Character. Path to extracted session file (invisibly)
#'
#' @details
#' The function is designed for incremental updates throughout a work session:
#' - First call: Creates session file
#' - Subsequent calls: Overwrites with latest export, creating backup
#' - Backups stored in dev/sessions/.backups/ with timestamps
#' - Original filename preserved for easy reference
#'
#' @examples
#' \dontrun{
#' # Call multiple times throughout the day - automatically handles updates
#' extract_copilot_chat("~/Downloads/copilot_export.zip")
#' # ... work for 2 hours ...
#' extract_copilot_chat("~/Downloads/copilot_export.zip")  # Creates backup
#' # ... handle brushfire ...
#' extract_copilot_chat("~/Downloads/copilot_export.zip")  # Another backup
#'
#' # Disable backups (not recommended for active sessions)
#' extract_copilot_chat("~/Downloads/copilot_export.zip", backup = FALSE)
#'
#' # Specify topic for multi-topic days
#' extract_copilot_chat(
#'   "~/Downloads/copilot_export.zip",
#'   topic = "data_validation"
#' )
#' }
#'
#' @export
extract_copilot_chat <- function(zip_path,
                                   session_date = Sys.Date(),
                                   topic = NULL,
                                   sessions_dir = "dev/sessions",
                                   backup = TRUE,
                                   quiet = FALSE) {
  
  # Validate inputs ----
  if (!file.exists(zip_path)) {
    stop("Zip file not found: ", zip_path)
  }
  
  if (!grepl("\\.zip$", zip_path, ignore.case = TRUE)) {
    stop("File must be a .zip archive: ", zip_path)
  }
  
  # Ensure sessions directory exists ----
  if (!dir.exists(sessions_dir)) {
    dir.create(sessions_dir, recursive = TRUE)
    if (!quiet) message("Created directory: ", sessions_dir)
  }
  
  # Ensure backup directory exists (if using backups) ----
  if (backup) {
    backup_dir <- file.path(sessions_dir, ".backups")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
      if (!quiet) message("Created backup directory: ", backup_dir)
    }
  }
  
  # Format session date ----
  if (inherits(session_date, "Date")) {
    date_str <- format(session_date, "%Y-%m-%d")
  } else {
    # Validate date string format
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", session_date)) {
      stop("session_date must be Date object or 'YYYY-MM-DD' format")
    }
    date_str <- session_date
  }
  
  # Build output filename ----
  if (!is.null(topic)) {
    # Sanitize topic (remove spaces, special chars)
    topic_clean <- gsub("[^a-zA-Z0-9_-]", "_", topic)
    output_filename <- paste0(date_str, "_", topic_clean, ".md")
  } else {
    output_filename <- paste0(date_str, ".md")
  }
  
  output_path <- file.path(sessions_dir, output_filename)
  
  # Backup existing file if it exists ----
  if (file.exists(output_path) && backup) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    backup_filename <- paste0(
      tools::file_path_sans_ext(output_filename),
      "_backup_",
      timestamp,
      ".md"
    )
    backup_path <- file.path(backup_dir, backup_filename)
    
    file.copy(output_path, backup_path, overwrite = FALSE)
    
    if (!quiet) {
      message("Backed up existing session to: ", basename(backup_path))
    }
  }
  
  # Create temporary directory for extraction ----
  temp_dir <- tempfile(pattern = "copilot_chat_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  
  # Unzip archive ----
  unzip(zip_path, exdir = temp_dir, overwrite = TRUE)
  
  # Find main .md file in root of extracted archive ----
  # (not in subdirectories - those are code suggestions)
  all_files <- list.files(temp_dir, full.names = TRUE, recursive = FALSE)
  md_files <- all_files[grepl("\\.md$", all_files, ignore.case = TRUE)]
  
  if (length(md_files) == 0) {
    stop("No .md file found in zip archive root")
  }
  
  if (length(md_files) > 1) {
    warning(
      "Multiple .md files found in root. Using first: ",
      basename(md_files[1])
    )
  }
  
  # Copy to sessions directory (overwrite if exists) ----
  file.copy(md_files[1], output_path, overwrite = TRUE)
  
  # Success message ----
  if (!quiet) {
    if (file.exists(output_path)) {
      message(
        "✓ Updated session transcript: ", output_filename, "\n",
        "  Session date: ", date_str,
        if (!is.null(topic)) paste0("\n  Topic: ", topic) else "",
        if (backup && file.exists(file.path(backup_dir, list.files(backup_dir, pattern = date_str)[1]))) 
          paste0("\n  (Previous version backed up)")
        else ""
      )
    }
  }
  
  invisible(output_path)
}
