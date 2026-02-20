#' Clean Up Old Session Backups
#'
#' Removes backup files older than specified number of days to prevent
#' backup directory bloat.
#'
#' @param sessions_dir Character. Path to sessions directory 
#'   (default: "dev/sessions")
#' @param days_to_keep Numeric. Keep backups from last N days (default: 7)
#' @param dry_run Logical. Show what would be deleted without deleting? 
#'   (default: FALSE)
#'
#' @return Invisible NULL
#'
#' @examples
#' \dontrun{
#' # See what would be deleted
#' cleanup_session_backups(dry_run = TRUE)
#'
#' # Delete backups older than 7 days
#' cleanup_session_backups()
#'
#' # Keep backups from last 30 days
#' cleanup_session_backups(days_to_keep = 30)
#' }
#'
#' @export
cleanup_session_backups <- function(sessions_dir = "dev/sessions",
                                     days_to_keep = 7,
                                     dry_run = FALSE) {
  
  backup_dir <- file.path(sessions_dir, ".backups")
  
  if (!dir.exists(backup_dir)) {
    message("No backup directory found: ", backup_dir)
    return(invisible(NULL))
  }
  
  # Get all backup files
  backup_files <- list.files(
    backup_dir, 
    pattern = "_backup_\\d{8}_\\d{6}\\.md$",
    full.names = TRUE
  )
  
  if (length(backup_files) == 0) {
    message("No backup files found")
    return(invisible(NULL))
  }
  
  # Calculate cutoff date
  cutoff_date <- Sys.time() - (days_to_keep * 86400)  # 86400 seconds per day
  
  # Find files to delete
  files_info <- file.info(backup_files)
  old_files <- backup_files[files_info$mtime < cutoff_date]
  
  if (length(old_files) == 0) {
    message("No backups older than ", days_to_keep, " days found")
    return(invisible(NULL))
  }
  
  # Report or delete
  if (dry_run) {
    message(
      "Would delete ", length(old_files), " backup(s) older than ", 
      days_to_keep, " days:"
    )
    for (f in old_files) {
      message("  - ", basename(f))
    }
  } else {
    unlink(old_files)
    message(
      "Deleted ", length(old_files), " backup(s) older than ", 
      days_to_keep, " days"
    )
  }
  
  invisible(NULL)
}