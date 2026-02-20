# Tests for dev/extract_copilot_chat.R
# 
# Note: These tests use temporary directories and mock zip files
# to avoid depending on actual GitHub Copilot exports

# Helper: Create mock Copilot export zip ----
create_mock_copilot_zip <- function(chat_content = "# Mock Chat\n\nTest content",
                                     include_code_folders = TRUE) {
  temp_dir <- tempfile(pattern = "mock_copilot_")
  dir.create(temp_dir)
  
  # Create main chat markdown file
  chat_file <- file.path(temp_dir, "chat_transcript.md")
  writeLines(chat_content, chat_file)
  
  # Optionally create code suggestion folders (to mimic real exports)
  if (include_code_folders) {
    code_dir <- file.path(temp_dir, "code_suggestions")
    dir.create(code_dir)
    
    # Mock code suggestion subfolder
    suggestion_dir <- file.path(code_dir, "suggestion_001")
    dir.create(suggestion_dir)
    writeLines("# Mock code", file.path(suggestion_dir, "version_1.R"))
  }
  
  # Create zip file
  zip_file <- tempfile(fileext = ".zip")
  
  # Get original working directory
  orig_wd <- getwd()
  on.exit(setwd(orig_wd), add = TRUE)
  
  # Change to temp directory to create clean zip structure
  setwd(temp_dir)
  files_to_zip <- list.files(temp_dir, recursive = TRUE, full.names = FALSE)
  
  # Create zip (suppress warnings about timestamps)
  suppressWarnings(
    zip(zipfile = zip_file, files = files_to_zip, flags = "-q")
  )
  
  # Clean up temp directory
  unlink(temp_dir, recursive = TRUE)
  
  zip_file
}

# Setup and Teardown ----
test_that("test helper creates valid mock zip", {
  zip_path <- create_mock_copilot_zip()
  on.exit(unlink(zip_path))
  
  expect_true(file.exists(zip_path))
  expect_match(zip_path, "\\.zip$")
  
  # Verify zip contains .md file
  temp_extract <- tempfile()
  dir.create(temp_extract)
  on.exit(unlink(temp_extract, recursive = TRUE), add = TRUE)
  
  unzip(zip_path, exdir = temp_extract)
  md_files <- list.files(temp_extract, pattern = "\\.md$", recursive = FALSE)
  expect_true(length(md_files) > 0)
})

# extract_copilot_chat() tests ----

test_that("extract_copilot_chat creates session file", {
  # Setup
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  zip_path <- create_mock_copilot_zip("# Test Chat\n\nContent here")
  on.exit(unlink(zip_path), add = TRUE)
  
  # Test
  result <- extract_copilot_chat(
    zip_path,
    session_date = "2026-02-20",
    sessions_dir = temp_sessions,
    quiet = TRUE
  )
  
  # Verify
  expect_true(file.exists(result))
  expect_equal(basename(result), "2026-02-20.md")
  
  # Verify content was extracted
  content <- readLines(result)
  expect_true(any(grepl("Test Chat", content)))
})

test_that("extract_copilot_chat handles topic suffix", {  
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  zip_path <- create_mock_copilot_zip()
  on.exit(unlink(zip_path), add = TRUE)
  
  result <- extract_copilot_chat(
    zip_path,
    session_date = "2026-02-20",
    topic = "data_validation",
    sessions_dir = temp_sessions,
    quiet = TRUE
  )
  
  expect_equal(basename(result), "2026-02-20_data_validation.md")
})

test_that("extract_copilot_chat sanitizes topic names", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  zip_path <- create_mock_copilot_zip()
  on.exit(unlink(zip_path), add = TRUE)
  
  result <- extract_copilot_chat(
    zip_path,
    session_date = "2026-02-20",
    topic = "quarto config & setup!",
    sessions_dir = temp_sessions,
    quiet = TRUE
  )
  
  # Special characters should be converted to underscores
  expect_match(basename(result), "^2026-02-20_quarto_config___setup_\\.md$")
})

test_that("extract_copilot_chat creates backup on subsequent calls", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  # First call
  zip_path1 <- create_mock_copilot_zip("# First version")
  on.exit(unlink(zip_path1), add = TRUE)
  
  extract_copilot_chat(
    zip_path1,
    session_date = "2026-02-20",
    sessions_dir = temp_sessions,
    backup = TRUE,
    quiet = TRUE
  )
  
  # Wait a moment to ensure different timestamp
  Sys.sleep(1)
  
  # Second call
  zip_path2 <- create_mock_copilot_zip("# Second version")
  on.exit(unlink(zip_path2), add = TRUE)
  
  extract_copilot_chat(
    zip_path2,
    session_date = "2026-02-20",
    sessions_dir = temp_sessions,
    backup = TRUE,
    quiet = TRUE
  )
  
  # Verify main file has new content
  main_file <- file.path(temp_sessions, "2026-02-20.md")
  content <- readLines(main_file)
  expect_true(any(grepl("Second version", content)))
  
  # Verify backup exists with old content
  backup_dir <- file.path(temp_sessions, ".backups")
  expect_true(dir.exists(backup_dir))
  
  backups <- list.files(backup_dir, pattern = "2026-02-20_backup_.*\\.md$")
  expect_true(length(backups) >= 1)
  
  backup_content <- readLines(file.path(backup_dir, backups[1]))
  expect_true(any(grepl("First version", backup_content)))
})

test_that("extract_copilot_chat respects backup = FALSE", {
  skip_if_not_installed("testthat")
  source("../../dev/extract_copilot_chat.R")
  
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  # First call
  zip_path1 <- create_mock_copilot_zip("# First")
  on.exit(unlink(zip_path1), add = TRUE)
  
  extract_copilot_chat(
    zip_path1,
    session_date = "2026-02-20",
    sessions_dir = temp_sessions,
    backup = FALSE,
    quiet = TRUE
  )
  
  # Second call without backup
  zip_path2 <- create_mock_copilot_zip("# Second")
  on.exit(unlink(zip_path2), add = TRUE)
  
  extract_copilot_chat(
    zip_path2,
    session_date = "2026-02-20",
    sessions_dir = temp_sessions,
    backup = FALSE,
    quiet = TRUE
  )
  
  # Verify no backup directory created
  backup_dir <- file.path(temp_sessions, ".backups")
  expect_false(dir.exists(backup_dir))
})

test_that("extract_copilot_chat validates date format", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  zip_path <- create_mock_copilot_zip()
  on.exit(unlink(zip_path), add = TRUE)
  
  # Invalid date format should error
  expect_error(
    extract_copilot_chat(
      zip_path,
      session_date = "02/20/2026",  # Wrong format
      sessions_dir = temp_sessions,
      quiet = TRUE
    ),
    "YYYY-MM-DD"
  )
  
  expect_error(
    extract_copilot_chat(
      zip_path,
      session_date = "2026-2-20",  # Missing leading zeros
      sessions_dir = temp_sessions,
      quiet = TRUE
    ),
    "YYYY-MM-DD"
  )
})

test_that("extract_copilot_chat accepts Date objects", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  zip_path <- create_mock_copilot_zip()
  on.exit(unlink(zip_path), add = TRUE)
  
  test_date <- as.Date("2026-02-20")
  
  result <- extract_copilot_chat(
    zip_path,
    session_date = test_date,
    sessions_dir = temp_sessions,
    quiet = TRUE
  )
  
  expect_equal(basename(result), "2026-02-20.md")
})

test_that("extract_copilot_chat errors on missing zip file", {
  expect_error(
    extract_copilot_chat("/nonexistent/path/file.zip"),
    "Zip file not found"
  )
})

test_that("extract_copilot_chat errors on non-zip file", {
  temp_file <- tempfile(fileext = ".txt")
  writeLines("not a zip", temp_file)
  on.exit(unlink(temp_file))
  
  expect_error(
    extract_copilot_chat(temp_file),
    "must be a .zip archive"
  )
})

test_that("extract_copilot_chat errors on zip without markdown", {
  # Create zip without .md file
  temp_dir <- tempfile()
  dir.create(temp_dir)
  temp_file <- file.path(temp_dir, "test.txt")
  writeLines("not markdown", temp_file)
  
  zip_file <- tempfile(fileext = ".zip")
  orig_wd <- getwd()
  setwd(temp_dir)
  suppressWarnings(zip(zipfile = zip_file, files = "test.txt", flags = "-q"))
  setwd(orig_wd)
  
  unlink(temp_dir, recursive = TRUE)
  on.exit(unlink(zip_file))
  
  expect_error(
    extract_copilot_chat(zip_file, quiet = TRUE),
    "No .md file found"
  )
})

test_that("extract_copilot_chat creates sessions directory if missing", {
  temp_sessions <- tempfile(pattern = "sessions_")
  # Don't create directory
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  zip_path <- create_mock_copilot_zip()
  on.exit(unlink(zip_path), add = TRUE)
  
  # Directory doesn't exist yet
  expect_false(dir.exists(temp_sessions))
  
  extract_copilot_chat(
    zip_path,
    session_date = "2026-02-20",
    sessions_dir = temp_sessions,
    quiet = TRUE
  )
  
  # Should be created
  expect_true(dir.exists(temp_sessions))
})

# cleanup_session_backups() tests ----

test_that("cleanup_session_backups removes old backups", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  backup_dir <- file.path(temp_sessions, ".backups")
  dir.create(backup_dir)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  # Create old backup (simulate by setting mtime)
  old_backup <- file.path(backup_dir, "2026-02-10_backup_20260210_100000.md")
  writeLines("old", old_backup)
  
  # Set modification time to 10 days ago
  old_time <- Sys.time() - (10 * 86400)
  Sys.setFileTime(old_backup, old_time)
  
  # Create recent backup
  recent_backup <- file.path(backup_dir, "2026-02-20_backup_20260220_100000.md")
  writeLines("recent", recent_backup)
  
  # Cleanup with 7 day threshold
  cleanup_session_backups(
    sessions_dir = temp_sessions,
    days_to_keep = 7,
    dry_run = FALSE
  )
  
  # Old backup should be gone
  expect_false(file.exists(old_backup))
  
  # Recent backup should remain
  expect_true(file.exists(recent_backup))
})

test_that("cleanup_session_backups dry_run doesn't delete files", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  backup_dir <- file.path(temp_sessions, ".backups")
  dir.create(backup_dir)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  # Create old backup
  old_backup <- file.path(backup_dir, "2026-02-10_backup_20260210_100000.md")
  writeLines("old", old_backup)
  old_time <- Sys.time() - (10 * 86400)
  Sys.setFileTime(old_backup, old_time)
  
  # Dry run
  cleanup_session_backups(
    sessions_dir = temp_sessions,
    days_to_keep = 7,
    dry_run = TRUE
  )
  
  # File should still exist
  expect_true(file.exists(old_backup))
})

test_that("cleanup_session_backups handles missing backup directory", {
  skip_if_not_installed("testthat")
  source("../../dev/extract_copilot_chat.R")
  
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  # No .backups directory exists
  expect_message(
    cleanup_session_backups(sessions_dir = temp_sessions),
    "No backup directory found"
  )
})

test_that("cleanup_session_backups handles empty backup directory", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  backup_dir <- file.path(temp_sessions, ".backups")
  dir.create(backup_dir)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  expect_message(
    cleanup_session_backups(sessions_dir = temp_sessions),
    "No backup files found"
  )
})

test_that("cleanup_session_backups only removes backup files", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  backup_dir <- file.path(temp_sessions, ".backups")
  dir.create(backup_dir)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  # Create properly formatted backup
  old_backup <- file.path(backup_dir, "2026-02-10_backup_20260210_100000.md")
  writeLines("old", old_backup)
  old_time <- Sys.time() - (10 * 86400)
  Sys.setFileTime(old_backup, old_time)
  
  # Create file with wrong naming pattern
  other_file <- file.path(backup_dir, "notes.md")
  writeLines("notes", other_file)
  Sys.setFileTime(other_file, old_time)
  
  cleanup_session_backups(
    sessions_dir = temp_sessions,
    days_to_keep = 7
  )
  
  # Properly named backup should be deleted
  expect_false(file.exists(old_backup))
  
  # Other file should remain (wrong naming pattern)
  expect_true(file.exists(other_file))
})

# Integration test ----

test_that("full workflow: extract, update, cleanup", {
  temp_sessions <- tempfile(pattern = "sessions_")
  dir.create(temp_sessions)
  on.exit(unlink(temp_sessions, recursive = TRUE), add = TRUE)
  
  # First extraction
  zip1 <- create_mock_copilot_zip("# Version 1")
  on.exit(unlink(zip1), add = TRUE)
  
  extract_copilot_chat(
    zip1,
    session_date = "2026-02-20",
    sessions_dir = temp_sessions,
    quiet = TRUE
  )
  
  session_file <- file.path(temp_sessions, "2026-02-20.md")
  expect_true(file.exists(session_file))
  
  # Update (creates backup)
  Sys.sleep(1)  # Ensure different timestamp
  zip2 <- create_mock_copilot_zip("# Version 2")
  on.exit(unlink(zip2), add = TRUE)
  
  extract_copilot_chat(
    zip2,
    session_date = "2026-02-20",
    sessions_dir = temp_sessions,
    quiet = TRUE
  )
  
  # Verify backup exists
  backup_dir <- file.path(temp_sessions, ".backups")
  backups <- list.files(backup_dir, pattern = "\\.md$")
  expect_true(length(backups) >= 1)
  
  # Make backup "old" for cleanup test
  backup_file <- file.path(backup_dir, backups[1])
  old_time <- Sys.time() - (10 * 86400)
  Sys.setFileTime(backup_file, old_time)
  
  # Cleanup
  cleanup_session_backups(
    sessions_dir = temp_sessions,
    days_to_keep = 7
  )
  
  # Old backup should be removed
  expect_false(file.exists(backup_file))
  
  # Main session file should remain
  expect_true(file.exists(session_file))
  content <- readLines(session_file)
  expect_true(any(grepl("Version 2", content)))
})
