# AI Session Logs

This folder contains unfiltered GitHub Copilot chat transcripts.

## Purpose
- Document AI assistance during development
- Enable colleague review of AI-generated suggestions
- Maintain transparency in AI-assisted workflows

## Format
- One `.md` file per work session (by date)
- Updated incrementally throughout active session via `extract_copilot_chat()`
- Committed at end of session with related code changes

## Usage
To understand AI's role in any commit:
1. Check commit date
2. Read corresponding session file
3. See full conversation context

## Backup System
- `.backups/` folder contains timestamped snapshots from throughout the day
- Automatically created when updating an existing session file
- `.backups/` is gitignored (local scratch space only)
- Cleanup old backups weekly via `cleanup_session_backups()`

## Usage
To understand AI's role in any commit:
1. Check commit date
2. Read corresponding session file
3. See full conversation context

## Workflow
```r
# Throughout the day - call as often as needed
source("dev/extract_copilot_chat.R")
extract_copilot_chat("~/Downloads/copilot_export.zip")

# End of week cleanup
cleanup_session_backups(days_to_keep = 7)