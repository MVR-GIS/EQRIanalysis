# AI Session Logs

This folder contains unfiltered GitHub Copilot chat transcripts.

## Purpose
- Document AI assistance during development
- Enable colleague review of AI-generated suggestions
- Maintain transparency in AI-assisted workflows

## Format
- One `.md` file per work session (by date)
- Updated incrementally throughout active session via `extract_copilot_chat()`
- AI-generated session handoff included at end
- Committed at end of session with related code changes

## Backup System
- `.backups/` folder contains timestamped snapshots from throughout the day
- Automatically created when updating an existing session file
- `.backups/` is gitignored (local scratch space only)
- Cleanup old backups weekly via `cleanup_session_backups()`

## Workflow

### Throughout the Day
```r
# Load functions
devtools::document()
devtools::load_all()

# Update session log as often as needed (creates backups automatically)
extract_copilot_chat("~/Downloads/copilot_export.zip")
```

### End of Session

**1. Request AI-generated handoff in your active Copilot chat:**

```
Generate a session handoff using the format in dev/sessions/dev_HANDOFF_TEMPLATE.md.

Base it on:
- Our conversation today
- Recent git commits in the repo
- Modified files

Provide formatted markdown ready to include in the session file.
```

**2. After AI generates the handoff, export and extract:**

```r
# Export chat from GitHub Copilot (three-dot menu → Export → markdown)
# This now includes the AI-generated handoff

# Extract to session file
extract_copilot_chat("~/Downloads/copilot_export.zip")

# Session file now contains: chat transcript + AI handoff
```

**3. Commit:**


### Weekly Maintenance
```r
# Remove backups older than 7 days
cleanup_session_backups()
```

## Using Session Logs for Context

To continue work in a new Copilot chat session:

1. Open previous session file (e.g., `dev/sessions/2026-02-20.md`)
2. Scroll to "Session Handoff" section at bottom
3. Copy handoff content
4. Paste into new Copilot chat to establish context

## Testing

The `extract_copilot_chat()` and `cleanup_session_backups()` functions 
are tested via `tests/testthat/test-extract_copilot_chat.R`.
