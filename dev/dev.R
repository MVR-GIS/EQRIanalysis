# Building a Prod-Ready, Robust Quarto Website with embedded R Package.



## Configure Quarto extension: custom callout
# https://github.com/coatless-quarto/custom-callout

quarto add coatless-quarto/custom-callout

## Build website locally
install.packages("psych")


## Update AI Chat Artifacts
extract_copilot_chat(file.path(
  Sys.getenv("USERPROFILE"), "Downloads", "copilot_export.zip")
)
# Request session handoff
"Generate handoff using `dev/sessions/dev_HANDOFF_TEMPLATE.md`."
cleanup_session_backups()

## Update R package
devtools::document()
devtools::load_all()

### Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

### Install package
options(pkgType = "binary")
devtools::install(
  quick = TRUE,
  upgrade = "always",
  quiet = TRUE,
  force = TRUE
)

## Render Quarto Website from R console
quarto::quarto_render("01_questions.qmd")
quarto::quarto_render("02_indicators.qmd")
quarto::quarto_render()


## Render from terminal

### in Terminal
# quarto render
