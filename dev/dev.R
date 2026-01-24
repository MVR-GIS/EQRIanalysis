# Building a Prod-Ready, Robust Quarto Website with embedded R Package.

## Build website locally
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

## Render Quarto Website
### in Terminal
quarto render
