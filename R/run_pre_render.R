#' @title Run Pre Render Steps
#' @description Runs pre render steps.
#' @returns NULL
#' @export
#' @importFrom here i_am
#' @importFrom devtools install
run_pre_render <- function() {
  # Declare location of current script
  here::i_am("R/run_pre_render.R")

  options(pkgType = "binary")
  devtools::install(
    quick = TRUE,
    upgrade = "always",
    quiet = TRUE,
    force = TRUE
  )

  # Update data
  get_data_csv()
}
