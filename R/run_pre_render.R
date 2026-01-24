#' @title Run Pre Render Steps
#' @description Runs pre render steps.
#' @returns NULL
#' @export
run_pre_render <- function() {
  # Update data
  get_data_csv()
}
