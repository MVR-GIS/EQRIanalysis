run_pre_render <- function() {
  # Source all the functions in the R directory
  sapply(list.files('R', full.names = TRUE), source)



  # Update data
  get_data_csv()
}