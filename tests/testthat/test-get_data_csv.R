source(here::here("R/get_data_csv.R"))

testthat::test_that("check that get_data_csv works", {
  file.remove(here::here("data/responses.rds"))
  get_data_csv_path <- get_data_csv()
  testthat::expect_true(file.exists(get_data_csv_path))
})
