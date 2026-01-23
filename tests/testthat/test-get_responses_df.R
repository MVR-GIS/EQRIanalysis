source(here::here("R/get_responses_df.R"))

testthat::test_that("check that get_responses_df works", {
  responses_df <- get_responses_df()
  testthat::expect_true(is.data.frame(responses_df))
  testthat::expect_true(any(c("PROJECT_ID", "PROJECT_NAME") %in% colnames(responses_df)))
})
