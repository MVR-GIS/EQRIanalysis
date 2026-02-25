# Tests for create_question_label_table()

test_that("create_question_label_table returns valid table", {
  responses_df <- get_responses_df()
  
  efa_result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,
      fm = "minres"
    )
  })
  
  label_table <- create_question_label_table(efa_result, responses_df)
  
  # Test structure
  expect_s3_class(label_table, "data.frame")
  expect_true("Item" %in% names(label_table))
  expect_true("Label" %in% names(label_table))
  expect_true("Indicator" %in% names(label_table))
  
  # Test content
  expect_true(nrow(label_table) == efa_result$sample$n_items)
  expect_true(all(grepl("^Q[0-9]+$", label_table$Item)))
})

test_that("create_question_label_table item codes match loadings", {
  responses_df <- get_responses_df()
  
  efa_result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,
      fm = "minres"
    )
  })
  
  label_table <- create_question_label_table(efa_result, responses_df)
  loading_items <- rownames(efa_result$loadings)
  
  # Item codes should match exactly
  expect_equal(sort(label_table$Item), sort(loading_items))
})