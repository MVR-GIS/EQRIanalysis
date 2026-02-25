# Tests for factor loading visualization functions

test_that("plot_factor_loadings creates valid ggplot", {
  responses_df <- get_responses_df()
  
  # Run EFA
  efa_result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,
      fm = "minres"
    )
  })
  
  # Create plot
  plot <- plot_factor_loadings(efa_result)
  
  # Test
  expect_s3_class(plot, "ggplot")
})

test_that("plot_factor_loadings handles use_clean parameter", {
  responses_df <- get_responses_df()
  
  efa_result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2
    )
  })
  
  # Test with clean loadings
  plot_clean <- plot_factor_loadings(efa_result, use_clean = TRUE)
  expect_s3_class(plot_clean, "ggplot")
  
  # Test with all loadings
  plot_all <- plot_factor_loadings(efa_result, use_clean = FALSE)
  expect_s3_class(plot_all, "ggplot")
})

test_that("plot_variance_explained creates valid plot", {
  responses_df <- get_responses_df()
  
  efa_result <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2
    )
  })
  
  plot <- plot_variance_explained(efa_result)
  
  expect_s3_class(plot, "ggplot")
})

test_that("plot_factor_correlations handles oblique vs orthogonal", {
  responses_df <- get_responses_df()
  
  # Oblique rotation (should return plot)
  efa_oblique <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,
      rotate = "oblimin"
    )
  })
  
  plot_oblique <- suppressMessages({
    plot_factor_correlations(efa_oblique)
  })
  expect_s3_class(plot_oblique, "ggplot")
  
  # Orthogonal rotation (should return NULL with message)
  efa_orthogonal <- suppressMessages({
    run_efa(
      responses_df,
      program_name = "Military",
      milestone_name = "100% (Corrected Final Design)",
      nfactors = 2,
      rotate = "varimax"
    )
  })
  
  expect_message(
    plot_orthogonal <- plot_factor_correlations(efa_orthogonal),
    "No factor correlations available"
  )
  expect_null(plot_orthogonal)
})