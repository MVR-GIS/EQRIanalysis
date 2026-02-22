# Per Little & Rubin (2019): Structural missingness requires context-aware analysis

library(EQRIanalysis)

responses_df <- get_responses_df()

# ===========================================================================
# STRATEGY 1: Identify "Core Items" (Asked Across All/Most Contexts)
# ===========================================================================

cat("=== STEP 1: Identify Core Items ===\n")
cat("Per Little & Rubin (2019): Focus on items common across contexts\n\n")

# Calculate coverage per question
question_coverage <- responses_df %>%
  group_by(QUESTION_NUMBER) %>%
  summarise(
    n_contexts = n_distinct(paste(PROGRAMTYPE_NAME, MILESTONE_DESC)),
    n_total_responses = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_contexts))

# Define "core" as present in >= 8 of 10 contexts (80% coverage)
# Per Schafer & Graham (2002): <20% missing acceptable
core_threshold <- 8

core_items <- question_coverage %>%
  filter(n_contexts >= core_threshold) %>%
  pull(QUESTION_NUMBER)

cat("Core items (present in >=", core_threshold, "of 10 contexts):\n")
cat("Total:", length(core_items), "\n")
print(core_items)

# Context-specific items
context_specific <- question_coverage %>%
  filter(n_contexts < core_threshold)

cat("\n\nContext-specific items (milestone/program-specific):\n")
cat("Total:", length(context_specific$QUESTION_NUMBER), "\n")
print(context_specific %>% select(QUESTION_NUMBER, n_contexts))

# ===========================================================================
# STRATEGY 2: Test Factorability on Core Items Only
# ===========================================================================

if (length(core_items) >= 3) {
  cat("\n\n=== STEP 2: Assess Factorability of Core Items ===\n")
  cat("Per Flora & Curran (2004): Need >= 3 items for factor analysis\n\n")
  
  # Filter responses to core items only
  responses_core <- responses_df %>%
    filter(QUESTION_NUMBER %in% core_items)
  
  # Convert to wide format (aggregated across all contexts)
  wide_core <- responses_core %>%
    distinct(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, .keep_all = TRUE) %>%
    select(QUESTIONNAIREEVENT_ID, QUESTION_NUMBER, RESPONSEVALUE) %>%
    pivot_wider(
      names_from = QUESTION_NUMBER,
      values_from = RESPONSEVALUE,
      names_prefix = "Q"
    ) %>%
    select(-QUESTIONNAIREEVENT_ID)
  
  # Check missingness in core items
  missing_core <- colMeans(is.na(wide_core)) * 100
  cat("Missing data in core items:\n")
  print(sort(missing_core, decreasing = TRUE))
  
  # Complete cases
  complete_core <- complete.cases(wide_core)
  n_complete <- sum(complete_core)
  
  cat(sprintf(
    "\n\nComplete cases for core items: %d / %d (%.1f%%)\n",
    n_complete, nrow(wide_core), 100 * n_complete / nrow(wide_core)
  ))
  
  if (n_complete >= 200) {
    cat("\nSUCCESS: Sufficient sample for polychoric FA (Flora & Curran, 2004)\n")
    cat("Recommendation: Proceed with Sprint 1 using", length(core_items), "core items\n")
  } else if (n_complete >= 10) {
    cat("\nWARNING: Sample below recommended N=200 for polychoric FA\n")
    cat("Recommendation: Use Pearson correlations or aggregate more data\n")
  } else {
    cat("\nINSUFFICIENT: Cannot perform factor analysis\n")
    cat("Recommendation: Analyze contexts separately\n")
  }
} else {
  cat("\n\nINSUFFICIENT CORE ITEMS\n")
  cat("Recommendation: Analyze context-specific factors separately\n")
}