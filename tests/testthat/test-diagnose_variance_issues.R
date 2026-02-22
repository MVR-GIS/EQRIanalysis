
responses_df <- get_responses_df()
wide_all <- get_wide_responses(responses_df, NULL, NULL)

# Run diagnostic
variance_check <- diagnose_variance_issues(wide_all)

# Examine problematic items
View(variance_check$problematic_items)

# For each problematic item, check distribution
for (item in variance_check$problematic_items$item) {
  cat("\n", item, ":\n")
  print(table(wide_all[[item]], useNA = "ifany"))
}