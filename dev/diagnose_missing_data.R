# Diagnostic: Understanding pairwise variance collapse
library(EQRIanalysis)
library(dplyr)

responses_df <- get_responses_df()

# Get the problematic context
wide_military_95 <- get_wide_responses(
  responses_df,
  program_name = "Military",
  milestone_name = "95% (Final Design)"
)

cat("=== Sample Size ===\n")
cat("Rows:", nrow(wide_military_95), "\n")
cat("Columns:", ncol(wide_military_95), "\n\n")

cat("=== Missing Data Pattern ===\n")
# Percent missing per column
missing_pct <- colMeans(is.na(wide_military_95)) * 100
cat("Items with >50% missing:\n")
print(names(missing_pct)[missing_pct > 50])
cat("\nOverall missing %:", mean(missing_pct), "\n\n")

cat("=== Pairwise Complete Cases ===\n")
# For first few pairs, show how many complete cases exist
for (i in 1:min(5, ncol(wide_military_95))) {
  for (j in (i + 1):min(5, ncol(wide_military_95))) {
    complete <- complete.cases(wide_military_95[, c(i, j)])
    n_complete <- sum(complete)

    # If complete pairs exist, check variance
    if (n_complete > 0) {
      var_i <- var(wide_military_95[complete, i])
      var_j <- var(wide_military_95[complete, j])

      cat(sprintf(
        "%s × %s: %d complete pairs, var1=%.3f, var2=%.3f\n",
        names(wide_military_95)[i],
        names(wide_military_95)[j],
        n_complete,
        var_i,
        var_j
      ))
    } else {
      cat(sprintf(
        "%s × %s: 0 complete pairs\n",
        names(wide_military_95)[i],
        names(wide_military_95)[j]
      ))
    }
  }
}

# Test correlation
cat("\n=== Correlation Test ===\n")
test_cor <- suppressWarnings(cor(
  wide_military_95,
  use = "pairwise.complete.obs"
))
n_na_cors <- sum(is.na(test_cor)) - ncol(wide_military_95) # Exclude diagonal
cat(
  "NA correlations:",
  n_na_cors,
  "out of",
  ncol(wide_military_95) * (ncol(wide_military_95) - 1) / 2,
  "total\n"
)
