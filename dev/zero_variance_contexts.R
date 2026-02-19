# Diagnostic script to identify zero-variance items by context
library(dplyr)
library(tidyr)

responses_df <- get_responses_df()

# Get all unique contexts
contexts <- responses_df %>%
  distinct(INDICATOR, PROGRAMTYPE_NAME, MILESTONE_DESC) %>%
  arrange(INDICATOR, PROGRAMTYPE_NAME, MILESTONE_DESC)

# Check each context for zero-variance items
context_summary <- list()

for (i in 1:nrow(contexts)) {
  ctx <- contexts[i, ]
  
  tryCatch({
    wide_df <- get_wide_responses(
      responses_df,
      ctx$INDICATOR,
      ctx$PROGRAMTYPE_NAME,
      ctx$MILESTONE_DESC
    )
    
    # Calculate variances
    vars <- sapply(wide_df, var, na.rm = TRUE)
    zero_var <- sum(vars == 0 | is.na(vars))
    
    context_summary[[i]] <- data.frame(
      indicator = ctx$INDICATOR,
      program = ctx$PROGRAMTYPE_NAME,
      milestone = ctx$MILESTONE_DESC,
      n_obs = nrow(wide_df),
      n_items = ncol(wide_df),
      n_zero_var = zero_var,
      viable = (ncol(wide_df) - zero_var) >= 2  # Can run alpha?
    )
  }, error = function(e) {
    context_summary[[i]] <- data.frame(
      indicator = ctx$INDICATOR,
      program = ctx$PROGRAMTYPE_NAME,
      milestone = ctx$MILESTONE_DESC,
      n_obs = 0,
      n_items = 0,
      n_zero_var = NA,
      viable = FALSE,
      error = e$message
    )
  })
}

# Combine results
summary_df <- do.call(rbind, context_summary)

# View problematic contexts
summary_df %>%
  filter(!viable) %>%
  arrange(indicator, program, milestone)
