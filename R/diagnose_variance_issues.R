#' @title Diagnose Variance Issues in Wide Data
#' @description Pre-flight check for correlation analysis. Identifies items
#'   with zero or near-zero variance that would cause cor() to fail.
#' @param wide_data data.frame; Wide format response data
#' @param tolerance numeric; Minimum variance threshold. Default uses R's
#'   machine epsilon per R FAQ 7.31
#' @returns list with diagnostic information
#' @export
#' @references
#'   R FAQ 7.31: Why doesn't R think these numbers are equal?
#'   https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f
#'
#'   Burns, P. (2011). The R Inferno. Circle 1: Floating Point Comparison.
#'   https://www.burns-stat.com/pages/Tutor/R_inferno.pdf
#'
#' @section Development Notes:
#'   This function was updated 2026-02-22 to include robust error handling
#'   for sparse contingency tables and NA correlations per psych::polychoric()
#'   documentation. See `dev/sessions/2026-02-22.md` for complete development context.
#'
diagnose_variance_issues <- function(
  wide_data,
  tolerance = .Machine$double.eps^0.5
) {
  if (!is.data.frame(wide_data)) {
    stop("wide_data must be a data.frame")
  }

  # Compute variance and SD for all items
  item_stats <- lapply(names(wide_data), function(item_name) {
    x <- wide_data[[item_name]]

    # Get statistics
    var_val <- var(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
    n_unique <- length(unique(x[!is.na(x)]))
    n_obs <- sum(!is.na(x))

    # Check if problematic
    is_problematic <- is.na(var_val) ||
      is.na(sd_val) ||
      var_val < tolerance ||
      sd_val < tolerance

    # Determine reason
    reason <- if (is.na(var_val) || is.na(sd_val)) {
      "NA variance/SD (all values identical or all NA)"
    } else if (var_val < tolerance) {
      "Variance below tolerance (effectively zero)"
    } else if (sd_val < tolerance) {
      "SD below tolerance (floating point issue)"
    } else {
      "OK"
    }

    list(
      item = item_name,
      n_obs = n_obs,
      n_unique = n_unique,
      variance = var_val,
      sd = sd_val,
      is_problematic = is_problematic,
      reason = reason
    )
  })

  stats_df <- do.call(rbind, lapply(item_stats, as.data.frame))

  # Identify problematic items
  problematic <- stats_df[stats_df$is_problematic, ]

  # Summary message
  if (nrow(problematic) > 0) {
    message("\n=== Variance Diagnostics ===")
    message(paste("Problematic items:", nrow(problematic), "/", nrow(stats_df)))
    message("\nDetails:")
    for (i in 1:nrow(problematic)) {
      message(paste(
        " ",
        problematic$item[i],
        "\n    Unique values:",
        problematic$n_unique[i],
        "\n    Variance:",
        format(problematic$variance[i], scientific = TRUE),
        "\n    SD:",
        format(problematic$sd[i], scientific = TRUE),
        "\n    Reason:",
        problematic$reason[i]
      ))
    }
  } else {
    message("No variance issues detected")
  }

  list(
    all_items = stats_df,
    problematic_items = problematic,
    n_problematic = nrow(problematic),
    tolerance = tolerance,
    recommendation = if (nrow(problematic) > 0) {
      paste(
        "Remove",
        nrow(problematic),
        "problematic items before attempting correlation analysis.",
        "These items have insufficient variation for cor() computation."
      )
    } else {
      "Data appear suitable for correlation analysis."
    }
  )
}
