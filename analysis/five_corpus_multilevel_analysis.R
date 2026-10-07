# Multilevel analysis of engagement in five social-media corpora
#
# This script applies the same analysis separately to:
#   1. Reddit posts;
#   2. X posts;
#   3. YouTube comments;
#   4. YouTube titles and descriptions;
#   5. Stack Exchange questions.
#
# For each corpus, it fits:
#   1. common within-community slopes for all four content features;
#   2. slopes that vary across communities (partially pooled)
#
# Each corpus is standardised independently. The models are also fitted
# independently. 
# Engagement uses log1p() in the four nonnegative corpora and asinh() for
# Stack Exchange, where question scores can be negative because of downvotes.
# The YouTube-comments model omits the in/out-balance random slope because its
# variance was estimated as exactly zero in the full model, causing singularity.
#
# Usage:
#   Rscript five_corpus_multilevel_analysis.R [data_directory] [output_directory]
#
# Defaults:
#   data:    ../data
#   output:  five_corpus_multilevel_results

library(data.table)
library(lme4)

arguments <- commandArgs(trailingOnly = TRUE)
script_argument <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run this script with Rscript.", call. = FALSE)
}
script_directory <- dirname(normalizePath(sub("^--file=", "", script_argument)))
project_directory <- normalizePath(file.path(script_directory, ".."))
data_directory <- if (length(arguments) >= 1L) {
  arguments[[1L]]
} else {
  file.path(project_directory, "data")
}
output_directory <- if (length(arguments) >= 2L) {
  arguments[[2L]]
} else {
  file.path(project_directory, "analysis", "five_corpus_multilevel_results")
}

corpus_config <- data.table(
  corpus = c(
    "reddit",
    "x",
    "youtube_comments",
    "youtube_title_desc",
    "stackexchange"
  ),
  file_name = c(
    "reddit_posts.csv",
    "x_tweets.csv",
    "youtube_comments.csv",
    "youtube_title_desc.csv",
    "stackexchange_questions.csv"
  ),
  outcome_transform = c(
    "log1p",
    "log1p",
    "log1p",
    "log1p",
    "asinh"
  ),
  random_slope_spec = c(
    "all_features",
    "all_features",
    "without_in_out_balance",
    "all_features",
    "all_features"
  )
)
corpus_config[, data_path := file.path(data_directory, file_name)]

features <- c("negativity", "moralisation", "arousal", "in_out_balance")
z_features <- paste0("z_", features)
within_features <- paste0("within_", z_features)

# scale() uses R's default sample standard deviation.
standard_z <- function(x) {
  as.numeric(scale(x))
}

transform_engagement <- function(x, transformation) {
  if (any(!is.finite(x))) {
    stop(
      "Engagement contains missing or non-finite values after preprocessing.",
      call. = FALSE
    )
  }
  if (transformation == "log1p") {
    if (any(x < 0)) {
      stop(
        "log1p was requested but engagement contains negative values.",
        call. = FALSE
      )
    }
    return(log1p(x))
  }
  if (transformation == "asinh") {
    return(asinh(x))
  }
  stop("Unknown outcome transformation: ", transformation, call. = FALSE)
}

fixed_effects_table <- function(model, model_name) {
  coefficients <- summary(model)$coefficients
  data.table(
    model = model_name,
    term = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    standard_error = coefficients[, "Std. Error"],
    statistic = coefficients[, "t value"],
    ci_low = coefficients[, "Estimate"] - 1.96 * coefficients[, "Std. Error"],
    ci_high = coefficients[, "Estimate"] + 1.96 * coefficients[, "Std. Error"]
  )
}

add_corpus_column <- function(table, corpus_name) {
  table <- copy(table)
  table[, corpus := corpus_name]
  setcolorder(table, c("corpus", setdiff(names(table), "corpus")))
  table
}

analyse_corpus <- function(
    corpus_name,
    data_path,
    outcome_transform,
    random_slope_spec,
    corpus_output_directory) {
  message("Starting corpus: ", corpus_name)

  if (!file.exists(data_path)) {
    stop("Input file not found: ", data_path, call. = FALSE)
  }
  dir.create(
    corpus_output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # ---------------------------------------------------------------------------
  # 1. Prepare this corpus
  # ---------------------------------------------------------------------------

  required_columns <- c("item_id", "community", "engagement", features)
  corpus_data <- fread(
    data_path,
    select = required_columns,
    showProgress = TRUE
  )
  raw_rows <- nrow(corpus_data)

  numeric_columns <- c("engagement", features)
  for (column in numeric_columns) {
    set(
      corpus_data,
      j = column,
      value = suppressWarnings(as.numeric(corpus_data[[column]]))
    )
  }

  duplicate_rows <- duplicated(
    corpus_data,
    by = c("community", "item_id")
  )
  duplicate_count <- sum(duplicate_rows)
  if (duplicate_count > 0L) {
    corpus_data <- corpus_data[!duplicate_rows]
  }

  valid_rows <- !is.na(corpus_data$community) &
    nzchar(as.character(corpus_data$community))
  for (column in c("engagement", features)) {
    valid_rows <- valid_rows & is.finite(corpus_data[[column]])
  }
  corpus_data <- corpus_data[valid_rows]

  if (nrow(corpus_data) == 0L) {
    stop(
      "No complete observations remain after preprocessing for corpus: ",
      corpus_name,
      call. = FALSE
    )
  }
  corpus_data[, community := as.character(community)]
  corpus_data[, transformed_engagement := transform_engagement(
    engagement,
    outcome_transform
  )]

  # Standardisation is performed separately within each corpus.
  corpus_data[, z_y := standard_z(transformed_engagement)]
  for (feature in features) {
    corpus_data[, (paste0("z_", feature)) := standard_z(get(feature))]
  }

  # Group-mean centring makes the coefficients within-community slopes.
  for (index in seq_along(z_features)) {
    source <- z_features[[index]]
    target <- within_features[[index]]
    corpus_data[
      ,
      (target) := get(source) - mean(get(source)),
      by = community
    ]
  }

  setorder(corpus_data, community)

  audit <- data.table(
    outcome_transform = outcome_transform,
    random_slope_spec = random_slope_spec,
    raw_rows = raw_rows,
    duplicate_rows_removed = duplicate_count,
    complete_rows = nrow(corpus_data),
    rows_excluded_after_preprocessing =
      raw_rows - duplicate_count - nrow(corpus_data),
    communities = uniqueN(corpus_data$community),
    minimum_community_n = corpus_data[, .N, by = community][, min(N)],
    median_community_n = corpus_data[, .N, by = community][, median(N)],
    maximum_community_n = corpus_data[, .N, by = community][, max(N)],
    zero_engagement = sum(corpus_data$engagement == 0),
    negative_engagement = sum(corpus_data$engagement < 0)
  )

  # ---------------------------------------------------------------------------
  # 2. Common within-community slopes
  # ---------------------------------------------------------------------------

  fixed_part <- paste(within_features, collapse = " + ")

  common_formula <- as.formula(
    paste("z_y ~", fixed_part, "+ (1 | community)")
  )

  model_control <- lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )

  common_model <- lmer(
    formula = common_formula,
    data = corpus_data,
    REML = TRUE,
    control = model_control
  )

  # ---------------------------------------------------------------------------
  # 3. Community-varying, partially pooled slopes
  # ---------------------------------------------------------------------------

  random_slope_features <- if (random_slope_spec == "all_features") {
    within_features
  } else if (random_slope_spec == "without_in_out_balance") {
    setdiff(within_features, "within_z_in_out_balance")
  } else {
    stop(
      "Unknown random-slope specification: ",
      random_slope_spec,
      call. = FALSE
    )
  }
  random_slope_part <- paste(random_slope_features, collapse = " + ")

  varying_formula <- as.formula(
    paste(
      "z_y ~", fixed_part,
      "+ (1 +", random_slope_part, "|| community)"
    )
  )

  varying_model <- lmer(
    formula = varying_formula,
    data = corpus_data,
    REML = TRUE,
    control = model_control
  )

  # ---------------------------------------------------------------------------
  # 4. Construct compact result tables
  # ---------------------------------------------------------------------------

  fixed_effects <- rbindlist(list(
    fixed_effects_table(common_model, "common_slopes"),
    fixed_effects_table(varying_model, "varying_slopes")
  ))

  random_effects <- as.data.table(as.data.frame(VarCorr(varying_model)))
  setnames(
    random_effects,
    c("grp", "var1", "var2", "vcov", "sdcor"),
    c(
      "group",
      "term_1",
      "term_2",
      "variance_or_covariance",
      "sd_or_correlation"
    )
  )

  # Record slopes omitted from the final random-effects structure as
  # unavailable rather than as estimated zero-variance components. This keeps
  # the result tables complete without implying that a random-slope
  # distribution was estimated for those features.
  omitted_random_slope_terms <- setdiff(
    within_features,
    random_effects$term_1
  )
  if (length(omitted_random_slope_terms) > 0L) {
    random_effects <- rbindlist(list(
      random_effects,
      data.table(
        group = "not_estimated",
        term_1 = omitted_random_slope_terms,
        term_2 = NA_character_,
        variance_or_covariance = NA_real_,
        sd_or_correlation = NA_real_
      )
    ), use.names = TRUE)
  }

  community_slopes <- as.data.table(
    coef(varying_model)$community,
    keep.rownames = "community"
  )
  # An omitted random slope is absent from coef(). Add an NA column so that
  # Dataset S1 retains all four features without presenting the fixed effect as
  # though it were a fitted community-specific slope.
  missing_community_slope_terms <- setdiff(
    within_features,
    names(community_slopes)
  )
  for (term in missing_community_slope_terms) {
    community_slopes[, (term) := NA_real_]
  }
  # coef() may repeat a fixed-only coefficient for every community. Replace
  # such repeated values with NA because no community-specific slope was
  # estimated for an omitted random-slope term.
  for (term in omitted_random_slope_terms) {
    community_slopes[, (term) := NA_real_]
  }
  community_sizes <- corpus_data[
    ,
    .(n = .N, mean_z_y = mean(z_y)),
    by = community
  ]
  community_slopes <- merge(
    community_sizes,
    community_slopes,
    by = "community",
    all.x = TRUE,
    sort = TRUE
  )
  setnames(community_slopes, "(Intercept)", "intercept")

  model_comparison <- rbindlist(lapply(
    list(common_slopes = common_model, varying_slopes = varying_model),
    function(model) {
      likelihood <- logLik(model)
      data.table(
        n_observations = nobs(model),
        n_parameters = attr(likelihood, "df"),
        REML_log_likelihood = as.numeric(likelihood),
        AIC = AIC(model),
        BIC = BIC(model),
        residual_sd = sigma(model),
        singular_fit = isSingular(model, tol = 1e-4)
      )
    }
  ), idcol = "model")

  # ---------------------------------------------------------------------------
  # 5. Magnitude concealed by signed averaging
  # ---------------------------------------------------------------------------

  magnitude_summary <- merge(
    fixed_effects[
      model == "varying_slopes" & term %in% within_features,
      .(
        term,
        average_slope = estimate,
        average_standard_error = standard_error,
        average_ci_low = ci_low,
        average_ci_high = ci_high
      )
    ],
    random_effects[
      term_1 %in% within_features & (is.na(term_2) | term_2 == ""),
      .(
        term = term_1,
        random_slope_variance = variance_or_covariance,
        random_slope_sd = sd_or_correlation
      )
    ],
    by = "term",
    all = TRUE,
    sort = FALSE
  )

  magnitude_summary[, feature := sub("^within_z_", "", term)]
  magnitude_summary[, random_slope_estimated :=
    term %in% random_slope_features]
  magnitude_summary[, display_order := match(term, within_features)]
  setorder(magnitude_summary, display_order)
  magnitude_summary[, display_order := NULL]

  if (nrow(magnitude_summary) != length(within_features) ||
      any(!is.finite(magnitude_summary$average_slope)) ||
      any(
        magnitude_summary$random_slope_estimated &
          !is.finite(magnitude_summary$random_slope_sd)
      )) {
    stop(
      "Could not match all fixed slopes to their random-slope standard ",
      "deviations for corpus: ",
      corpus_name,
      call. = FALSE
    )
  }

  magnitude_summary[, `:=`(
    expected_absolute_slope = NA_real_,
    probability_positive = NA_real_
  )]

  estimated_tau <- magnitude_summary$random_slope_estimated &
    is.finite(magnitude_summary$random_slope_sd)
  magnitude_summary[
    estimated_tau,
    `:=`(
      expected_absolute_slope = abs(average_slope),
      probability_positive = as.numeric(average_slope > 0)
    )
  ]

  positive_tau <- estimated_tau & magnitude_summary$random_slope_sd > 0
  magnitude_summary[
    positive_tau,
    expected_absolute_slope :=
      random_slope_sd * sqrt(2 / pi) *
        exp(-average_slope^2 / (2 * random_slope_variance)) +
      average_slope *
        (2 * pnorm(average_slope / random_slope_sd) - 1)
  ]
  magnitude_summary[
    positive_tau,
    probability_positive := pnorm(average_slope / random_slope_sd)
  ]

  magnitude_summary[, `:=`(
    absolute_average_slope = abs(average_slope),
    root_mean_square_slope = NA_real_
  )]
  magnitude_summary[
    estimated_tau,
    root_mean_square_slope := sqrt(
      average_slope^2 + random_slope_variance
    )
  ]
  magnitude_summary[, magnitude_to_average_ratio := NA_real_]
  magnitude_summary[
    estimated_tau & absolute_average_slope > 0,
    magnitude_to_average_ratio :=
      expected_absolute_slope / absolute_average_slope
  ]
  magnitude_summary[
    estimated_tau & absolute_average_slope == 0,
    magnitude_to_average_ratio := Inf
  ]
  magnitude_summary[, cancellation_index := NA_real_]
  magnitude_summary[
    estimated_tau & expected_absolute_slope > 0,
    cancellation_index :=
      1 - absolute_average_slope / expected_absolute_slope
  ]

  setcolorder(
    magnitude_summary,
    c(
      "feature",
      "term",
      "average_slope",
      "average_standard_error",
      "average_ci_low",
      "average_ci_high",
      "random_slope_estimated",
      "random_slope_sd",
      "random_slope_variance",
      "absolute_average_slope",
      "expected_absolute_slope",
      "root_mean_square_slope",
      "probability_positive",
      "magnitude_to_average_ratio",
      "cancellation_index"
    )
  )

  # ---------------------------------------------------------------------------
  # 6. Label and save this corpus's results
  # ---------------------------------------------------------------------------

  audit <- add_corpus_column(audit, corpus_name)
  fixed_effects <- add_corpus_column(fixed_effects, corpus_name)
  random_effects <- add_corpus_column(random_effects, corpus_name)
  community_slopes <- add_corpus_column(community_slopes, corpus_name)
  model_comparison <- add_corpus_column(model_comparison, corpus_name)
  magnitude_summary <- add_corpus_column(magnitude_summary, corpus_name)

  fwrite(
    audit,
    file.path(corpus_output_directory, "data_audit.csv")
  )
  fwrite(
    fixed_effects,
    file.path(corpus_output_directory, "fixed_effects.csv")
  )
  fwrite(
    random_effects,
    file.path(corpus_output_directory, "random_effects.csv")
  )
  fwrite(
    community_slopes,
    file.path(
      corpus_output_directory,
      "community_partially_pooled_slopes.csv"
    )
  )
  fwrite(
    model_comparison,
    file.path(corpus_output_directory, "model_comparison.csv")
  )
  fwrite(
    magnitude_summary,
    file.path(
      corpus_output_directory,
      "magnitude_concealed_by_signed_averaging.csv"
    )
  )

  writeLines(
    c(
      paste("CORPUS:", corpus_name),
      paste("OUTCOME TRANSFORMATION:", outcome_transform),
      paste("RANDOM-SLOPE SPECIFICATION:", random_slope_spec),
      "",
      "COMMON-SLOPE MODEL",
      capture.output(summary(common_model)),
      "",
      "VARYING-SLOPE MODEL",
      capture.output(summary(varying_model))
    ),
    file.path(corpus_output_directory, "model_summaries.txt")
  )

  message(
    "Completed corpus: ",
    corpus_name,
    " (", nrow(corpus_data), " observations; ",
    uniqueN(corpus_data$community), " communities)"
  )

  # Return only compact tables. The fitted models and full corpus data can then
  # be released before the next corpus is analysed.
  list(
    audit = audit,
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    community_slopes = community_slopes,
    model_comparison = model_comparison,
    magnitude_summary = magnitude_summary
  )
}

# -----------------------------------------------------------------------------
# 7. Run the five separate analyses
# -----------------------------------------------------------------------------

missing_files <- corpus_config[!file.exists(data_path)]
if (nrow(missing_files) > 0L) {
  stop(
    "The following input files were not found: ",
    paste(missing_files$data_path, collapse = ", "),
    call. = FALSE
  )
}

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
fwrite(
  corpus_config,
  file.path(output_directory, "corpus_manifest.csv")
)

analysis_results <- vector("list", nrow(corpus_config))
names(analysis_results) <- corpus_config$corpus

for (index in seq_len(nrow(corpus_config))) {
  corpus_name <- corpus_config$corpus[[index]]
  corpus_path <- corpus_config$data_path[[index]]
  outcome_transform <- corpus_config$outcome_transform[[index]]
  random_slope_spec <- corpus_config$random_slope_spec[[index]]
  corpus_output_directory <- file.path(output_directory, corpus_name)

  analysis_results[[corpus_name]] <- analyse_corpus(
    corpus_name = corpus_name,
    data_path = corpus_path,
    outcome_transform = outcome_transform,
    random_slope_spec = random_slope_spec,
    corpus_output_directory = corpus_output_directory
  )

  invisible(gc())
}

# -----------------------------------------------------------------------------
# 8. Save combined tables for cross-corpus comparison and plotting
# -----------------------------------------------------------------------------

combine_result <- function(result_name) {
  rbindlist(
    lapply(analysis_results, function(result) result[[result_name]]),
    use.names = TRUE,
    fill = TRUE
  )
}

all_audits <- combine_result("audit")
all_fixed_effects <- combine_result("fixed_effects")
all_random_effects <- combine_result("random_effects")
all_community_slopes <- combine_result("community_slopes")
all_model_comparisons <- combine_result("model_comparison")
all_magnitude_summaries <- combine_result("magnitude_summary")

fwrite(
  all_audits,
  file.path(output_directory, "all_corpora_data_audit.csv")
)
fwrite(
  all_fixed_effects,
  file.path(output_directory, "all_corpora_fixed_effects.csv")
)
fwrite(
  all_random_effects,
  file.path(output_directory, "all_corpora_random_effects.csv")
)
fwrite(
  all_community_slopes,
  file.path(
    output_directory,
    "all_corpora_community_partially_pooled_slopes.csv"
  )
)
fwrite(
  all_model_comparisons,
  file.path(output_directory, "all_corpora_model_comparison.csv")
)
fwrite(
  all_magnitude_summaries,
  file.path(
    output_directory,
    "all_corpora_magnitude_concealed_by_signed_averaging.csv"
  )
)

message("All five analyses complete. Results written to: ", output_directory)
