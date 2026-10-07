# Generate supplementary datasets and tables.
#
# The diagnostic directory must contain the CSV files created by the earlier
# Reddit diagnostic scripts. The fourth argument is optional; by default the
# script reads the combined community-slope file from the results directory.
# With no arguments, paths are resolved relative to this script's project.
# Diagnostic CSVs default to supplementary_materials/reddit_diagnostics.
# Datasets S1-S2 and Tables S1-S3 are generated from the multilevel analysis
# outputs. If diagnostic CSVs are missing, Table S4 is skipped and
# missing paths are reported.
#
# Usage:
#   Rscript supplementary_materials/scripts/prepare_supplementary_materials.R \
#     analysis/five_corpus_multilevel_results \
#     supplementary_materials/reddit_diagnostics \
#     supplementary_materials \
#     all_corpora_community_partially_pooled_slopes.csv

library(data.table)

arguments <- commandArgs(trailingOnly = TRUE)
script_argument <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run this script with Rscript (see Usage above).", call. = FALSE)
}
script_directory <- dirname(normalizePath(sub("^--file=", "", script_argument)))
project_directory <- normalizePath(file.path(script_directory, "..", ".."))
results_directory <- if (length(arguments) >= 1L) {
  arguments[[1L]]
} else {
  file.path(project_directory, "analysis", "five_corpus_multilevel_results")
}
diagnostic_directory <- if (length(arguments) >= 2L) {
  arguments[[2L]]
} else {
  file.path(project_directory, "supplementary_materials", "reddit_diagnostics")
}
output_directory <- if (length(arguments) >= 3L) {
  arguments[[3L]]
} else {
  file.path(project_directory, "supplementary_materials")
}
community_slopes_path <- if (length(arguments) >= 4L) {
  arguments[[4L]]
} else {
  file.path(
    results_directory,
    "all_corpora_community_partially_pooled_slopes.csv"
  )
}

required_result_paths <- file.path(results_directory, c(
  "all_corpora_fixed_effects.csv",
  "all_corpora_data_audit.csv",
  "all_corpora_model_comparison.csv",
  "all_corpora_magnitude_concealed_by_signed_averaging.csv",
  "all_corpora_community_partially_pooled_slopes.csv"
))
missing_result_paths <- required_result_paths[!file.exists(required_result_paths)]
if (length(missing_result_paths) > 0L) {
  stop("Missing required analysis file(s):\n",
       paste(missing_result_paths, collapse = "\n"), call. = FALSE)
}
fixed_effects <- fread(file.path(results_directory, "all_corpora_fixed_effects.csv"))

table_directory <- file.path(output_directory, "tables")
dir.create(table_directory, recursive = TRUE, showWarnings = FALSE)

corpus_labels <- c(
  reddit = "Reddit",
  x = "X",
  stackexchange = "Stack Exchange",
  youtube_title_desc = "YouTube titles and descriptions",
  youtube_comments = "YouTube comments"
)
corpus_keys <- names(corpus_labels)

feature_labels <- c(
  negativity = "Negativity",
  moralisation = "Moralization",
  arousal = "Arousal",
  in_out_balance = "In-group versus out-group balance"
)

# ---------------------------------------------------------------------------
# Datasets S1-S2  Community-level slopes and model fixed effects
# ---------------------------------------------------------------------------

# Dataset S1 contains the partially pooled community coefficients from the
# varying-slope models. Dataset S2 contains fixed-effect estimates from both
# the common-slope and varying-slope models.
dataset_s1_path <- file.path(
  results_directory,
  "all_corpora_community_partially_pooled_slopes.csv"
)
dataset_s1 <- fread(dataset_s1_path)
required_dataset_s1_columns <- c(
  "corpus", "community", "n", "mean_z_y", "intercept",
  "within_z_negativity", "within_z_moralisation", "within_z_arousal",
  "within_z_in_out_balance"
)
missing_dataset_s1_columns <- setdiff(
  required_dataset_s1_columns,
  names(dataset_s1)
)
if (length(missing_dataset_s1_columns) > 0L ||
    anyDuplicated(dataset_s1, by = c("corpus", "community"))) {
  stop(
    "Dataset S1 is missing required fields or has duplicate corpus-community rows.",
    call. = FALSE
  )
}
setcolorder(dataset_s1, required_dataset_s1_columns)
fwrite(
  dataset_s1,
  file.path(output_directory, "dataset_s1_community_partially_pooled_slopes.csv"),
  na = "NA"
)

required_dataset_s2_columns <- c(
  "corpus", "model", "term", "estimate", "standard_error", "statistic",
  "ci_low", "ci_high"
)
missing_dataset_s2_columns <- setdiff(
  required_dataset_s2_columns,
  names(fixed_effects)
)
if (length(missing_dataset_s2_columns) > 0L ||
    anyDuplicated(fixed_effects, by = c("corpus", "model", "term"))) {
  stop(
    "Dataset S2 is missing required fields or has duplicate corpus-model-term rows.",
    call. = FALSE
  )
}
dataset_s2 <- fixed_effects[, ..required_dataset_s2_columns]
fwrite(
  dataset_s2,
  file.path(output_directory, "dataset_s2_fixed_effects.csv"),
  na = "NA"
)
message("Datasets S1-S2 written to: ", output_directory)

# ---------------------------------------------------------------------------
# Table S1  Corpus description and preprocessing
# ---------------------------------------------------------------------------

audit <- fread(file.path(results_directory, "all_corpora_data_audit.csv"))
audit[, corpus_label := corpus_labels[corpus]]
audit[, corpus_order := match(corpus, corpus_keys)]
audit[, `:=`(
  zero_engagement_percent = 100 * zero_engagement / complete_rows,
  negative_engagement_percent = 100 * negative_engagement / complete_rows
)]
table_s1 <- audit[
  order(corpus_order),
  .(
    corpus = corpus_label,
    raw_rows,
    duplicate_rows_removed,
    complete_rows,
    rows_excluded_after_preprocessing,
    communities,
    minimum_community_n,
    median_community_n,
    maximum_community_n,
    zero_engagement_percent,
    negative_engagement_percent
  )
]
fwrite(table_s1, file.path(table_directory, "table_s1_corpus_description.csv"))

# ---------------------------------------------------------------------------
# Table S2  Model comparison
# ---------------------------------------------------------------------------

comparison <- fread(
  file.path(results_directory, "all_corpora_model_comparison.csv")
)
common <- comparison[model == "common_slopes"]
varying <- comparison[model == "varying_slopes"]
table_s2 <- merge(
  common,
  varying,
  by = "corpus",
  suffixes = c("_common", "_varying")
)
table_s2[, `:=`(
  corpus_label = corpus_labels[corpus],
  corpus_order = match(corpus, corpus_keys),
  delta_AIC = AIC_common - AIC_varying,
  delta_BIC = BIC_common - BIC_varying
)]
table_s2 <- table_s2[
  order(corpus_order),
  .(
    corpus = corpus_label,
    n_parameters_common,
    n_parameters_varying,
    AIC_common,
    AIC_varying,
    delta_AIC,
    BIC_common,
    BIC_varying,
    delta_BIC
  )
]
fwrite(table_s2, file.path(table_directory, "table_s2_model_comparison.csv"))

# ---------------------------------------------------------------------------
# Table S3  Cross-corpus slopes and magnitude
# ---------------------------------------------------------------------------

magnitude <- fread(file.path(
  results_directory,
  "all_corpora_magnitude_concealed_by_signed_averaging.csv"
))
# Match the common-model coefficient to each varying-model summary by key.
# Panel B compares expected_absolute_slope with absolute_common_slope.
common_coefficients <- fixed_effects[
  model == "common_slopes" & term %in% paste0("within_z_", names(feature_labels)),
  .(corpus, feature = sub("^within_z_", "", term), common_slope = estimate)
]
expected_keys <- CJ(corpus = corpus_keys, feature = names(feature_labels))
for (source in list(common_coefficients, magnitude)) {
  if (nrow(source) != nrow(expected_keys) ||
      anyDuplicated(source, by = c("corpus", "feature")) ||
      nrow(merge(expected_keys, source, by = c("corpus", "feature"))) !=
        nrow(expected_keys)) {
    stop("Expected one summary for each of the 20 corpus-feature pairs.",
         call. = FALSE)
  }
}
magnitude <- merge(magnitude, common_coefficients,
                   by = c("corpus", "feature"), all.x = TRUE, sort = FALSE)
if (any(!is.finite(magnitude$common_slope))) {
  stop("Could not match every summary to a finite common-slope coefficient.",
       call. = FALSE)
}
magnitude[, absolute_common_slope := abs(common_slope)]

magnitude[, `:=`(
  corpus_label = corpus_labels[corpus],
  feature_label = feature_labels[feature],
  corpus_order = match(corpus, corpus_keys),
  feature_order = match(feature, names(feature_labels))
)]
table_s3 <- magnitude[
  order(corpus_order, feature_order),
  .(
    corpus = corpus_label,
    feature = feature_label,
    common_slope,
    average_slope,
    average_ci_low,
    average_ci_high,
    random_slope_sd,
    absolute_common_slope,
    expected_absolute_slope
  )
]
fwrite(
  table_s3,
  file.path(table_directory, "table_s3_slopes_and_concealed_magnitude.csv"),
  na = "NA"
)

# ---------------------------------------------------------------------------
# Table S4  Exploratory Reddit diagnostics
# ---------------------------------------------------------------------------

diagnostic_paths <- file.path(diagnostic_directory, c(
  "reddit_negativity_diagnostic_coefficients.csv",
  "reddit_positive_negativity_coefficients.csv",
  "reddit_knitting_arousal_coefficients.csv",
  "reddit_negativity_decile_diagnostics.csv",
  "reddit_positive_negativity_group_diagnostics.csv",
  "reddit_knitting_arousal_deciles.csv"
))
missing_diagnostic_paths <- diagnostic_paths[!file.exists(diagnostic_paths)]
if (length(missing_diagnostic_paths) > 0L) {
  message("Datasets S1-S2 and Tables S1-S3 written to: ", output_directory)
  message(
    "Table S4 was NOT generated or refreshed.\n",
    "Missing Reddit diagnostic file(s):\n",
    paste(missing_diagnostic_paths, collapse = "\n"),
    "\nRerun with the diagnostic folder as the second argument."
  )
  quit(save = "no", status = 0L)
}

negative_coefficients <- fread(file.path(
  diagnostic_directory,
  "reddit_negativity_diagnostic_coefficients.csv"
))
positive_coefficients <- fread(file.path(
  diagnostic_directory,
  "reddit_positive_negativity_coefficients.csv"
))
knitting_coefficients <- fread(file.path(
  diagnostic_directory,
  "reddit_knitting_arousal_coefficients.csv"
))

if (!file.exists(community_slopes_path)) {
  stop(
    "Partially pooled community-slope file not found: ",
    community_slopes_path,
    call. = FALSE
  )
}
community_slopes <- fread(community_slopes_path)

# The combined Dataset S1 has a corpus column. The single-corpus Reddit output
# has the same slope columns but no corpus column, so both formats are accepted.
if ("corpus" %in% names(community_slopes)) {
  community_slopes <- community_slopes[corpus == "reddit"]
}

required_slope_columns <- c(
  "community",
  "within_z_negativity",
  "within_z_arousal"
)
missing_slope_columns <- setdiff(
  required_slope_columns,
  names(community_slopes)
)
if (length(missing_slope_columns) > 0L) {
  stop(
    "The community-slope file is missing required columns: ",
    paste(missing_slope_columns, collapse = ", "),
    call. = FALSE
  )
}

diagnostic_rows <- rbindlist(list(
  negative_coefficients[, .(
    community,
    feature = "Negativity",
    n
  )],
  positive_coefficients[, .(
    community,
    feature = "Negativity",
    n
  )],
  knitting_coefficients[, .(
    community,
    feature = "Arousal",
    n
  )]
), use.names = TRUE)

partially_pooled_slopes <- rbindlist(list(
  community_slopes[, .(
    community,
    feature = "Negativity",
    partially_pooled_slope = within_z_negativity
  )],
  community_slopes[, .(
    community,
    feature = "Arousal",
    partially_pooled_slope = within_z_arousal
  )]
), use.names = TRUE)

table_s4 <- merge(
  diagnostic_rows,
  partially_pooled_slopes,
  by = c("community", "feature"),
  all.x = TRUE,
  sort = FALSE
)

if (any(!is.finite(table_s4$partially_pooled_slope))) {
  stop(
    "Could not match every Table S4 row to a partially pooled slope.",
    call. = FALSE
  )
}

table_s4[, pattern := fifelse(
  community == "AmItheAsshole",
  "Engagement declines mainly in the highest negativity deciles",
  fifelse(
    community == "tifu",
    "Engagement declines progressively at higher negativity",
    fifelse(
      community %in% c("cscareerquestions", "medicine"),
      "Engagement increases across the higher negativity range",
      "Engagement increases sharply in the highest arousal deciles"
    )
  )
)]
table_s4[, display_order := match(
  community,
  c("AmItheAsshole", "tifu", "cscareerquestions", "medicine", "knitting")
)]
setorder(table_s4, display_order)
table_s4[, display_order := NULL]
fwrite(
  table_s4,
  file.path(table_directory, "table_s4_exploratory_reddit_diagnostics.csv")
)

message("Supplementary datasets and tables written to: ", output_directory)
