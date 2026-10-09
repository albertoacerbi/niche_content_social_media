# Reddit diagnostics and Figure S5 from scored item-level data
#
#
# This scripts regenerates all six diagnostic CSVs (including tifu and medicine, 
# not reported in the manuscript) and the combined Figure S5 PDF/PNG. 
# It needs no precomputed diagnostic CSVs or multilevel fits. 
# The input already contains the four scored content features.
#
# Run from the project root:
#   Rscript supplementary_materials/scripts/generate_reddit_diagnostics.R
# Optional arguments: [reddit_posts.csv] [supplementary_output_directory]
# Defaults are resolved relative to this script.
#

library(data.table)
library(ggplot2)

arguments <- commandArgs(trailingOnly = TRUE)
script_argument <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run this script with Rscript.", call. = FALSE)
}
script_directory <- dirname(normalizePath(sub("^--file=", "", script_argument)))
project_directory <- normalizePath(file.path(script_directory, "..", ".."))
input_path <- if (length(arguments) >= 1L) arguments[[1L]] else {
  file.path(project_directory, "data", "reddit_posts.csv")
}
output_directory <- if (length(arguments) >= 2L) arguments[[2L]] else {
  file.path(project_directory, "supplementary_materials")
}
diagnostic_directory <- file.path(output_directory, "reddit_diagnostics")
figure_directory <- file.path(output_directory, "figures")

features <- c("negativity", "moralisation", "arousal", "in_out_balance")
required_columns <- c("community", "engagement", features)
if (!file.exists(input_path)) stop("Input not found: ", input_path, call. = FALSE)
data <- fread(input_path, select = required_columns)
if (!all(required_columns %in% names(data))) {
  stop("Input must contain: ", paste(required_columns, collapse = ", "),
       call. = FALSE)
}

data <- data[complete.cases(data) & nzchar(community)]
for (column in c("engagement", features)) {
  if (!is.numeric(data[[column]]) || any(!is.finite(data[[column]]))) {
    stop("Non-numeric or infinite values in ", column, call. = FALSE)
  }
}
if (nrow(data) == 0L || any(data$engagement < 0) || any(data$negativity < 0)) {
  stop("Expected complete Reddit observations with nonnegative engagement and negativity.",
       call. = FALSE)
}

population_z <- function(x) {
  centered <- x - mean(x)
  scale <- sqrt(mean(centered^2))
  if (!is.finite(scale) || scale == 0) stop("Cannot standardize a constant variable.")
  centered / scale
}
data[, z_y := population_z(log1p(engagement))]
for (feature in features) {
  data[, (paste0("z_", feature)) := population_z(get(feature))]
}

least_squares <- function(x, y) {
  decomposition <- svd(x)
  keep <- decomposition$d > max(dim(x)) * .Machine$double.eps * max(decomposition$d)
  if (!any(keep)) return(rep(0, ncol(x)))
  as.vector(decomposition$v[, keep, drop = FALSE] %*%
    (crossprod(decomposition$u[, keep, drop = FALSE], y) / decomposition$d[keep]))
}

quantile_bins <- function(x, groups) {
  # NumPy's linear quantile interpolation uses zero-based fractional indices.
  # Keep that arithmetic order: adding 1 before taking floor (R quantile type 7)
  # can round a near-integer index differently and move a boundary observation.
  ordered <- sort(x)
  # pandas passes probabilities through percentile units (q * 100 / 100).
  # Preserve that round-trip too; it affects a medicine decile boundary.
  probabilities <- (seq(0, 1, length.out = groups + 1L) * 100) / 100
  positions <- (length(ordered) - 1) * probabilities
  lower <- floor(positions)
  fraction <- positions - lower
  left <- ordered[lower + 1L]
  right <- ordered[pmin(lower + 2L, length(ordered))]
  edges <- unique(ifelse(fraction < 0.5,
                         left + (right - left) * fraction,
                         right - (right - left) * (1 - fraction)))
  if (length(edges) < 2L) {
    stop("Cannot form quantile bins from a constant focal feature.", call. = FALSE)
  }
  # Ties stay together; bins need not contain equal numbers of observations.
  as.integer(cut(x, breaks = edges, include.lowest = TRUE, right = TRUE,
                 labels = FALSE))
}

summarize_bins <- function(part, x, y, relationship, zero_aware = FALSE,
                           knitting = FALSE, bin_name = "decile") {
  values <- part[[x]]
  if (zero_aware) {
    bins <- rep(1L, length(values))
    positive <- values != 0
    if (any(positive)) bins[positive] <- quantile_bins(values[positive], 9L) + 1L
  } else {
    bins <- quantile_bins(values, 10L)
  }
  working <- data.table(community = part$community, bin = bins, x = values, y = part[[y]])
  result <- working[, .(n = .N, mean_x = mean(x), minimum_x = min(x),
                        maximum_x = max(x), mean_y = mean(y), sd_y = sd(y)),
                    by = .(community, bin)]
  setorder(result, community, bin)
  result[, standard_error := sd_y / sqrt(n)]
  result[, `:=`(ci_low = mean_y - 1.96 * standard_error,
                ci_high = mean_y + 1.96 * standard_error,
                relationship = relationship)]
  setnames(result, "bin", bin_name)
  if (knitting) result[, c("community", "minimum_x", "maximum_x") := NULL]
  result
}

make_diagnostic <- function(community_id, focal_feature, zero_aware = FALSE,
                            knitting = FALSE, bin_name = "decile") {
  part <- copy(data[community == community_id])
  if (nrow(part) < 2L) stop("Insufficient observations for ", community_id)
  part[, centred_z_y := z_y - mean(z_y)]
  for (feature in features) {
    z <- paste0("z_", feature)
    part[, (paste0("centred_", z)) := get(z) - mean(get(z))]
  }
  unadjusted <- summarize_bins(part, focal_feature, "centred_z_y", "Unadjusted",
                               zero_aware, knitting, bin_name)
  controls <- as.matrix(part[, paste0("centred_z_", setdiff(features, focal_feature)),
                             with = FALSE])
  y <- part$centred_z_y
  x <- part[[paste0("centred_z_", focal_feature)]]
  part[, adjusted_engagement := y - as.vector(controls %*% least_squares(controls, y))]
  part[, adjusted_feature := x - as.vector(controls %*% least_squares(controls, x))]
  adjusted <- summarize_bins(part, "adjusted_feature", "adjusted_engagement", "Adjusted",
                             FALSE, knitting, bin_name)
  full_x <- as.matrix(part[, paste0("centred_z_", features), with = FALSE])
  beta <- setNames(least_squares(full_x, y), features)
  coefficients <- data.table(community = community_id, n = nrow(part),
                              pairwise_correlation = cor(x, y))
  if (knitting) {
    coefficients[, `:=`(conditional_arousal_slope = beta[["arousal"]],
                         conditional_negativity_slope = beta[["negativity"]],
                         conditional_moralisation_slope = beta[["moralisation"]],
                         conditional_in_out_balance_slope = beta[["in_out_balance"]])]
  } else {
    coefficients[, conditional_negativity_slope := beta[["negativity"]]]
    if (zero_aware) {
      coefficients[, zero_negativity_proportion := mean(part$negativity == 0)]
    } else {
      coefficients[, nonzero_negativity_proportion := mean(part$negativity > 0)]
    }
  }
  list(summaries = rbindlist(list(unadjusted, adjusted)), coefficients = coefficients)
}

negative <- lapply(c("tifu", "AmItheAsshole"), make_diagnostic,
                   focal_feature = "negativity")
positive <- lapply(c("cscareerquestions", "medicine"), make_diagnostic,
                   focal_feature = "negativity", zero_aware = TRUE, bin_name = "group")
knitting_result <- make_diagnostic("knitting", "arousal", knitting = TRUE)
combine <- function(results, component) rbindlist(lapply(results, `[[`, component))
outputs <- list(
  reddit_negativity_decile_diagnostics.csv = combine(negative, "summaries"),
  reddit_negativity_diagnostic_coefficients.csv = combine(negative, "coefficients"),
  reddit_positive_negativity_group_diagnostics.csv = combine(positive, "summaries"),
  reddit_positive_negativity_coefficients.csv = combine(positive, "coefficients"),
  reddit_knitting_arousal_deciles.csv = knitting_result$summaries,
  reddit_knitting_arousal_coefficients.csv = knitting_result$coefficients
)
dir.create(diagnostic_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)
for (name in names(outputs)) fwrite(outputs[[name]], file.path(diagnostic_directory, name))
message("Recomputed six diagnostic CSVs from ", nrow(data), " complete Reddit observations.")

# Use the same combined layout as prepare_supplementary_materials.R.
negative_deciles <- fread(file.path(
  diagnostic_directory,
  "reddit_negativity_decile_diagnostics.csv"
))
aita <- negative_deciles[
  community == "AmItheAsshole",
  .(
    case = "A. r/AmItheAsshole negativity",
    relationship,
    bin = decile,
    mean_y,
    ci_low,
    ci_high
  )
]

positive_groups <- fread(file.path(
  diagnostic_directory,
  "reddit_positive_negativity_group_diagnostics.csv"
))
cscareer <- positive_groups[
  community == "cscareerquestions",
  .(
    case = "B. r/cscareerquestions negativity",
    relationship,
    bin = group,
    mean_y,
    ci_low,
    ci_high
  )
]

knitting_deciles <- fread(file.path(
  diagnostic_directory,
  "reddit_knitting_arousal_deciles.csv"
))
knitting <- knitting_deciles[
  ,
  .(
    case = "C. r/knitting arousal",
    relationship,
    bin = decile,
    mean_y,
    ci_low,
    ci_high
  )
]

figure_data <- rbindlist(list(aita, cscareer, knitting))
figure_data[, case := factor(
  case,
  levels = c(
    "A. r/AmItheAsshole negativity",
    "B. r/cscareerquestions negativity",
    "C. r/knitting arousal"
  )
)]
figure_data[, relationship := factor(
  relationship,
  levels = c("Unadjusted", "Adjusted"),
  labels = c("Unadjusted association", "Adjusted for other features")
)]

figure_s5 <- ggplot(
  figure_data,
  aes(x = bin, y = mean_y, colour = relationship, fill = relationship)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.45) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_high),
    alpha = 0.16,
    colour = NA
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_grid(rows = vars(case), cols = vars(relationship)) +
  scale_colour_manual(values = c("#3D86A6", "#D87A3A")) +
  scale_fill_manual(values = c("#3D86A6", "#D87A3A")) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Exploratory Reddit community diagnostics",
    x = "Feature bin",
    y = "Mean standardized engagement or residual"
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text.y = element_text(face = "bold", angle = 0),
    strip.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(
  file.path(figure_directory, "figure_s5_reddit_community_diagnostics.pdf"),
  figure_s5,
  width = 8.3,
  height = 9.2,
  units = "in"
)
ggsave(
  file.path(figure_directory, "figure_s5_reddit_community_diagnostics.png"),
  figure_s5,
  width = 8.3,
  height = 9.2,
  units = "in",
  dpi = 300,
  bg = "white"
)

message("Reddit diagnostics and Figure S5 written to: ", output_directory)
