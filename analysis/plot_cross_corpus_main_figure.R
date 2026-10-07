# Create Figure 2: condensed cross-corpus multilevel results
#
# Required input files in the results directory:
#   all_corpora_fixed_effects.csv
#   all_corpora_magnitude_concealed_by_signed_averaging.csv
#
# Usage:
#   Rscript plot_cross_corpus_main_figure.R \
#     five_corpus_multilevel_results \
#     five_corpus_multilevel_results/figure_2_cross_corpus
#
# Defaults:
#   results: five_corpus_multilevel_results
#   output:  five_corpus_multilevel_results/figure_2_cross_corpus

library(data.table)
library(ggplot2)
library(patchwork)

arguments <- commandArgs(trailingOnly = TRUE)
script_argument <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run this script with Rscript.", call. = FALSE)
}
script_directory <- dirname(normalizePath(sub("^--file=", "", script_argument)))
project_directory <- normalizePath(file.path(script_directory, ".."))
results_directory <- if (length(arguments) >= 1L) {
  arguments[[1L]]
} else {
  file.path(project_directory, "analysis", "five_corpus_multilevel_results")
}
output_prefix <- if (length(arguments) >= 2L) {
  arguments[[2L]]
} else {
  file.path(results_directory, "figure_2_cross_corpus")
}

fixed_effects_path <- file.path(
  results_directory,
  "all_corpora_fixed_effects.csv"
)
magnitude_path <- file.path(
  results_directory,
  "all_corpora_magnitude_concealed_by_signed_averaging.csv"
)

required_paths <- c(fixed_effects_path, magnitude_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0L) {
  stop(
    "Missing required result file(s): ",
    paste(missing_paths, collapse = ", "),
    call. = FALSE
  )
}

fixed_effects <- fread(fixed_effects_path)
magnitude <- fread(magnitude_path)

feature_information <- data.table(
  term = c(
    "within_z_negativity",
    "within_z_moralisation",
    "within_z_arousal",
    "within_z_in_out_balance"
  ),
  feature_key = c(
    "negativity",
    "moralisation",
    "arousal",
    "in_out_balance"
  ),
  feature_label = c(
    "Negativity",
    "Moralization",
    "Arousal",
    "In/out balance"
  ),
  feature_order = 1:4
)

corpus_information <- data.table(
  corpus = c(
    "reddit",
    "x",
    "stackexchange",
    "youtube_title_desc",
    "youtube_comments"
  ),
  corpus_label = c(
    "Reddit",
    "X",
    "Stack Exchange",
    "YouTube titles",
    "YouTube comments"
  ),
  corpus_order = 1:5
)

check_columns <- function(data, required, table_name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      table_name,
      " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

check_columns(
  fixed_effects,
  c("corpus", "model", "term", "estimate"),
  "all_corpora_fixed_effects.csv"
)
check_columns(
  magnitude,
  c(
    "corpus",
    "feature",
    "average_slope",
    "random_slope_sd",
    "expected_absolute_slope"
  ),
  "all_corpora_magnitude_concealed_by_signed_averaging.csv"
)

# -----------------------------------------------------------------------------
# Construct one row for every corpus-feature combination
# -----------------------------------------------------------------------------

summary_data <- fixed_effects[
  model == "common_slopes" & term %in% feature_information$term,
  .(
    corpus,
    term,
    common_estimate = estimate
  )
]
summary_data <- merge(
  summary_data,
  feature_information,
  by = "term",
  all.x = TRUE,
  sort = FALSE
)

magnitude[, feature_key := feature]
summary_data <- merge(
  summary_data,
  magnitude[
    ,
    .(
      corpus,
      feature_key,
      varying_average_slope = average_slope,
      random_slope_sd,
      expected_absolute_slope
    )
  ],
  by = c("corpus", "feature_key"),
  all.x = TRUE,
  sort = FALSE
)
summary_data <- merge(
  summary_data,
  corpus_information,
  by = "corpus",
  all.x = TRUE,
  sort = FALSE
)

expected_rows <- nrow(corpus_information) * nrow(feature_information)
if (nrow(summary_data) != expected_rows ||
    any(!is.finite(summary_data$common_estimate)) ||
    any(!is.finite(summary_data$varying_average_slope)) ||
    any(
      is.finite(summary_data$random_slope_sd) !=
        is.finite(summary_data$expected_absolute_slope)
    )) {
  stop(
    "Could not construct all 20 corpus-feature summaries.",
    call. = FALSE
  )
}

summary_data[, `:=`(
  random_slope_estimated = is.finite(random_slope_sd),
  slope_sd_low = varying_average_slope - random_slope_sd,
  slope_sd_high = varying_average_slope + random_slope_sd,
  common_absolute_slope = abs(common_estimate)
)]

feature_levels <- rev(
  feature_information[order(feature_order), feature_label]
)
corpus_levels <- corpus_information[order(corpus_order), corpus_label]

summary_data[, feature_label := factor(
  feature_label,
  levels = feature_levels
)]
summary_data[, corpus_label := factor(
  corpus_label,
  levels = corpus_levels
)]

# -----------------------------------------------------------------------------
# Panel A: common slopes and between-community slope heterogeneity
#
# The orange coefficient comes from the common-slope model. The blue band is
# the varying-model mean plus or minus one estimated random-slope standard
# deviation. The blue circle marks the varying-model mean. The band describes
# between-community heterogeneity.
# -----------------------------------------------------------------------------

panel_a <- ggplot(summary_data, aes(y = feature_label)) +
  geom_vline(
    xintercept = 0,
    colour = "grey55",
    linewidth = 0.45
  ) +
  geom_segment(
    data = summary_data[random_slope_estimated == TRUE],
    aes(
      x = slope_sd_low,
      xend = slope_sd_high,
      yend = feature_label
    ),
    colour = "#2878B5",
    linewidth = 7,
    alpha = 0.26,
    lineend = "butt"
  ) +
  geom_point(
    aes(x = varying_average_slope),
    shape = 21,
    size = 3,
    stroke = 0.5,
    colour = "#165A8A",
    fill = "#2878B5"
  ) +
  geom_point(
    aes(x = common_estimate),
    shape = 23,
    size = 3.4,
    stroke = 0.65,
    colour = "#8C3D1F",
    fill = "#D26A3A"
  ) +
  facet_grid(
    rows = vars(corpus_label),
    scales = "free_y",
    space = "free_y",
    drop = FALSE
  ) +
  scale_x_continuous(
    name = "Signed standardized slope",
    expand = expansion(mult = c(0.06, 0.06))
  ) +
  scale_y_discrete(name = NULL, drop = FALSE) +
  labs(
    title = "A. Common slopes and\ncommunity heterogeneity"
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey88",
      linewidth = 0.35
    ),
    axis.text.y = element_text(
      colour = "grey15",
      margin = margin(r = 5)
    ),
    axis.text.x = element_text(colour = "grey25"),
    axis.title.x = element_text(margin = margin(t = 8)),
    strip.text.y = element_text(
      face = "bold",
      size = 8.5
    ),
    strip.background.y = element_rect(
      fill = "grey94",
      colour = NA
    ),
    plot.title = element_text(face = "bold", size = 11.5),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 6)
  )

# -----------------------------------------------------------------------------
# Panel B: common-slope magnitude versus expected community-level magnitude
# -----------------------------------------------------------------------------

panel_b <- ggplot(summary_data, aes(y = feature_label)) +
  geom_segment(
    data = summary_data[random_slope_estimated == TRUE],
    aes(
      x = common_absolute_slope,
      xend = expected_absolute_slope,
      yend = feature_label
    ),
    colour = "grey65",
    linewidth = 1.2
  ) +
  geom_point(
    aes(x = common_absolute_slope),
    shape = 23,
    size = 3.4,
    stroke = 0.65,
    colour = "#8C3D1F",
    fill = "white"
  ) +
  geom_point(
    data = summary_data[random_slope_estimated == TRUE],
    aes(x = expected_absolute_slope),
    shape = 21,
    size = 3,
    stroke = 0.5,
    colour = "#165A8A",
    fill = "white"
  ) +
  facet_grid(
    rows = vars(corpus_label),
    scales = "free_y",
    space = "free_y",
    drop = FALSE
  ) +
  scale_x_continuous(
    name = "Absolute standardized slope",
    limits = c(0, NA),
    expand = expansion(mult = c(0.015, 0.08))
  ) +
  scale_y_discrete(name = NULL, drop = FALSE) +
  labs(
    title = "B. Common versus\ncommunity-level magnitude"
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey88",
      linewidth = 0.35
    ),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "grey25"),
    axis.title.x = element_text(margin = margin(t = 8)),
    strip.text.y = element_blank(),
    strip.background.y = element_blank(),
    plot.title = element_text(face = "bold", size = 11.5),
    plot.margin = margin(t = 8, r = 6, b = 8, l = 0)
  )

figure <- panel_a | panel_b
figure <- figure +
  plot_layout(widths = c(1.16, 1))

figure_output_directory <- dirname(output_prefix)
dir.create(
  figure_output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = paste0(output_prefix, ".pdf"),
  plot = figure,
  width = 8,
  height = 8.6,
  units = "in"
)
ggsave(
  filename = paste0(output_prefix, ".png"),
  plot = figure,
  width = 8,
  height = 8.6,
  units = "in",
  dpi = 300,
  bg = "white"
)

message(
  "Figure 2 written to: ",
  paste0(output_prefix, ".pdf"),
  " and ",
  paste0(output_prefix, ".png")
)
