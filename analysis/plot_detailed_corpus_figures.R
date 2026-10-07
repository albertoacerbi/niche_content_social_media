# Create Figure 1 and Figures S1-S4 from the five-corpus multilevel results
#
# Required input files in the results directory:
#   all_corpora_fixed_effects.csv
#   all_corpora_community_partially_pooled_slopes.csv
#   all_corpora_magnitude_concealed_by_signed_averaging.csv
#
# Usage:
#   Rscript plot_detailed_corpus_figures.R \
#     five_corpus_multilevel_results \
#     five_corpus_multilevel_results/detailed_figures
#
# Defaults:
#   results: five_corpus_multilevel_results
#   output:  five_corpus_multilevel_results/detailed_figures

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
output_directory <- if (length(arguments) >= 2L) {
  arguments[[2L]]
} else {
  file.path(results_directory, "detailed_figures")
}

fixed_effects_path <- file.path(
  results_directory,
  "all_corpora_fixed_effects.csv"
)
community_slopes_path <- file.path(
  results_directory,
  "all_corpora_community_partially_pooled_slopes.csv"
)
magnitude_path <- file.path(
  results_directory,
  "all_corpora_magnitude_concealed_by_signed_averaging.csv"
)

required_paths <- c(
  fixed_effects_path,
  community_slopes_path,
  magnitude_path
)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0L) {
  stop(
    "Missing required result file(s): ",
    paste(missing_paths, collapse = ", "),
    call. = FALSE
  )
}

fixed_effects <- fread(fixed_effects_path)
community_slopes <- fread(community_slopes_path)
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
  display_order = 1:4
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
    "YouTube titles/descriptions",
    "YouTube comments"
  ),
  output_stem = c(
    "figure_1_reddit",
    "figure_s1_x",
    "figure_s2_stackexchange",
    "figure_s3_youtube_titles_descriptions",
    "figure_s4_youtube_comments"
  )
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
  community_slopes,
  c("corpus", "community", feature_information$term),
  "all_corpora_community_partially_pooled_slopes.csv"
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

feature_levels <- rev(
  feature_information[order(display_order), feature_label]
)

make_corpus_figure <- function(corpus_id, corpus_label, output_stem) {
  fixed_part <- fixed_effects[
    corpus == corpus_id &
      model == "common_slopes" &
      term %in% feature_information$term,
    .(
      term,
      common_estimate = estimate
    )
  ]
  fixed_part <- merge(
    feature_information,
    fixed_part,
    by = "term",
    all.x = TRUE,
    sort = FALSE
  )

  magnitude_part <- copy(magnitude[corpus == corpus_id])
  magnitude_part[, feature_key := feature]
  magnitude_part <- merge(
    magnitude_part,
    feature_information,
    by = "feature_key",
    all.x = TRUE,
    sort = FALSE
  )

  comparison_data <- merge(
    fixed_part,
    magnitude_part[
      ,
      .(
        feature_key,
        varying_average_slope = average_slope,
        random_slope_sd,
        expected_absolute_slope
      )
    ],
    by = "feature_key",
    all.x = TRUE,
    sort = FALSE
  )

  if (nrow(comparison_data) != nrow(feature_information) ||
      any(!is.finite(comparison_data$common_estimate)) ||
      any(!is.finite(comparison_data$varying_average_slope)) ||
      any(
        is.finite(comparison_data$random_slope_sd) !=
          is.finite(comparison_data$expected_absolute_slope)
      )) {
    stop(
      "Incomplete common-slope or varying-slope results for corpus: ",
      corpus_id,
      call. = FALSE
    )
  }

  comparison_data[, `:=`(
    random_slope_estimated = is.finite(random_slope_sd),
    slope_sd_low = varying_average_slope - random_slope_sd,
    slope_sd_high = varying_average_slope + random_slope_sd,
    common_absolute_slope = abs(common_estimate),
    common_label = sprintf("%+.3f", common_estimate)
  )]

  community_part <- community_slopes[corpus == corpus_id]
  community_long <- melt(
    community_part,
    id.vars = c("corpus", "community"),
    measure.vars = feature_information$term,
    variable.name = "term",
    value.name = "slope",
    variable.factor = FALSE
  )
  community_long <- merge(
    community_long,
    comparison_data[, .(term, random_slope_estimated)],
    by = "term",
    all.x = TRUE,
    sort = FALSE
  )
  community_long <- merge(
    community_long,
    feature_information,
    by = "term",
    all.x = TRUE,
    sort = FALSE
  )

  comparison_data[, feature_label := factor(
    feature_label,
    levels = feature_levels
  )]
  community_long[, feature_label := factor(
    feature_label,
    levels = feature_levels
  )]
  all_horizontal_values <- c(
    community_long$slope,
    comparison_data$common_estimate,
    comparison_data$slope_sd_low,
    comparison_data$slope_sd_high,
    0
  )
  horizontal_range <- diff(range(all_horizontal_values, finite = TRUE))
  if (!is.finite(horizontal_range) || horizontal_range <= 0) {
    horizontal_range <- 1
  }
  comparison_data[, label_x := max(all_horizontal_values, na.rm = TRUE) +
    0.045 * horizontal_range]

  set.seed(20260928)
  panel_a <- ggplot() +
    geom_vline(
      xintercept = 0,
      colour = "grey55",
      linewidth = 0.45
    ) +
    geom_segment(
      data = comparison_data[random_slope_estimated == TRUE],
      aes(
        x = slope_sd_low,
        xend = slope_sd_high,
        y = feature_label,
        yend = feature_label
      ),
      colour = "#2878B5",
      linewidth = 7,
      alpha = 0.22,
      lineend = "butt"
    ) +
    geom_jitter(
      data = community_long[
        random_slope_estimated & is.finite(slope)
      ],
      aes(x = slope, y = feature_label),
      width = 0,
      height = 0.13,
      shape = 16,
      size = 1.65,
      alpha = 0.48,
      colour = "grey35"
    ) +
    geom_point(
      data = comparison_data,
      aes(x = varying_average_slope, y = feature_label),
      shape = 21,
      size = 3.2,
      stroke = 0.55,
      colour = "#165A8A",
      fill = "#2878B5"
    ) +
    geom_point(
      data = comparison_data,
      aes(x = common_estimate, y = feature_label),
      shape = 23,
      size = 3.7,
      stroke = 0.65,
      colour = "#8C3D1F",
      fill = "#D26A3A"
    ) +
    geom_text(
      data = comparison_data,
      aes(x = label_x, y = feature_label, label = common_label),
      hjust = 0,
      size = 3.4,
      colour = "#8C3D1F"
    ) +
    scale_x_continuous(
      name = "Within-community standardized slope",
      expand = expansion(mult = c(0.04, 0.13))
    ) +
    scale_y_discrete(name = NULL, drop = FALSE) +
    labs(
      title = "A. Community slopes versus the common slope" #,
 #     subtitle = paste(
#        "Grey dots: partially pooled community slopes.",
 #       "Orange diamonds: common slopes. Blue circles and bands: varying-slope means +/- 1 SD."
#      )
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(
        colour = "grey88",
        linewidth = 0.35
      ),
      axis.text.y = element_text(
        colour = "grey15",
        margin = margin(r = 7)
      ),
      axis.text.x = element_text(colour = "grey25"),
      axis.title.x = element_text(margin = margin(t = 8)),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(
        colour = "grey30",
        size = 9.5,
        margin = margin(b = 10)
      ),
      plot.margin = margin(t = 8, r = 28, b = 10, l = 10)
    )

  panel_b_title <- "B. Common versus community-level magnitude"

  panel_b <- ggplot(comparison_data, aes(y = feature_label)) +
    geom_segment(
      data = comparison_data[random_slope_estimated == TRUE],
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
      size = 3.7,
      stroke = 0.65,
      colour = "#8C3D1F",
      fill = "white"
    ) +
    geom_point(
      data = comparison_data[random_slope_estimated == TRUE],
      aes(x = expected_absolute_slope),
      shape = 21,
      size = 3.2,
      stroke = 0.55,
      colour = "#165A8A",
      fill = "white"
    ) +
    scale_x_continuous(
      name = "Absolute standardized slope",
      limits = c(0, NA),
      expand = expansion(mult = c(0.01, 0.08))
    ) +
    scale_y_discrete(name = NULL, drop = FALSE) +
    labs(
      title = panel_b_title #,
   #   subtitle = paste(
  #      "Orange diamonds: |common slope|.",
  #      "Blue circles: expected |community slope|."
 #     )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(
        colour = "grey88",
        linewidth = 0.35
      ),
      axis.text.y = element_text(
        colour = "grey15",
        margin = margin(r = 7)
      ),
      axis.text.x = element_text(colour = "grey25"),
      axis.title.x = element_text(margin = margin(t = 8)),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(
        colour = "grey30",
        size = 9.5,
        margin = margin(b = 10)
      ),
      plot.margin = margin(t = 8, r = 28, b = 10, l = 10)
    )

  figure <- panel_a / panel_b +
    plot_layout(heights = c(1.45, 1)) +
    plot_annotation(
      title = corpus_label,
      theme = theme(
        plot.title = element_text(face = "bold", size = 13)
      )
    )

  output_prefix <- file.path(output_directory, output_stem)
  ggsave(
    filename = paste0(output_prefix, ".pdf"),
    plot = figure,
    width = 7.5,
    height = 7.4,
    units = "in"
  )
  ggsave(
    filename = paste0(output_prefix, ".png"),
    plot = figure,
    width = 7.5,
    height = 7.4,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  message("Created detailed figure for: ", corpus_label)
  invisible(figure)
}

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

for (index in seq_len(nrow(corpus_information))) {
  make_corpus_figure(
    corpus_id = corpus_information$corpus[[index]],
    corpus_label = corpus_information$corpus_label[[index]],
    output_stem = corpus_information$output_stem[[index]]
  )
}

message("Detailed figures written to: ", output_directory)
