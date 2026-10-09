# Codes and data to reproduce "Associations between content and engagement vary across social media communities"

This repository contains the data and R scripts for reproducing the analyses in the manuscript. 

## Repository contents

- `data/`: five item-level analysis datasets for the five corpora.
- `analysis/`: scripts for the five-corpus multilevel models and the main and detailed model figures. Reproduce the material in the main manuscript.
- `supplementary_materials/scripts/`: scripts for the Reddit diagnostics and supplementary datasets and tables. Reproduce the material in Supplementary Information


Each data file has these columns:

- `item_id`: a study-specific surrogate key. Rows that shared a source item ID within the same community receive the same key, preserving the original analysis's duplicate-removal rule. The original source IDs cannot be recovered from this key.
- `community`: the community label used to group observations in the models.
- `engagement`: the engagement measure used in the analysis.
- `negativity`, `moralisation`, `arousal`, and `in_out_balance`: the scored content features used in the analysis.

## Requirements

Use R and install these packages before running the scripts:

```r   
install.packages(c("data.table", "lme4", "ggplot2", "patchwork"))
```

## Reproduction steps

1. Fit the models for all five corpora. This writes model summaries and analysis tables under `analysis/five_corpus_multilevel_results/`.

   ```sh
   Rscript analysis/five_corpus_multilevel_analysis.R
   ```

2. Generate the main cross-corpus figure and the detailed corpus figures from those model outputs.

   ```sh
   Rscript analysis/plot_cross_corpus_main_figure.R
   Rscript analysis/plot_detailed_corpus_figures.R
   ```

3. Recompute the Reddit diagnostic tables and Figure S5 directly from the scored Reddit data.

   ```sh
   Rscript supplementary_materials/scripts/generate_reddit_diagnostics.R
   ```

4. Create Datasets S1-S2 and the supplementary tables using the model outputs and Reddit diagnostic tables.

   ```sh
   Rscript supplementary_materials/scripts/prepare_supplementary_materials.R
   ```

The supplementary script writes `dataset_s1_community_partially_pooled_slopes.csv` and `dataset_s2_fixed_effects.csv` directly into `supplementary_materials/`, along with generated tables and figures in its `tables/` and `figures/` subfolders. Analysis outputs go into `analysis/five_corpus_multilevel_results/`. 

These generated results and figures are not included in the repository.

## Data provenance

The analysis data are derived from Reddit, X, YouTube, and Stack Exchange material. Removing text and replacing item IDs reduces direct identification and makes the files sufficient for reproducing the statistical models. The files here do not reproduce the original text-scoring step; they start from the already-scored features.
