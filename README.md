# Project ReadMe

## Overview

This repository contains the analysis and scripts for the multimorbidity clustering and demographic characteristics project (Paper 1). The primary goal is to analyze chronic disease patterns across different age and sex strata in rural South African population data, generate summary tables (Table 1), and produce informative visualizations (e.g., line plots, upset plots) for academic publication.

## Data

* **VARAW**: Main analysis dataset containing demographic (Age, Sex, Marital), behavioral (Alcohol, Smoking, Drugs), and clinical indicators (13 chronic conditions, e.g. Hypertension, Diabetes, HIV, COPD, etc.)
* **PAT1, PAT2, PAT3**: Subsets for different periods (2012–2015, 2016–2019, 2020–2022) used for Upset plots.
* **merged\_result**: Pre-computed ranking results for disease combinations used in line graph "Changing Ranks Over Time".

## Methods

1. **Data Preparation**

   * Imported and cleaned raw datasets.
   * Converted binary condition flags and demographic fields to appropriate types.
   * Defined age groups and combined Age × Sex strata.
2. **Table 1 Generation**

   * Used `tableone` package to compute baseline characteristics: mean ± SD for continuous variables and N (%) for categorical.
   * Stratified by Age–Sex groups.
   * Exported results to HTML/Markdown and Excel for manuscript.
3. **Visualizations**

   * **Line Graph**: Tracked disease-combination ranks over three periods, with labels and styling via `ggplot2`.
   * **Upset Plots**: Illustrated intersection sizes of chronic conditions per period using `ComplexUpset` and `patchwork`, applying a global bold/text-size theme.
4. **Statistical Testing**

   * Enabled omnibus p-values (ANOVA for continuous, χ²/Fisher’s exact for categorical) via `CreateTableOne(test=TRUE)`.

## Code Structure

```
/ R/
  ├── data_prep.R       # Data import, cleaning, and type conversions
  ├── table1_generation.R  # Scripts to create Table 1 outputs (HTML, Markdown, Excel)
  ├── plots/
  │     ├── line_graph.R  # Line plot of disease ranks
  │     └── upset_plots.R # Upset plots for chronic condition intersections
  └── utils.R            # Helper functions (theme settings, export routines)
```

## Dependencies

* R (>= 4.1)
* data.table
* dplyr, tidyr
* tableone
* ggplot2, ComplexUpset, patchwork
* knitr, kableExtra
* writexl (for Excel exports)

Install all via:

```r
install.packages(c(
  "data.table", "dplyr", "tidyr", "tableone",
  "ggplot2", "ComplexUpset", "patchwork",
  "knitr", "kableExtra", "writexl"
))
```

## Usage

1. Clone the repository and open in RStudio.
2. Run `data_prep.R` to prepare datasets.
3. Run `table1_generation.R` to create Table 1 and export to HTML/Excel.
4. Run scripts in `plots/` to generate and save figures.
5. Review outputs in `output/` folder for manuscript integration.

## Outputs

* **output/Table1.html**, **output/Table1.xlsx**: Baseline characteristics tables.
* **output/Changing\_Ranks\_Over\_Time.png**: Line graph of disease ranks.
* **output/Upset\_Plots.png**: Combined Upset plots for three periods.

## Contact

For questions or issues, please reach out to Cyril Chironda ([cyril.chironda@wits.ac.za](mailto:cyril.chironda@wits.ac.za)).
