Prepare Data
================

- [Required Packages &
  Reproducibility](#required-packages--reproducibility)
- [Tidy Data](#tidy-data)
- [Save Data for Analysis](#save-data-for-analysis)
- [Visualization of Data](#visualization-of-data)
  - [Dependent Variable per Gender](#dependent-variable-per-gender)
  - [Correlations Matrix](#correlations-matrix)

## Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
#renv::snapshot()
```

## Tidy Data

This code chuck downloads the data from Qualtrics using the API and
cleans the raw data.

``` r
d <- fetch_survey(surveyID = "SV_0DMqk8UcwVLGrNH", 
                    verbose = TRUE, force_request = T,
                    label = FALSE, convert = FALSE)  %>%
  filter(consent == 0)
source(here("src/data-processing/clean_data.R"))
```

## Save Data for Analysis

``` r
source(here("src/data-processing/data_for_analysis.R"))
save(d, file = here("data/intermediate/clean_data.RData"))
```

## Visualization of Data

### Dependent Variable per Gender

<img src="../../report/figures/Dependent Variable-1.png" style="display: block; margin: auto;" />

### Correlations Matrix

<img src="../../report/figures/Correlations Matrix-1.png" style="display: block; margin: auto;" />
