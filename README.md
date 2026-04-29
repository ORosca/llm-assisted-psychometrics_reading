# llm-assisted-psychometrics_reading
An end-to-end R pipeline for the DAACS reading assessment. Features reproducible data standardization, LLM-assisted item harmonization, and advanced IRT modeling (Rasch, 2PL, Bifactor) to evaluate internal structure and support score validity.
# DAACS Reading Assessment: LLM-assisted Operational Psychometric Pipeline

This repository contains an end-to-end, reproducible R workflow for the data processing, item harmonization, and psychometric modeling of the DAACS reading assessment. 

The pipeline is designed to transform raw, multi-wave institutional data into analysis-ready formats, conduct rigorous item quality checks, and evaluate competing Item Response Theory (IRT) models. Ultimately, these steps are foundational for establishing evidence regarding the validity of the assessment scale scores across diverse student populations.

## Key Highlights

* **Reproducible Data Engineering:** Automated ingestion, standardization, and deduplication of complex, multi-wave educational datasets.
* **LLM-Assisted Harmonization:** An auditable, LLM-supported fuzzy-matching workflow for mapping item text to global identifiers, demonstrating the integration of AI tools into operational psychometric workflows.
* **Advanced Psychometrics:** Full-bank and trimmed-bank IRT model comparisons (Rasch, Unidimensional 2PL, and 2PL Bifactor) using the `mirt` package.
* **Quality Assurance:** Built-in missingness diagnostics, response-time filters, and item-level sample-size reporting.

## Repository Structure

```text
├── README.md
├── R/
│   └── utils_read_pipeline.R                 # Shared helper functions (QA, missingness, recoding)
├── src/
│   ├── 01_data_preprocessing/
│   │   ├── 01a_clean_2022_cohort.R           # Standardizes legacy UMGC1 + UA2 wide datasets
│   │   └── 01b_combine_and_finalize.R        # Stacks cohorts, resolves duplicates, and outputs final analytic sample
│   ├── 02_item_harmonization/
│   │   └── 02a_llm_harmonization_audit.R     # Generates LLM candidate pools and applies human-reviewed patches
│   ├── 03_psychometric_modeling/
│   │   ├── 03a_irt_model_comparison.R        # Evaluates Rasch vs 2PL vs Bifactor models on the full bank
│   │   ├── 03b_item_diagnostics_trimming.R   # Flags poor-performing items and generates a trimmed item pool
│   │   └── 03c_trimmed_model_comparison.R    # Refits and compares models on the trimmed item bank
     └── 04_reporting/
       └── 04a_item_selection_summaries.R    # Generates cross-tabulated sample size thresholds for DIF/IRT
```
## Workflow Overview
* **Preprocessing** (01_data_preprocessing/): Raw data is ingested, standard demographics are recoded, and student response times are filtered to remove non-valid attempts. The datasets from multiple academic years are stacked, deduplicated via heuristic patching, and finalized into a single analytic file.
* **Item Harmonization** (02_item_harmonization/): Unmatched reading items undergo an automated, LLM-assisted audit. The pipeline ranks candidate item matches using string distance metrics (Jaro-Winkler/Levenshtein) and integrates human-reviewed corrections to ensure item identity consistency.
* **Psychometric Modeling** (03_psychometric_modeling/): Using the mirt package, the pipeline fits multiple IRT models to evaluate the internal structure of the assessment. It outputs comprehensive model fit statistics (AIC, BIC, SABIC, LRT), item parameter distributions, and theta score comparisons to support the validity of the assessment scale scores.
* **Reporting** (04_reporting/): Produces traditional descriptive statistics and evaluates domain-level item counts against sample-size thresholds required for subsequent analyses (e.g., multivariable DIF).
## Requirements
* **R** (>= 4.1.0)
* **Core Packages**: dplyr, tidyr, readr, readxl, stringr, purrr, tibble, ggplot2
* **Psychometrics**: mirt
* **Text Processing**: stringdist
## Author
* **Oxana Rosca**, PhD Educational Psychology | Psychometrics
