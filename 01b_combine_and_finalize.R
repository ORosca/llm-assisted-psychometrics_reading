# 01b_combine_and_finalize.R
# ============================================================
# Purpose:
# 1. Stack 2022 and 2022-23 reading wide files and harmonize columns.
# 2. Run initial missingness diagnostics on the raw stacked data.
# 3. Detect duplicate students and duplicate score patterns.
# 4. Prepare auditable artifacts for patch-then-remove deduplication (UMGC/UMGC1).
# 5. Apply duplicate removals and patch UMGC rows.
# 6. Run final missingness diagnostics, create descriptive summaries, 
#    item-level sample-size tables, and save the final cleaned dataset.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(ggplot2)
})

# ------------------------------------------------------------
# Section 1. Paths and Configuration
# ------------------------------------------------------------

project_dir <- "C:/Users/orosc/OneDrive/Read"
source(file.path(project_dir, "utils_read_pipeline.R"))

file_2022 <- file.path(
  project_dir,
  "read_v2_umgc1ua2-anSamp2-2022_qa_outputs",
  "final_clean_datasets",
  "read_umgc1ua2_anSamp2_wide.rds"
)

file_2022_23 <- file.path(
  project_dir,
  "read_v2_ua23umgc-2022-23_qa_outputs",
  "read_ua23umgc_wide.rds"
)

output_dir <- file.path(project_dir, "read_v2_umgc_ua_22_23_combined_outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(file_2022))
stopifnot(file.exists(file_2022_23))

# ------------------------------------------------------------
# Section 2. Load, Harmonize, and Stack Datasets
# ------------------------------------------------------------

wide_2022 <- readRDS(file_2022)
wide_2022_23 <- readRDS(file_2022_23)

if (!"wave" %in% names(wide_2022)) {
  wide_2022 <- wide_2022 %>% mutate(wave = 2022L)
}

wide_2022 <- wide_2022 %>%
  mutate(
    DAACS_ID = as.character(DAACS_ID),
    global_id = as.character(global_id),
    college = as.character(college),
    wave = as.integer(wave),
    source_file = "umgc1ua2_anSamp2_2022"
  )

wide_2022_23 <- wide_2022_23 %>%
  mutate(
    DAACS_ID = as.character(DAACS_ID),
    global_id = as.character(global_id),
    college = as.character(college),
    wave = as.integer(wave),
    source_file = "ua23umgc_2022_23"
  )

assert_has_cols(wide_2022, c("DAACS_ID", "global_id", "college", "wave"), "wide_2022")
assert_has_cols(wide_2022_23, c("DAACS_ID", "global_id", "college", "wave"), "wide_2022_23")

wide_2022 <- wide_2022 %>% coerce_factors_to_char() %>% coerce_item_cols()
wide_2022_23 <- wide_2022_23 %>% coerce_factors_to_char() %>% coerce_item_cols()

item_cols_all <- sort(unique(c(get_item_cols(wide_2022), get_item_cols(wide_2022_23))))
item_cols_all <- item_cols_all[order(substr(item_cols_all, 1, 4), item_cols_all)]

non_item_cols_all <- union(
  names(wide_2022)[!names(wide_2022) %in% get_item_cols(wide_2022)],
  names(wide_2022_23)[!names(wide_2022_23) %in% get_item_cols(wide_2022_23)]
)

master_cols <- c(non_item_cols_all, item_cols_all)

wide_2022 <- align_to_master_cols(wide_2022, master_cols)
wide_2022_23 <- align_to_master_cols(wide_2022_23, master_cols)

read_v2_umgc_ua_2waves4sets_raw <- bind_rows(wide_2022, wide_2022_23)

# Initial Missingness Diagnostics
miss_raw <- run_missingness_diagnostics(
  df = read_v2_umgc_ua_2waves4sets_raw,
  stage_name = "raw_stacked",
  save_plots = TRUE,
  output_dir = output_dir
)

# ------------------------------------------------------------
# Section 3. Duplicate Diagnostics
# ------------------------------------------------------------

dat_check <- read_v2_umgc_ua_2waves4sets_raw

dup_global_id <- dat_check %>%
  count(global_id, sort = TRUE) %>%
  filter(n > 1)

dup_global_id_rows <- dat_check %>%
  semi_join(dup_global_id, by = "global_id") %>%
  arrange(global_id, DAACS_ID, college, wave)

dup_full_row_flag <- duplicated(dat_check) | duplicated(dat_check, fromLast = TRUE)
dup_full_rows <- dat_check[dup_full_row_flag, , drop = FALSE] %>%
  arrange(DAACS_ID, college, wave)

id_like_cols <- intersect(c("DAACS_ID", "global_id", "college", "wave"), names(dat_check))
substantive_cols <- setdiff(names(dat_check), id_like_cols)

dup_substantive_rows <- dat_check %>%
  mutate(.row_index = row_number()) %>%
  group_by(across(all_of(substantive_cols))) %>%
  mutate(substantive_dup_n = n()) %>%
  ungroup() %>%
  filter(substantive_dup_n > 1) %>%
  arrange(DAACS_ID, college, wave)

score_cols <- names(dat_check)[grepl("^Q\\d{3}", names(dat_check))]

dup_score_only_rows <- dat_check %>%
  mutate(.row_index = row_number()) %>%
  group_by(across(all_of(score_cols))) %>%
  mutate(score_only_dup_n = n()) %>%
  ungroup() %>%
  filter(score_only_dup_n > 1) %>%
  arrange(DAACS_ID, college, wave)

score_cols_dup <- names(dup_score_only_rows)[grepl("^Q\\d{3}", names(dup_score_only_rows))]

dup_score_only_rows <- dup_score_only_rows %>%
  mutate(
    score_pattern = apply(select(., all_of(score_cols_dup)), 1, function(x) {
      paste(ifelse(is.na(x), "NA", x), collapse = "|")
    })
  ) %>%
  group_by(score_pattern) %>%
  mutate(
    dup_group = cur_group_id(),
    n_in_group = n()
  ) %>%
  ungroup() %>%
  arrange(dup_group, DAACS_ID, global_id)

dup_daacs_within_college <- dat_check %>%
  count(college, DAACS_ID, sort = TRUE) %>%
  filter(n > 1)

dup_daacs_within_college_rows <- dat_check %>%
  semi_join(dup_daacs_within_college, by = c("college", "DAACS_ID")) %>%
  arrange(college, DAACS_ID, wave)

# ------------------------------------------------------------
# Section 4. Demographic Alignment & Patch Preparation (UMGC/UMGC1)
# ------------------------------------------------------------

demo_check_vars <- c("wave", "age", "gender", "ethnicity")
assert_has_cols(
  dup_score_only_rows,
  c("dup_group", "DAACS_ID", "global_id", demo_check_vars),
  "dup_score_only_rows"
)

dup_score_demo_summary <- dup_score_only_rows %>%
  group_by(dup_group) %>%
  summarise(
    n_rows = n(),
    n_global_id = n_distinct(global_id),
    n_DAACS_ID = n_distinct(DAACS_ID),
    n_wave = n_distinct(wave),
    n_age = n_distinct(age),
    n_gender = n_distinct(gender),
    n_ethnicity = n_distinct(ethnicity),
    same_demo_profile = n_wave == 1 & n_age == 1 & n_gender == 1 & n_ethnicity == 1,
    same_global_id = n_global_id == 1,
    same_DAACS_ID = n_DAACS_ID == 1,
    likely_same_person = same_demo_profile & same_global_id & same_DAACS_ID,
    .groups = "drop"
  ) %>%
  arrange(desc(!likely_same_person), dup_group)

groups_same_demo_pairs <- dup_score_only_rows %>%
  semi_join(
    dup_score_demo_summary %>% filter(same_demo_profile),
    by = "dup_group"
  ) %>%
  count(dup_group, college, name = "n_by_college") %>%
  tidyr::pivot_wider(
    names_from = college,
    values_from = n_by_college,
    values_fill = 0
  ) %>%
  filter(umgc == 1, umgc1 == 1) %>%
  select(dup_group)

cols_not_to_patch <- c(
  "DAACS_ID", "global_id", "college", "source_file",
  "dup_group", "n_in_group", "score_pattern", ".row_index"
)

patch_cols <- setdiff(names(dup_score_only_rows), cols_not_to_patch)

umgc_rows_patched <- dup_score_only_rows %>%
  semi_join(groups_same_demo_pairs, by = "dup_group") %>%
  arrange(dup_group, college) %>%
  group_by(dup_group) %>%
  group_modify(~{
    umgc_row  <- .x %>% filter(college == "umgc")
    umgc1_row <- .x %>% filter(college == "umgc1")
    
    stopifnot(nrow(umgc_row) == 1, nrow(umgc1_row) == 1)
    
    patched <- umgc_row
    
    for (nm in patch_cols) {
      patched[[nm]] <- dplyr::coalesce(umgc_row[[nm]], umgc1_row[[nm]])
    }
    patched
  }) %>%
  ungroup()

rows_to_remove <- dup_score_only_rows %>%
  semi_join(groups_same_demo_pairs, by = "dup_group") %>%
  filter(college == "umgc1") %>%
  distinct(global_id)

patch_audit <- umgc_rows_patched %>%
  select(global_id, all_of(patch_cols)) %>%
  rename_with(~ paste0(.x, "_patched"), all_of(patch_cols)) %>%
  left_join(
    dup_score_only_rows %>%
      semi_join(groups_same_demo_pairs, by = "dup_group") %>%
      filter(college == "umgc") %>%
      select(global_id, all_of(patch_cols)),
    by = "global_id"
  )

patch_audit$n_fields_filled <- rowSums(
  sapply(patch_cols, function(nm) {
    is.na(patch_audit[[nm]]) & !is.na(patch_audit[[paste0(nm, "_patched")]])
  })
)

patch_audit <- patch_audit %>% arrange(desc(n_fields_filled), global_id)

# ------------------------------------------------------------
# Section 5. Finalize Cleaned Dataset
# ------------------------------------------------------------

umgc_ids_to_replace <- umgc_rows_patched %>% distinct(global_id)

read_v2_umgc_ua_22_23 <- read_v2_umgc_ua_2waves4sets_raw %>%
  anti_join(umgc_ids_to_replace, by = "global_id") %>%
  anti_join(rows_to_remove, by = "global_id") %>%
  bind_rows(umgc_rows_patched) %>%
  distinct(global_id, .keep_all = TRUE)

stopifnot(sum(duplicated(read_v2_umgc_ua_22_23$global_id)) == 0)

n_input <- nrow(read_v2_umgc_ua_2waves4sets_raw)
n_removed_umgc1 <- nrow(rows_to_remove)
n_replaced_umgc <- nrow(umgc_ids_to_replace)
n_final <- nrow(read_v2_umgc_ua_22_23)

cat("\nFinalize step summary:\n")
cat("Input raw rows:             ", n_input, "\n")
cat("UMGC1 rows removed:         ", n_removed_umgc1, "\n")
cat("Original UMGC rows replaced:", n_replaced_umgc, "\n")
cat("Patched UMGC rows added:    ", nrow(umgc_rows_patched), "\n")
cat("Final rows:                 ", n_final, "\n")

miss_nodup <- run_missingness_diagnostics(
  df = read_v2_umgc_ua_22_23,
  stage_name = "after_removing_duplicates_and_patching",
  save_plots = TRUE,
  output_dir = output_dir
)

# ------------------------------------------------------------
# Section 6. Descriptive Statistics & Reporting
# ------------------------------------------------------------

read_v2_umgc_ua_22_23 <- read_v2_umgc_ua_22_23 %>%
  mutate(
    college   = as.factor(college),
    wave      = as.factor(wave),
    age_d24   = as.factor(age_d24),
    gender    = as.factor(gender),
    ethnicity = as.factor(ethnicity),
    military  = as.factor(military),
    pell      = as.factor(pell),
    age       = suppressWarnings(as.numeric(age)),
    transfer  = suppressWarnings(as.numeric(transfer)),
    readTime  = suppressWarnings(as.numeric(readTime))
  )

cat_vars <- c("college", "wave", "age_d24", "gender", "ethnicity", "military", "pell")

cat_descriptives <- lapply(cat_vars, function(v) {
  read_v2_umgc_ua_22_23 %>%
    count(.data[[v]], .drop = FALSE) %>%
    mutate(
      variable = v,
      percent = 100 * n / sum(n)
    ) %>%
    rename(category = 1) %>%
    select(variable, category, n, percent)
})

cat_descriptives_df <- bind_rows(cat_descriptives)

num_descriptives <- read_v2_umgc_ua_22_23 %>%
  summarise(
    age_n = sum(!is.na(age)),
    age_mean = mean(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    
    transfer_n = sum(!is.na(transfer)),
    transfer_mean = mean(transfer, na.rm = TRUE),
    transfer_median = median(transfer, na.rm = TRUE),
    transfer_sd = sd(transfer, na.rm = TRUE),
    
    readTime_n = sum(!is.na(readTime)),
    readTime_mean = mean(readTime, na.rm = TRUE),
    readTime_median = median(readTime, na.rm = TRUE),
    readTime_sd = sd(readTime, na.rm = TRUE)
  )

p_age_hist <- ggplot(read_v2_umgc_ua_22_23, aes(x = age)) +
  geom_histogram(bins = 30) + labs(title = "Age Distribution") + theme_minimal()

p_transfer_hist <- ggplot(read_v2_umgc_ua_22_23, aes(x = transfer)) +
  geom_histogram(bins = 30) + labs(title = "Transferred Credits Distribution") + theme_minimal()

sample_size_report <- make_item_sample_size_report(read_v2_umgc_ua_22_23)

qa_summary <- list(
  n_input_raw = n_input,
  duplicate_global_id_n = nrow(dup_global_id),
  duplicate_full_row_n = nrow(dup_full_rows),
  duplicate_score_only_row_n = nrow(dup_score_only_rows),
  n_removed_duplicate_umgc1_rows = n_removed_umgc1,
  n_replaced_umgc_rows = n_replaced_umgc,
  n_patched_umgc_rows_added = nrow(umgc_rows_patched),
  n_final = nrow(read_v2_umgc_ua_22_23),
  item_sample_size_overall_n = nrow(sample_size_report$overall_counts)
)

read_item_cols <- grep("^Q\\d+", names(read_v2_umgc_ua_22_23), value = TRUE)
read_item_counts <- sapply(read_v2_umgc_ua_22_23[read_item_cols], function(x) sum(!is.na(x)))
q <- quantile(read_item_counts, probs = c(0, 0.25, 0.50, 0.75, 1.00), na.rm = TRUE)

summary_line <- paste0(
  "Response counts per reading item: ",
  "Min = ", unname(q[1]), ", Q1 = ", unname(q[2]),
  ", Median = ", unname(q[3]), ", Q3 = ", unname(q[4]),
  ", Max = ", unname(q[5])
)
cat(summary_line, "\n")

# ------------------------------------------------------------
# Section 7. Save Outputs
# ------------------------------------------------------------

# Data files
save_both(read_v2_umgc_ua_2waves4sets_raw, output_dir, "read_v2_umgc_ua_2waves4sets_raw")
save_both(read_v2_umgc_ua_22_23, output_dir, "read_v2_umgc_ua_22_23")

# Duplicate tracking
save_both(dup_global_id_rows, output_dir, "dup_global_id_rows")
save_both(dup_score_demo_summary, output_dir, "dup_score_demo_summary")
save_both(patch_audit, output_dir, "patch_audit")

# Descriptives
write.csv(cat_descriptives_df, file.path(output_dir, "categorical_descriptives_read_v2_umgc_ua_22_23.csv"), row.names = FALSE)
write.csv(num_descriptives, file.path(output_dir, "numeric_descriptives_read_v2_umgc_ua_22_23.csv"), row.names = FALSE)

# Item Sample Sizes
write.csv(sample_size_report$overall_counts, file.path(output_dir, "item_sample_size_overall.csv"), row.names = FALSE)
write.csv(sample_size_report$by_demo_counts, file.path(output_dir, "item_sample_size_by_demo.csv"), row.names = FALSE)

# QA Summaries
write.csv(tibble(metric = names(qa_summary), value = unlist(qa_summary)), file.path(output_dir, "qa_summary.csv"), row.names = FALSE)
writeLines(summary_line, con = file.path(output_dir, "read_v2_umgc_ua_22_23_item_count_quartiles.txt"))

# Plots
ggsave(file.path(output_dir, "age_histogram.png"), p_age_hist, width = 7, height = 5, dpi = 300)
ggsave(file.path(output_dir, "transfer_histogram.png"), p_transfer_hist, width = 7, height = 5, dpi = 300)

cat("\nCombined stacking, duplicate diagnostics, patching, and finalization complete.\n")