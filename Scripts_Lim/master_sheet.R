### Joining all datasets into a master sheet
### Created by: Wei Shen Lim
### Updated on: 2026-10-31
###############################################################################

## Load libraries
library(tidyverse)
library(janitor)
library(stringi)
library(here)
library(purrr)
library(openxlsx) 

### Standardize Hawaiian Diacritics and Names contextually ####################
clean_hawaiian_text <- function(text) {
  text_clean <- str_replace_all(text, "[\u02BB\u2018\u2019`']", "") 
  text_ascii <- stri_trans_general(text_clean, "Latin-ASCII")      
  
  text_final <- text_ascii %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%                        
    str_remove_all("^_+|_+$")                                     
  
  if_else(is.na(text), NA_character_, text_final)
}

### PARSER A: Processes Transposed Matrix Blocks (2004-2020 Layouts) ##########
parse_transposed_block <- function(block_matrix) {
  row_has_data <- rowSums(!is.na(block_matrix) & block_matrix != "") > 0
  col_has_data <- colSums(!is.na(block_matrix) & block_matrix != "") > 0
  block_matrix <- block_matrix[row_has_data, col_has_data, drop = FALSE]
  if (ncol(block_matrix) < 2 || nrow(block_matrix) == 0) return(NULL)
  
  meta_list <- list()
  meta_rows <- c("Date", "Start Time", "Stop Time", "Number of Transect Lines", 
                 "Length of Transect Lines", "Point Spacing", "Number of Quadrats per transect", 
                 "Location", "latitude", "longitude", "School")
  
  for (m in meta_rows) {
    row_idx <- which(block_matrix[, 1] == m)
    if (length(row_idx) > 0) meta_list[[clean_hawaiian_text(m)]] <- block_matrix[min(row_idx), 2]
    else meta_list[[clean_hawaiian_text(m)]] <- NA_character_
  }
  
  organism_row_idx <- which(block_matrix[, 1] == "Organisms" | block_matrix[, 1] == "Organism")
  if (length(organism_row_idx) == 0) return(NULL)
  
  start_data <- min(organism_row_idx)
  totals_idx <- which(str_detect(block_matrix[, 1], "(?i)^TOTALS"))
  end_data   <- if (length(totals_idx) > 0) min(totals_idx) - 1 else nrow(block_matrix)
  
  if (start_data >= end_data) return(NULL)
  raw_data_sub <- block_matrix[start_data:end_data, , drop = FALSE]
  
  sample_header_row <- as.character(raw_data_sub[1, ])
  dummy_names <- paste0("Col_", 1:ncol(raw_data_sub))
  colnames(raw_data_sub) <- dummy_names
  
  tidy_block <- raw_data_sub[-1, , drop = FALSE] %>%
    as_tibble() %>%
    rename(taxon_raw = Col_1) %>%
    pivot_longer(cols = -taxon_raw, names_to = "column_id", values_to = "count_raw") %>%
    mutate(
      col_num = as.numeric(str_extract(column_id, "\\d+")),
      sample_label = sample_header_row[col_num],
      transect_id = as.character(str_extract(str_extract(sample_label, "(?i)(transect|t)-?\\s*\\d+"), "\\d+")),
      position = as.character(str_extract(str_extract(sample_label, "(?i)point\\s*\\d+\\.?\\d*|at point\\s*\\d+\\.?\\d*"), "\\d+\\.?\\d*"))
    ) %>%
    filter(!is.na(count_raw), count_raw != "", !str_detect(count_raw, "(?i)^na$|^$|[^0-9]")) %>%
    mutate(
      date                            = as.character(meta_list$date),
      start_time                      = as.character(meta_list$start_time),
      stop_time                       = as.character(meta_list$stop_time),
      number_of_transect_lines        = as.character(meta_list$number_of_transect_lines),
      length_of_transect_lines        = as.character(meta_list$length_of_transect_lines),
      point_spacing                   = as.character(meta_list$point_spacing),
      number_of_quadrats_per_transect = as.character(meta_list$number_of_quadrats_per_transect),
      site_raw                        = as.character(meta_list$location),
      site_key                        = clean_hawaiian_text(meta_list$location),
      latitude                        = as.character(meta_list$latitude),
      longitude                       = as.character(meta_list$longitude),
      school                          = as.character(meta_list$school),
      data_entry_by                   = NA_character_, 
      taxon_raw                       = clean_hawaiian_text(taxon_raw),
      count                           = as.numeric(count_raw)
    ) %>%
    select(date, start_time, stop_time, number_of_transect_lines, length_of_transect_lines, 
           point_spacing, number_of_quadrats_per_transect, site_raw, site_key, latitude, 
           longitude, school, data_entry_by, transect_id, position, taxon_raw, count)
  
  return(tidy_block)
}

### Reads and slices wide side-by-side matrices cleanly #######################
process_transposed_file <- function(file_path) {
  raw_grid <- read_csv(file_path, col_names = FALSE, show_col_types = FALSE) %>% as.matrix()
  
  date_indices <- which(raw_grid[1, ] == "Date")
  if(length(date_indices) == 0) date_indices <- which(raw_grid[2, ] == "Date") 
  if(length(date_indices) == 0) date_indices <- which(raw_grid[3, ] == "Date")
  
  end_indices <- c(date_indices[-1] - 1, ncol(raw_grid))
  
  map2(date_indices, end_indices, ~ {
    sub_matrix <- raw_grid[, .x:.y, drop = FALSE]
    parse_transposed_block(sub_matrix)
  }) %>% bind_rows()
}

### PARSER B: Processes Wide Matrix Blocks (2024-2026 Layouts) ################
process_wide_year <- function(file_name) {
  
  # Read file skipping the broad category groupings (limu, invertebrates)
  raw_df <- read_csv(here("Data_Clean", file_name), skip = 1, show_col_types = FALSE) %>% 
    clean_names()
  
  current_names <- names(raw_df)
  
  # PRIORITY CONDITIONAL: If a quadrat column name variant exists, flag the redundant position column for deletion
  has_quadrat_col <- any(current_names %in% c("quadrat", "quadrat_location_m"))
  
  if (has_quadrat_col) {
    # Isolate and purge the empty "position_number_on_transect_m" tracker column
    raw_df <- raw_df %>% select(-any_of("position_number_on_transect_m"))
  }
  
  # Map variables with high-priority order mapping for dynamic quadrat allocations
  raw_df <- raw_df %>%
    rename(
      position    = any_of(c("quadrat", "quadrat_location_m", "position_number_on_transect_m", "position_percent_on_transect_m", "position_on_transect")),
      transect_id = any_of(c("x_t", "transect_number", "transect", "number_t")),
      coder_id    = any_of(c("data_entry_by", "data_entry"))
    )
  
  # Final fallback initialization safety check
  if (!"transect_id" %in% names(raw_df)) raw_df$transect_id <- NA_character_
  if (!"position"    %in% names(raw_df)) raw_df$position    <- NA_character_
  if (!"coder_id"    %in% names(raw_df)) raw_df$coder_id    <- NA_character_
  
  raw_df <- raw_df %>% mutate(across(any_of(c("transect_id", "position", "coder_id")), ~as.character(.)))
  
  # Filter out structural trackers from getting scrambled into taxon items
  metadata_garbage <- c("date", "location", "coder_id", "transect_id", "position", "total_points", "total", "totals", "unknown_bulbous_algae", "within_2m_proximity_of_attached_coral_colony_y_n_mamala_bay_only", "sediment_depth", "notes_corrections_to_data_sheet")
  species_cols <- setdiff(names(raw_df), metadata_garbage)
  species_cols <- species_cols[!str_detect(species_cols, "^x\\d+")] 
  
  long_df <- raw_df %>%
    pivot_longer(cols = any_of(species_cols), names_to = "taxon_raw", values_to = "count_raw") %>%
    filter(!is.na(count_raw), count_raw != "", !str_detect(count_raw, "(?i)^na$|^$|[^0-9]")) %>%
    mutate(
      date                            = as.character(date),
      start_time                      = NA_character_,
      stop_time                       = NA_character_,
      number_of_transect_lines        = NA_character_,
      length_of_transect_lines        = NA_character_,
      point_spacing                   = NA_character_,
      number_of_quadrats_per_transect = NA_character_,
      site_raw                        = as.character(location),
      site_key                        = clean_hawaiian_text(location),
      latitude                        = NA_character_,
      longitude                       = NA_character_,
      school                          = NA_character_,
      data_entry_by                   = coder_id,
      taxon_raw                       = clean_hawaiian_text(taxon_raw),
      count                           = as.numeric(count_raw)
    ) %>%
    select(date, start_time, stop_time, number_of_transect_lines, length_of_transect_lines, 
           point_spacing, number_of_quadrats_per_transect, site_raw, site_key, latitude, 
           longitude, school, data_entry_by, transect_id, position, taxon_raw, count)
  
  return(long_df)
}



### Compile batch matrix groups and union pass ################################
# 1. Run Pipeline 1 for the 2004-2020 Transposed Batches
old_target_files <- c(
  "2004_datasheet_anonymized_output.csv", "2005_datasheet_anonymized_output.csv",
  "2006_and_2007_datasheet_anonymized_output.csv", "2008_datasheet_anonymized_output.csv",
  "2016_datasheet_anonymized_output.csv", "2017_datasheet_anonymized_output.csv",
  "2018_datasheet_anonymized_output.csv", "2019_datasheet_anonymized_output.csv",
  "2020_datasheet_anonymized_output.csv"
)
file_list_old <- here("Data_Clean", old_target_files)
data_2004_2020 <- map(file_list_old, process_transposed_file) %>% bind_rows()

# 2. Run Pipeline 2 for the 2024-2026 Wide Batches
new_wide_files <- c(
  "2024_datasheet_anonymized_output.csv", "2024_datasheet_copy_anonymized_output.csv", 
  "2025_datasheet_BIOL499_anonymized_output.csv", "2026_datasheet_anonymized_output.csv",
  "2026_datasheet_Mauliola_anonymized_output.csv"
)
data_2024_2026 <- map_dfr(new_wide_files, process_wide_year)

# 3. Stack layers and map your official master snail taxonomy criteria safely
master_output <- bind_rows(data_2004_2020, data_2024_2026) %>%
  filter(!is.na(date), !is.na(site_key)) %>%
  
  # Coerce string trackers back to clean numbers after the stack pass finishes
  mutate(across(any_of(c("transect_id", "position", "number_of_transect_lines", "length_of_transect_lines", "number_of_quadrats_per_transect", "latitude", "longitude")), ~suppressWarnings(as.numeric(.)))) %>%
  distinct(date, site_key, transect_id, position, taxon_raw, count, .keep_all = TRUE)

# 4. Export clean stacked master spreadsheet
dir.create(here("Outputs"), showWarnings = FALSE, recursive = TRUE)

# 5. Export finalized consolidated master long dataset to disk
write_csv(master_output, here("Outputs", "master_joined_long.csv"))
message("Success! Clean dataset saved with total rows: ", nrow(master_output), " clean rows.")



### Check unique names for taxon_raw, site_raw, and site_key ##################
# 1. Isolate unique Site Raw names with their corresponding computer matching keys
unique_sites <- master_output %>%
  distinct(site_raw, site_key) %>%
  arrange(site_key)

# 2. Isolate unique Taxon Raw computer matching keys
unique_taxa <- master_output %>%
  distinct(taxon_raw) %>%
  arrange(taxon_raw)

# Create a blank workbook container object
wb <- createWorkbook()

# Add individual tracking tabs
addWorksheet(wb, "Unique Sites Matrix")
addWorksheet(wb, "Unique Species Taxonomy")

# Write the data tracking matrices to their respective sheet slots
writeData(wb, "Unique Sites Matrix", unique_sites)
writeData(wb, "Unique Species Taxonomy", unique_taxa)

# Save the unified workbook out to disk
saveWorkbook(wb, here("Outputs", "master_unique_lookups.xlsx"), overwrite = TRUE)
message("Success! Multi-tab master lookup workbook saved to outputs/master_unique_lookups.xlsx")



### Validation script to check if every quadrat row successfully sums up to 25 total points
# 1. Run the point-sum validation check per unique quadrat sample
quadrat_validation <- master_output %>%
  # Group by the unique environmental anchors that isolate a single quadrat sample
  group_by(date, site_key, transect_id, position) %>%
  summarize(
    total_points_counted = sum(count, na.rm = TRUE),
    unique_species_found = n_distinct(taxon_raw),
    .groups = "drop"
  ) %>%
  # Flag any quadrat that does not equal exactly 25 points
  mutate(
    status = case_when(
      total_points_counted == 25 ~ "Valid (25 Pts)",
      total_points_counted < 25  ~ "Undercounted",
      total_points_counted > 25  ~ "Overcounted"
    )
  )

# 2. Print out a summary profile of your data integrity check
message("=== DATA SPRINT VALIDATION SUMMARY ===")
print(table(quadrat_validation$status))

# 3. Isolate the broken rows (mismatched point totals) for manual review
broken_quadrats <- quadrat_validation %>%
  filter(status != "Valid (25 Pts)") %>%
  arrange(site_key, transect_id, position)

if (nrow(broken_quadrats) > 0) {
  warning("Caution: Found ", nrow(broken_quadrats), " quadrats with point totals that do not equal 25.")
  print(head(broken_quadrats, 20))
  
  # Export the broken quadrats to a CSV file so the team can cross-reference the paper sheets
  write_csv(broken_quadrats, here("Outputs", "quadrats_needing_review.csv"))
  message("Saved list of problematic quadrats to: outputs/quadrats_needing_review.csv")
} else {
  message("Perfect Data! All processed quadrats sum up to exactly 25 points.")
}



### Programmatically normalize counts to Percent Cover to bypass the broken totals
normalized_analysis_data <- master_output %>%
  group_by(date, site_key, transect_id, position) %>%
  mutate(
    quadrat_total_pts = sum(count, na.rm = TRUE),
    percent_cover     = (count / quadrat_total_pts) * 100
  ) %>%
  ungroup()

# Export this complete dataset with percent cover
write_csv(normalized_analysis_data, here("Outputs", "master_normalized_percent_cover.csv"))
message("Success! Your normalized data sheet is ready at: Outputs/master_normalized_percent_cover.csv")

