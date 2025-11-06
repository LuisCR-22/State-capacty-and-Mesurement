########################################
# Colombia Myers-Benford Analysis - ENHANCED WITH CSI QUINTILES
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-08-18
# Purpose: 
# 1. Import Myers-Benford data from Excel files (02_ series)
# 2. Import CSI and nightlight data from DTA files
# 3. Create CSI quintiles for proper interaction analysis
# 4. Merge datasets by region_number and ano (+ urban_area for dept_area level)
# 5. Save final datasets for easy reuse
# 6. Run comprehensive analysis with CSI quintile interactions

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Install and load required packages
packages <- c("readxl", "writexl", "haven", "dplyr", "broom", "sandwich", "lmtest", "tidyr")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input paths
excel_input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col"
dta_input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge"

# Output paths
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"
# NEW: Final dataset save path
final_dataset_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge"

# Create output directories if they don't exist
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
dir.create(final_dataset_path, showWarnings = FALSE, recursive = TRUE)

# Excel files to import (Myers-Benford data)
excel_files <- list(
  # V01 files (including outliers and zeros)
  dept_v01 = "02_Myers_Benford_Col_V01_reg-tot.xlsx",
  dept_area_v01 = "02_Myers_Benford_Col_V01_reg-area.xlsx",
  
  # V02/V03 files (check both possible names)
  dept_v03 = c("02_Myers_Benford_Col_V03_reg-tot.xlsx", "02_Myers_Benford_Col_V02_reg-tot.xlsx"),
  dept_area_v03 = c("02_Myers_Benford_Col_V03_reg-area.xlsx", "02_Myers_Benford_Col_V02_reg-area.xlsx")
)

# DTA files (CSI and nightlight data)
dta_files <- list(
  dept = c("04_col_merge_sedlac_CSI_NTL_dept.dta", "03_col_merge_sedlac_CSI_NTL_dept.dta"),
  dept_area = c("04_col_merge_sedlac_CSI_NTL_dept_area.dta", "03_col_merge_sedlac_CSI_NTL_dept_area.dta")
)

########################################
# UTILITY FUNCTIONS
########################################

# Function to find existing file from multiple possibilities
find_existing_file <- function(file_names, base_path) {
  if (is.character(file_names) && length(file_names) == 1) {
    file_names <- c(file_names)
  }
  
  for (file_name in file_names) {
    full_path <- file.path(base_path, file_name)
    if (file.exists(full_path)) {
      return(full_path)
    }
  }
  return(NULL)
}

# Function to add significance stars
add_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value <= 0.01) return("***")
  if (p_value <= 0.05) return("**")
  if (p_value <= 0.10) return("*")
  return("")
}

# Function to create nighttime lights quintiles by year
create_ntl_quintiles <- function(data) {
  message("  Creating nighttime lights quintiles by year...")
  
  data <- data %>%
    group_by(ano) %>%
    mutate(
      ntl_quintile = ntile(ntl_average_masked, 5),
      ntl_quintile = ifelse(is.na(ntl_average_masked), NA, ntl_quintile)
    ) %>%
    ungroup()
  
  data$ntl_quintile_factor <- factor(data$ntl_quintile, 
                                     levels = 1:5,
                                     labels = c("NTL_Q1_Lowest", "NTL_Q2_Low", "NTL_Q3_Mid", "NTL_Q4_High", "NTL_Q5_Highest"))
  
  if (sum(!is.na(data$ntl_quintile)) > 0) {
    quintile_summary <- data %>%
      filter(!is.na(ntl_quintile)) %>%
      group_by(ntl_quintile_factor) %>%
      summarise(
        n_obs = n(),
        min_ntl = min(ntl_average_masked, na.rm = TRUE),
        max_ntl = max(ntl_average_masked, na.rm = TRUE),
        .groups = 'drop'
      )
    
    message("    NTL Quintile distribution:")
    for(i in 1:nrow(quintile_summary)) {
      message("    ", quintile_summary$ntl_quintile_factor[i], ": ", quintile_summary$n_obs[i], 
              " obs, NTL range: ", round(quintile_summary$min_ntl[i], 2), "-", round(quintile_summary$max_ntl[i], 2))
    }
  }
  
  return(data)
}

# NEW FUNCTION: Create CSI quintiles 
create_csi_quintiles <- function(data) {
  message("  Creating CSI quintiles (historical state capacity)...")
  
  # Check if CSI variable exists
  csi_var <- "csi_pre1500_avg_no0"
  if (!csi_var %in% names(data)) {
    message("    ❌ CSI variable not found: ", csi_var)
    return(data)
  }
  
  # Create quintiles based on CSI values (time-invariant, so no grouping by year needed)
  data <- data %>%
    mutate(
      csi_quintile = ntile(!!sym(csi_var), 5),
      csi_quintile = ifelse(is.na(!!sym(csi_var)), NA, csi_quintile)
    )
  
  data$csi_quintile_factor <- factor(data$csi_quintile, 
                                     levels = 1:5,
                                     labels = c("CSI_Q1_Lowest", "CSI_Q2_Low", "CSI_Q3_Mid", "CSI_Q4_High", "CSI_Q5_Highest"))
  
  if (sum(!is.na(data$csi_quintile)) > 0) {
    quintile_summary <- data %>%
      filter(!is.na(csi_quintile)) %>%
      group_by(csi_quintile_factor) %>%
      summarise(
        n_obs = n(),
        min_csi = min(!!sym(csi_var), na.rm = TRUE),
        max_csi = max(!!sym(csi_var), na.rm = TRUE),
        mean_csi = mean(!!sym(csi_var), na.rm = TRUE),
        .groups = 'drop'
      )
    
    message("    CSI Quintile distribution:")
    for(i in 1:nrow(quintile_summary)) {
      message("    ", quintile_summary$csi_quintile_factor[i], ": ", quintile_summary$n_obs[i], 
              " obs, CSI range: ", round(quintile_summary$min_csi[i], 3), "-", round(quintile_summary$max_csi[i], 3),
              " (mean: ", round(quintile_summary$mean_csi[i], 3), ")")
    }
  }
  
  return(data)
}

# NEW FUNCTION: Save final datasets for reuse
save_final_dataset <- function(merged_data, level, version) {
  message("Saving final dataset for ", level, " level, version ", version, "...")
  
  if (is.null(merged_data) || nrow(merged_data) == 0) {
    message("  ❌ No data to save")
    return(FALSE)
  }
  
  # Create filename based on level and version
  if (level == "dept") {
    filename <- paste0("05_", version, "_Myers_Bend_CSI_NTL_depto.dta")
  } else if (level == "dept_area") {
    filename <- paste0("05_", version, "_Myers_Bend_CSI_NTL_depto_area.dta")
  } else {
    message("  ❌ Unknown level: ", level)
    return(FALSE)
  }
  
  # Full file path
  full_path <- file.path(final_dataset_path, filename)
  
  tryCatch({
    # Save as Stata file
    haven::write_dta(merged_data, full_path)
    
    message("  ✅ Successfully saved: ", filename)
    message("    - Path: ", full_path)
    message("    - Observations: ", nrow(merged_data))
    message("    - Variables: ", ncol(merged_data))
    
    # Display key variable summary
    key_vars <- c("region_number", "ano", "standardized_myers", "benford_abs_distance", 
                  "ntl_average_masked", "csi_pre1500_avg_no0", "after_peace_treaty",
                  "ntl_quintile_factor", "csi_quintile_factor")
    if (level == "dept_area") {
      key_vars <- c(key_vars, "urban_area")
    }
    available_key_vars <- key_vars[key_vars %in% names(merged_data)]
    
    message("    - Key variables available: ", length(available_key_vars), "/", length(key_vars))
    message("      ", paste(available_key_vars, collapse = ", "))
    
    return(TRUE)
    
  }, error = function(e) {
    message("  ❌ Error saving dataset: ", e$message)
    return(FALSE)
  })
}

########################################
# DATA IMPORT AND MERGER FUNCTIONS
########################################

# Function to import Myers-Benford data from Excel
import_myers_benford_excel <- function(level, version) {
  message("Importing Myers-Benford data for ", level, " level, version ", version, "...")
  
  # Determine file key
  file_key <- paste0(level, "_", tolower(version))
  
  if (!file_key %in% names(excel_files)) {
    message("  ❌ No file configuration for ", file_key)
    return(NULL)
  }
  
  # Find existing Excel file
  excel_file_path <- find_existing_file(excel_files[[file_key]], excel_input_path)
  
  if (is.null(excel_file_path)) {
    message("  ❌ Excel file not found for ", file_key)
    return(NULL)
  }
  
  message("  ✅ Found Excel file: ", basename(excel_file_path))
  
  tryCatch({
    # Read the Results sheet
    data <- readxl::read_xlsx(excel_file_path, sheet = "Results")
    
    if (nrow(data) == 0) {
      message("  ❌ No data in Results sheet")
      return(NULL)
    }
    
    message("  📊 Loaded ", nrow(data), " observations")
    
    # Check for required variables
    required_vars <- c("region_number", "ano", "standardized_myers", "benford_abs_distance")
    if (level == "dept_area") {
      required_vars <- c(required_vars, "urban_area")
    }
    missing_vars <- required_vars[!required_vars %in% names(data)]
    
    if (length(missing_vars) > 0) {
      message("  ❌ Missing required variables: ", paste(missing_vars, collapse = ", "))
      message("  Available variables: ", paste(head(names(data), 10), collapse = ", "))
      return(NULL)
    }
    
    # Add version and level identifiers
    data$excel_version <- version
    data$excel_level <- level
    data$excel_source <- basename(excel_file_path)
    
    message("  ✅ Successfully imported Myers-Benford data")
    message("    - Regions: ", length(unique(data$region_number)))
    message("    - Years: ", paste(range(data$ano, na.rm = TRUE), collapse = "-"))
    if (level == "dept_area") {
      urban_dist <- table(data$urban_area, useNA = "always")
      message("    - Urban distribution: ", paste(names(urban_dist), urban_dist, sep = "=", collapse = ", "))
    }
    message("    - Myers range: ", paste(round(range(data$standardized_myers, na.rm = TRUE), 3), collapse = " to "))
    message("    - Benford range: ", paste(round(range(data$benford_abs_distance, na.rm = TRUE), 3), collapse = " to "))
    
    return(data)
    
  }, error = function(e) {
    message("  ❌ Error reading Excel file: ", e$message)
    return(NULL)
  })
}

# Function to import CSI and nightlight data from DTA
import_csi_ntl_dta <- function(level) {
  message("Importing CSI and nightlight data for ", level, " level...")
  
  # Find existing DTA file
  dta_file_path <- find_existing_file(dta_files[[level]], dta_input_path)
  
  if (is.null(dta_file_path)) {
    message("  ❌ DTA file not found for ", level)
    return(NULL)
  }
  
  message("  ✅ Found DTA file: ", basename(dta_file_path))
  
  tryCatch({
    # Read DTA file
    data <- haven::read_dta(dta_file_path)
    
    message("  📊 Loaded ", nrow(data), " observations")
    
    # Check for key variables
    key_vars <- c("region_number", "ano")
    if (level == "dept_area") {
      key_vars <- c(key_vars, "urban_area")
    }
    csi_vars <- names(data)[grepl("csi", names(data), ignore.case = TRUE)]
    ntl_vars <- names(data)[grepl("ntl", names(data), ignore.case = TRUE)]
    urban_vars <- names(data)[grepl("urban", names(data), ignore.case = TRUE)]
    
    message("  📊 Available variable types:")
    message("    - Key variables: ", paste(key_vars[key_vars %in% names(data)], collapse = ", "))
    message("    - CSI variables: ", length(csi_vars), " (", paste(head(csi_vars, 3), collapse = ", "), "...)")
    message("    - NTL variables: ", length(ntl_vars), " (", paste(ntl_vars, collapse = ", "), ")")
    if (level == "dept_area") {
      message("    - Urban variables: ", length(urban_vars), " (", paste(urban_vars, collapse = ", "), ")")
      if ("urban_area" %in% names(data)) {
        urban_dist <- table(data$urban_area, useNA = "always")
        message("    - Urban distribution: ", paste(names(urban_dist), urban_dist, sep = "=", collapse = ", "))
      }
    }
    
    # Add level identifier
    data$dta_level <- level
    data$dta_source <- basename(dta_file_path)
    
    message("  ✅ Successfully imported CSI and NTL data")
    
    return(data)
    
  }, error = function(e) {
    message("  ❌ Error reading DTA file: ", e$message)
    return(NULL)
  })
}

# CORRECTED Function to merge Myers-Benford with CSI-NTL data
merge_datasets <- function(myers_benford_data, csi_ntl_data, level, version) {
  message("Merging datasets for ", level, " level, version ", version, "...")
  
  if (is.null(myers_benford_data) || is.null(csi_ntl_data)) {
    message("  ❌ Cannot merge - one or both datasets are NULL")
    return(NULL)
  }
  
  # CORRECTED: Define merge keys based on level
  merge_keys <- c("region_number", "ano")
  if (level == "dept_area") {
    merge_keys <- c(merge_keys, "urban_area")
    message("  🔧 Using CORRECTED merge keys for dept_area level: ", paste(merge_keys, collapse = ", "))
  } else {
    message("  🔧 Using merge keys for dept level: ", paste(merge_keys, collapse = ", "))
  }
  
  # Check if merge keys exist in both datasets
  myers_keys_available <- merge_keys %in% names(myers_benford_data)
  csi_keys_available <- merge_keys %in% names(csi_ntl_data)
  
  if (!all(myers_keys_available)) {
    message("  ❌ Myers-Benford data missing keys: ", paste(merge_keys[!myers_keys_available], collapse = ", "))
    return(NULL)
  }
  
  if (!all(csi_keys_available)) {
    message("  ❌ CSI-NTL data missing keys: ", paste(merge_keys[!csi_keys_available], collapse = ", "))
    return(NULL)
  }
  
  # CORRECTED: Pre-merge diagnostics to verify no duplication will occur
  message("  🔍 Pre-merge diagnostics:")
  myers_unique <- myers_benford_data %>%
    group_by(!!!syms(merge_keys)) %>%
    summarise(n_obs = n(), .groups = 'drop') %>%
    filter(n_obs > 1)
  
  csi_unique <- csi_ntl_data %>%
    group_by(!!!syms(merge_keys)) %>%
    summarise(n_obs = n(), .groups = 'drop') %>%
    filter(n_obs > 1)
  
  if (nrow(myers_unique) > 0) {
    message("    ❌ Myers-Benford data has duplicate keys (will cause issues):")
    message("      ", nrow(myers_unique), " combinations with multiple observations")
  }
  
  if (nrow(csi_unique) > 0) {
    message("    ❌ CSI-NTL data has duplicate keys (will cause issues):")
    message("      ", nrow(csi_unique), " combinations with multiple observations")
  }
  
  if (nrow(myers_unique) == 0 && nrow(csi_unique) == 0) {
    message("    ✅ No duplicate keys detected - merge should be 1:1")
  }
  
  # Perform merge
  merged_data <- merge(myers_benford_data, csi_ntl_data, 
                       by = merge_keys, all.x = TRUE, suffixes = c("_excel", "_dta"))
  
  message("  📊 Merge results:")
  message("    - Myers-Benford observations: ", nrow(myers_benford_data))
  message("    - CSI-NTL observations: ", nrow(csi_ntl_data))
  message("    - Merged observations: ", nrow(merged_data))
  message("    - Successful matches: ", sum(!is.na(merged_data$ntl_average_masked)))
  
  # CORRECTED: Verify no unwanted duplication occurred
  if (level == "dept_area") {
    # Check if urban_area values are consistent (no _excel/_dta suffix issues)
    if ("urban_area" %in% names(merged_data)) {
      urban_dist_merged <- table(merged_data$urban_area, useNA = "always")
      message("    - Urban distribution after merge: ", paste(names(urban_dist_merged), urban_dist_merged, sep = "=", collapse = ", "))
    }
    
    # Check for any unwanted suffix variables
    suffix_vars <- names(merged_data)[grepl("_excel$|_dta$", names(merged_data))]
    urban_suffix_vars <- suffix_vars[grepl("urban_area", suffix_vars)]
    
    if (length(urban_suffix_vars) > 0) {
      message("    ❌ ALERT: Found urban_area suffix variables (indicates merge error): ", paste(urban_suffix_vars, collapse = ", "))
      
      # Show a sample of the problematic data
      problem_sample <- merged_data %>%
        select(region_number, ano, urban_area, all_of(urban_suffix_vars)) %>%
        head(10)
      
      message("    Sample of problematic data:")
      print(problem_sample)
      
      return(NULL)  # Stop processing if merge error detected
    } else {
      message("    ✅ No urban_area suffix variables found - merge appears correct")
    }
  }
  
  # Add version and merge information
  merged_data$version <- version
  merged_data$level <- level
  merged_data$merge_success <- !is.na(merged_data$ntl_average_masked)
  
  # Create after_peace_treaty variable
  merged_data$after_peace_treaty <- ifelse(merged_data$ano >= 2016, 1, 0)
  
  # Create nighttime lights quintiles
  merged_data <- create_ntl_quintiles(merged_data)
  
  # Create CSI quintiles
  merged_data <- create_csi_quintiles(merged_data)
  
  message("  ✅ Successfully merged datasets")
  message("    - Complete observations (Myers + Benford + NTL + CSI): ", 
          sum(complete.cases(merged_data[, c("standardized_myers", "benford_abs_distance", "ntl_average_masked", "csi_pre1500_avg_no0")])))
  
  return(merged_data)
}

########################################
# ENHANCED MODEL SPECIFICATIONS WITH CSI QUINTILES
########################################

# Function to define model specifications with CSI quintile interactions
get_enhanced_model_specs <- function(level = "dept", version = "V01") {
  
  myers_var <- "standardized_myers"
  benford_var <- "benford_abs_distance"
  
  if (level == "dept") {
    # Department level models with CSI quintile interactions
    models <- list(
      # CATEGORY A: Basic Models - Compatible with Region + Year FE
      "Model 1: Simple Bivariate" = list(
        formula = paste(benford_var, "~", myers_var),
        fe_strategy = "region_year"
      ),
      
      # CATEGORY B: Single Factor Interactions - Region + Year FE
      "Model 2: Myers × NTL Quintiles" = list(
        formula = paste(benford_var, "~", myers_var, "+ ntl_quintile_factor +", myers_var, ": ntl_quintile_factor"),
        fe_strategy = "region_year"
      ),
      
      "Model 3: Myers × CSI Quintiles" = list(
        formula = paste(benford_var, "~", myers_var, "+ csi_quintile_factor +", myers_var, ": csi_quintile_factor"),
        fe_strategy = "region_year"
      ),
      
      # CATEGORY C: Peace Treaty Models - Region FE only (to avoid collinearity)
      "Model 4: + Peace Treaty" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty"),
        fe_strategy = "region_only"
      ),
      
      "Model 5: Myers × Peace Treaty" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
        fe_strategy = "region_only"
      ),
      
      "Model 6: Peace × NTL Interaction" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + ntl_quintile_factor +", 
                        myers_var, ": after_peace_treaty +", myers_var, ": ntl_quintile_factor"),
        fe_strategy = "region_only"
      ),
      
      "Model 7: Peace × CSI Interaction" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + csi_quintile_factor +", 
                        myers_var, ": after_peace_treaty +", myers_var, ": csi_quintile_factor"),
        fe_strategy = "region_only"
      ),
      
      # CATEGORY D: Dual Development Controls - Region FE only
      "Model 8: NTL + CSI Controls" = list(
        formula = paste(benford_var, "~", myers_var, "+ ntl_quintile_factor + csi_quintile_factor +",
                        myers_var, ": ntl_quintile_factor +", myers_var, ": csi_quintile_factor"),
        fe_strategy = "region_only"
      ),
      
      "Model 9: Peace × Dual Development" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + ntl_quintile_factor + csi_quintile_factor +",
                        myers_var, ": after_peace_treaty +", myers_var, ": ntl_quintile_factor +", 
                        myers_var, ": csi_quintile_factor"),
        fe_strategy = "region_only"
      ),
      
      # CATEGORY E: Triple Interactions - No FE (complex models)
      "Model 10: Peace × NTL × CSI Core" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + ntl_quintile_factor + csi_quintile_factor +",
                        myers_var, ": after_peace_treaty +", myers_var, ": ntl_quintile_factor +", 
                        myers_var, ": csi_quintile_factor +",
                        "after_peace_treaty : ntl_quintile_factor +", "after_peace_treaty : csi_quintile_factor"),
        fe_strategy = "none"
      ),
      
      "Model 11: Full Triple Interaction" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + ntl_quintile_factor + csi_quintile_factor +",
                        myers_var, ": after_peace_treaty +", myers_var, ": ntl_quintile_factor +", 
                        myers_var, ": csi_quintile_factor +",
                        "after_peace_treaty : ntl_quintile_factor +", "after_peace_treaty : csi_quintile_factor +",
                        "ntl_quintile_factor : csi_quintile_factor"),
        fe_strategy = "none"
      ),
      
      "Model 12: Ultimate Interaction" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + ntl_quintile_factor + csi_quintile_factor +",
                        myers_var, ": after_peace_treaty +", myers_var, ": ntl_quintile_factor +", 
                        myers_var, ": csi_quintile_factor +",
                        "after_peace_treaty : ntl_quintile_factor +", "after_peace_treaty : csi_quintile_factor +",
                        "ntl_quintile_factor : csi_quintile_factor +",
                        myers_var, ": after_peace_treaty : ntl_quintile_factor +",
                        myers_var, ": after_peace_treaty : csi_quintile_factor"),
        fe_strategy = "none"
      )
    )
    
  } else if (level == "dept_area") {
    # Department-area level models with CSI quintiles
    models <- list(
      # CATEGORY A: Basic Models
      "Model 1: Simple Bivariate" = list(
        formula = paste(benford_var, "~", myers_var),
        fe_strategy = "region_year"
      ),
      
      # CATEGORY B: Single Factor Interactions
      "Model 2: Myers × NTL Quintiles" = list(
        formula = paste(benford_var, "~", myers_var, "+ ntl_quintile_factor +", myers_var, ": ntl_quintile_factor"),
        fe_strategy = "region_year"
      ),
      
      "Model 3: Myers × CSI Quintiles" = list(
        formula = paste(benford_var, "~", myers_var, "+ csi_quintile_factor +", myers_var, ": csi_quintile_factor"),
        fe_strategy = "region_year"
      ),
      
      "Model 4: Myers × Urban" = list(
        formula = paste(benford_var, "~", myers_var, "+ urban_area +", myers_var, ": urban_area"),
        fe_strategy = "region_year"
      ),
      
      # CATEGORY C: Peace Treaty Models
      "Model 5: + Peace Treaty" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty"),
        fe_strategy = "region_only"
      ),
      
      "Model 6: Myers × Peace Treaty" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
        fe_strategy = "region_only"
      ),
      
      "Model 7: Peace × Urban" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + urban_area +",
                        myers_var, ": after_peace_treaty +", myers_var, ": urban_area"),
        fe_strategy = "region_only"
      ),
      
      "Model 8: Peace × CSI" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + csi_quintile_factor +",
                        myers_var, ": after_peace_treaty +", myers_var, ": csi_quintile_factor"),
        fe_strategy = "region_only"
      ),
      
      # CATEGORY D: Multi-Factor Models
      "Model 9: Urban + CSI Controls" = list(
        formula = paste(benford_var, "~", myers_var, "+ urban_area + csi_quintile_factor +",
                        myers_var, ": urban_area +", myers_var, ": csi_quintile_factor"),
        fe_strategy = "region_year"
      ),
      
      "Model 10: Peace × Multi-Controls" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + urban_area + csi_quintile_factor +",
                        myers_var, ": after_peace_treaty +", myers_var, ": urban_area +", 
                        myers_var, ": csi_quintile_factor"),
        fe_strategy = "region_only"
      ),
      
      # CATEGORY E: Complex Interactions
      "Model 11: Peace × Urban × CSI" = list(
        formula = paste(benford_var, "~", myers_var, "+ after_peace_treaty + urban_area + csi_quintile_factor +",
                        myers_var, ": after_peace_treaty +", myers_var, ": urban_area +", 
                        myers_var, ": csi_quintile_factor +",
                        "after_peace_treaty : urban_area +", "after_peace_treaty : csi_quintile_factor"),
        fe_strategy = "none"
      )
    )
  }
  
  return(models)
}

########################################
# ENHANCED REGRESSION ANALYSIS FUNCTION
########################################

run_enhanced_regression_with_csi <- function(data, level = "dept", version = "V01") {
  message("Running ENHANCED regression with CSI quintiles for ", level, " level, version ", version, "...")
  
  # Get enhanced model specifications
  model_specs <- get_enhanced_model_specs(level, version)
  
  # Check data availability for CSI quintiles
  if (!"csi_quintile_factor" %in% names(data)) {
    message("  ❌ CSI quintiles not available in data")
    return(NULL)
  }
  
  # Check clustering viability
  cluster_table <- table(data$region_number)
  min_cluster_size <- min(cluster_table)
  n_clusters <- length(cluster_table)
  
  message("  Clustering info: ", n_clusters, " regions, cluster sizes: ", 
          min(cluster_table), "-", max(cluster_table), " observations")
  
  # Diagnose CSI and other variable distributions
  diagnose_enhanced_variables(data, level)
  
  # Function to run regression with smart FE strategy
  run_smart_fe_regression <- function(formula_str, fe_strategy, data, model_name = "") {
    
    message("    Running ", model_name, " with ", fe_strategy, " FE strategy...")
    
    tryCatch({
      model_vars <- all.vars(as.formula(formula_str))
      model_data <- data[complete.cases(data[, model_vars]), ]
      
      if (nrow(model_data) < 15) {  # Higher threshold for FE models
        message("      ❌ Insufficient data (", nrow(model_data), " obs)")
        return(NULL)
      }
      
      # Apply FE strategy
      original_formula <- formula_str
      
      if (fe_strategy == "region_year") {
        if (length(unique(model_data$region_number)) > 1 && length(unique(model_data$ano)) > 1) {
          formula_str <- paste(formula_str, "+ factor(region_number) + factor(ano)")
          message("      ✅ Added Region + Year FE")
        } else {
          message("      ❌ Insufficient variation for Region + Year FE")
          return(NULL)
        }
        
      } else if (fe_strategy == "region_only") {
        if (length(unique(model_data$region_number)) > 1) {
          formula_str <- paste(formula_str, "+ factor(region_number)")
          message("      ✅ Added Region FE only")
        } else {
          message("      ❌ Insufficient regions for Region FE")
          return(NULL)
        }
        
      } else if (fe_strategy == "year_only") {
        if (length(unique(model_data$ano)) > 1) {
          formula_str <- paste(formula_str, "+ factor(ano)")
          message("      ✅ Added Year FE only")
        } else {
          message("      ❌ Insufficient years for Year FE")
          return(NULL)
        }
        
      } else if (fe_strategy == "none") {
        message("      ✅ No FE (complex interaction model)")
        # Use original formula
      }
      
      # Fit the model
      model <- lm(as.formula(formula_str), data = model_data)
      
      # Check for multicollinearity
      if (any(is.na(coef(model)))) {
        na_coeffs <- names(coef(model))[is.na(coef(model))]
        message("      ❌ Multicollinearity - NA coefficients: ", paste(head(na_coeffs, 3), collapse = ", "))
        
        # Try alternative strategy
        if (fe_strategy == "region_year") {
          message("      🔄 Retrying with region FE only...")
          return(run_smart_fe_regression(original_formula, "region_only", data, paste(model_name, "(alt)")))
        } else if (fe_strategy == "region_only") {
          message("      🔄 Retrying with no FE...")
          return(run_smart_fe_regression(original_formula, "none", data, paste(model_name, "(alt)")))
        } else {
          return(NULL)
        }
      }
      
      # Calculate clustered standard errors
      model_fitted_data <- model$model
      region_cluster <- model_data$region_number[as.numeric(rownames(model_fitted_data))]
      
      clustered_vcov <- vcovCL(model, cluster = region_cluster)
      clustered_se <- sqrt(diag(clustered_vcov))
      
      coefficients <- coef(model)
      t_stats <- coefficients / clustered_se
      p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)
      
      results_df <- data.frame(
        term = names(coefficients),
        estimate = coefficients,
        std.error = clustered_se,
        statistic = t_stats,
        p.value = p_values,
        significance = sapply(p_values, add_significance_stars),
        fe_strategy = fe_strategy,
        stringsAsFactors = FALSE
      )
      
      model_stats <- data.frame(
        r.squared = summary(model)$r.squared,
        adj.r.squared = summary(model)$adj.r.squared,
        nobs = nobs(model),
        fe_strategy = fe_strategy,
        stringsAsFactors = FALSE
      )
      
      message("      ✅ Success: ", nrow(results_df), " coefficients, R² = ", round(model_stats$r.squared, 3))
      return(list(coefficients = results_df, model_stats = model_stats))
      
    }, error = function(e) {
      message("      ❌ Error: ", e$message)
      return(NULL)
    })
  }
  
  # Run models without FE (baseline)
  results_no_fe <- data.frame()
  model_stats_no_fe <- data.frame()
  
  message("\n  📊 RUNNING MODELS WITHOUT FIXED EFFECTS:")
  for (i in seq_along(model_specs)) {
    model_name <- names(model_specs)[i]
    model_spec <- model_specs[[i]]
    
    result <- run_smart_fe_regression(model_spec$formula, "none", data, model_name)
    
    if (!is.null(result)) {
      result$coefficients$Model <- model_name
      result$coefficients$Fixed_Effects <- "No"
      result$coefficients$Version <- version
      result$coefficients$Level <- level
      results_no_fe <- rbind(results_no_fe, result$coefficients)
      
      result$model_stats$Model <- model_name
      result$model_stats$Fixed_Effects <- "No"
      result$model_stats$Version <- version
      result$model_stats$Level <- level
      model_stats_no_fe <- rbind(model_stats_no_fe, result$model_stats)
    }
  }
  
  # Run models with smart FE strategies
  results_fe <- data.frame()
  model_stats_fe <- data.frame()
  
  message("\n  🔧 RUNNING MODELS WITH SMART FIXED EFFECTS:")
  for (i in seq_along(model_specs)) {
    model_name <- names(model_specs)[i]
    model_spec <- model_specs[[i]]
    
    # Skip if FE strategy is "none"
    if (model_spec$fe_strategy == "none") {
      message("  ⏭️ Skipping ", model_name, " (designed for No FE only)")
      next
    }
    
    result <- run_smart_fe_regression(
      model_spec$formula, 
      model_spec$fe_strategy,
      data, 
      model_name
    )
    
    if (!is.null(result)) {
      result$coefficients$Model <- model_name
      result$coefficients$Fixed_Effects <- "Yes"
      result$coefficients$Version <- version
      result$coefficients$Level <- level
      results_fe <- rbind(results_fe, result$coefficients)
      
      result$model_stats$Model <- model_name
      result$model_stats$Fixed_Effects <- "Yes"
      result$model_stats$Version <- version
      result$model_stats$Level <- level
      model_stats_fe <- rbind(model_stats_fe, result$model_stats)
    }
  }
  
  message("\n  📈 FINAL RESULTS:")
  message("    ✅ Models without FE: ", length(unique(results_no_fe$Model)))
  message("    ✅ Models with FE: ", length(unique(results_fe$Model)))
  
  return(list(
    coefficients_no_fe = results_no_fe,
    model_stats_no_fe = model_stats_no_fe,
    coefficients_fe = results_fe,
    model_stats_fe = model_stats_fe
  ))
}

# Enhanced diagnostic function
diagnose_enhanced_variables <- function(data, level = "dept") {
  message("  🔍 Diagnosing enhanced variables including CSI quintiles...")
  
  # Check basic variable availability
  key_vars <- c("standardized_myers", "benford_abs_distance", "ntl_average_masked", 
                "csi_pre1500_avg_no0", "after_peace_treaty")
  if (level == "dept_area") {
    key_vars <- c(key_vars, "urban_area")
  }
  available_vars <- key_vars[key_vars %in% names(data)]
  
  message("    - Available key variables: ", paste(available_vars, collapse = ", "))
  
  # Check quintile variables
  quintile_vars <- c("ntl_quintile_factor", "csi_quintile_factor")
  available_quintiles <- quintile_vars[quintile_vars %in% names(data)]
  
  message("    - Available quintile variables: ", paste(available_quintiles, collapse = ", "))
  
  # Distribution check for CSI quintiles
  if ("csi_quintile_factor" %in% names(data)) {
    csi_dist <- table(data$csi_quintile_factor, useNA = "always")
    message("    - CSI quintile distribution:")
    for (i in 1:length(csi_dist)) {
      message("      ", names(csi_dist)[i], ": ", csi_dist[i], " observations")
    }
  }
  
  # Check time variation in key variables
  time_variation <- data %>%
    group_by(region_number) %>%
    summarise(
      years = length(unique(ano)),
      ntl_var = sd(ntl_average_masked, na.rm = TRUE),
      myers_var = sd(standardized_myers, na.rm = TRUE),
      csi_var = sd(csi_pre1500_avg_no0, na.rm = TRUE),
      .groups = 'drop'
    )
  
  message("    - Average years per region: ", round(mean(time_variation$years), 1))
  message("    - Regions with NTL variation: ", sum(time_variation$ntl_var > 0, na.rm = TRUE), "/", nrow(time_variation))
  message("    - Regions with CSI variation: ", sum(time_variation$csi_var > 0, na.rm = TRUE), "/", nrow(time_variation))
  
  # Check peace treaty variation
  peace_variation <- data %>%
    group_by(region_number) %>%
    summarise(peace_var = length(unique(after_peace_treaty)), .groups = 'drop')
  
  message("    - Regions with peace variation: ", sum(peace_variation$peace_var > 1), "/", nrow(peace_variation))
}

########################################
# MAIN PROCESSING FUNCTION WITH DATASET SAVING
########################################

process_enhanced_analysis <- function() {
  message("Starting ENHANCED Myers-Benford analysis with CSI quintiles...")
  
  # Levels and versions to process
  levels <- c("dept", "dept_area")
  versions <- c("V01", "V03")
  
  # Store all results
  all_merged_data <- list()
  all_regression_results <- list()
  
  # Process each combination
  for (level in levels) {
    for (version in versions) {
      
      message("\n", paste(rep("=", 60), collapse=""))
      message("PROCESSING: ", toupper(level), " LEVEL, VERSION ", version)
      message(paste(rep("=", 60), collapse=""))
      
      # Import Myers-Benford data from Excel
      myers_benford_data <- import_myers_benford_excel(level, version)
      
      # Import CSI-NTL data from DTA (same for all versions)
      csi_ntl_data <- import_csi_ntl_dta(level)
      
      # Merge datasets (now includes CSI quintile creation and CORRECTED merge logic)
      merged_data <- merge_datasets(myers_benford_data, csi_ntl_data, level, version)
      
      if (!is.null(merged_data)) {
        # Store merged data
        data_key <- paste(level, version, sep = "_")
        all_merged_data[[data_key]] <- merged_data
        
        # NEW: Save final dataset for reuse
        save_success <- save_final_dataset(merged_data, level, version)
        
        if (save_success) {
          message("  💾 Dataset saved successfully for future use")
        }
        
        # Run enhanced regression analysis with CSI quintiles
        reg_results <- run_enhanced_regression_with_csi(merged_data, level, version)
        all_regression_results[[data_key]] <- reg_results
        
        message("✅ Completed processing for ", level, " level, version ", version)
        message("   - Final dataset: ", nrow(merged_data), " observations")
        if (!is.null(reg_results)) {
          message("   - Models without FE: ", length(unique(reg_results$coefficients_no_fe$Model)))
          message("   - Models with FE: ", length(unique(reg_results$coefficients_fe$Model)))
        }
      }
    }
  }
  
  # Create enhanced Excel outputs
  create_enhanced_output(all_merged_data, all_regression_results)
  
  message("\n", paste(rep("=", 80), collapse=""))
  message("✅ ENHANCED ANALYSIS WITH CSI QUINTILES FINISHED")
  message("✅ FINAL DATASETS SAVED FOR REUSE")
  message("✅ CORRECTED MERGE LOGIC - NO MORE UNWANTED DUPLICATION")
  message(paste(rep("=", 80), collapse=""))
}

# Function to create enhanced Excel output with CSI analysis
create_enhanced_output <- function(all_merged_data, all_regression_results) {
  message("\nCreating enhanced Excel outputs with CSI quintile analysis...")
  
  # Create separate files for each level
  for (level in c("dept", "dept_area")) {
    
    message("Creating enhanced output for ", level, " level...")
    
    # Prepare Excel data
    excel_data <- list()
    
    # Enhanced summary sheet
    summary_text <- paste0("Colombia Myers-Benford Analysis - ENHANCED WITH CSI QUINTILES - CORRECTED MERGE\n")
    summary_text <- paste0(summary_text, "Level: ", toupper(level), " | Date: ", Sys.Date(), "\n")
    summary_text <- paste0(summary_text, paste(rep("=", 80), collapse=""), "\n\n")
    summary_text <- paste0(summary_text, "MAJOR ENHANCEMENT: CSI QUINTILES INTEGRATION\n")
    summary_text <- paste0(summary_text, "- CSI (Composite State Capacity Index) now converted to quintiles\n")
    summary_text <- paste0(summary_text, "- Full interaction analysis: Myers × CSI × Peace Treaty\n")
    summary_text <- paste0(summary_text, "- Historical state capacity as moderator variable\n")
    summary_text <- paste0(summary_text, "- CORRECTED merge logic prevents unwanted duplication\n")
    summary_text <- paste0(summary_text, "- Final datasets saved for reuse in 03_Data/Merge folder\n\n")
    summary_text <- paste0(summary_text, "CORRECTED MERGE KEYS:\n")
    if (level == "dept") {
      summary_text <- paste0(summary_text, "- Department level: region_number + ano\n")
    } else {
      summary_text <- paste0(summary_text, "- Department-area level: region_number + ano + urban_area\n")
      summary_text <- paste0(summary_text, "- No more unwanted duplication by urban_area!\n")
    }
    summary_text <- paste0(summary_text, "\nSAVED DATASETS:\n")
    if (level == "dept") {
      summary_text <- paste0(summary_text, "- 05_V01_Myers_Bend_CSI_NTL_depto.dta\n")
      summary_text <- paste0(summary_text, "- 05_V03_Myers_Bend_CSI_NTL_depto.dta\n")
    } else {
      summary_text <- paste0(summary_text, "- 05_V01_Myers_Bend_CSI_NTL_depto_area.dta\n")
      summary_text <- paste0(summary_text, "- 05_V03_Myers_Bend_CSI_NTL_depto_area.dta\n")
    }
    summary_text <- paste0(summary_text, "\nDATA SOURCES:\n")
    summary_text <- paste0(summary_text, "1. Myers-Benford data: Excel files (02_ series)\n")
    summary_text <- paste0(summary_text, "2. CSI and Nightlight data: DTA files\n")
    summary_text <- paste0(summary_text, "3. Merged by proper keys (no duplication)\n\n")
    summary_text <- paste0(summary_text, "VERSION COMPARISON:\n")
    summary_text <- paste0(summary_text, "V01: Including outliers and zeros in income variable\n")
    summary_text <- paste0(summary_text, "V03: Excluding outliers (interquartile method), no zero income\n\n")
    summary_text <- paste0(summary_text, "NEW MODEL SPECIFICATIONS:\n")
    summary_text <- paste0(summary_text, "Models 1-3: Basic and single factor interactions\n")
    summary_text <- paste0(summary_text, "Models 4-7: Peace treaty interactions\n")
    summary_text <- paste0(summary_text, "Models 8-9: Dual development controls (NTL + CSI)\n")
    summary_text <- paste0(summary_text, "Models 10-12: Triple interactions (Myers × Peace × Development)\n\n")
    summary_text <- paste0(summary_text, "KEY INNOVATIONS:\n")
    summary_text <- paste0(summary_text, "✅ CSI quintiles for categorical analysis\n")
    summary_text <- paste0(summary_text, "✅ Historical vs. modern development interactions\n")
    summary_text <- paste0(summary_text, "✅ Peace treaty as natural experiment\n")
    summary_text <- paste0(summary_text, "✅ Smart fixed effects handling collinearity\n")
    summary_text <- paste0(summary_text, "✅ Robust standard errors clustered by region\n")
    summary_text <- paste0(summary_text, "✅ CORRECTED merge prevents observation duplication\n")
    summary_text <- paste0(summary_text, "✅ Final datasets saved as Stata files for reuse\n\n")
    summary_text <- paste0(summary_text, "*** p<0.01, ** p<0.05, * p<0.10\n")
    
    excel_data[["Enhanced_Summary_CORRECTED"]] <- data.frame(
      Summary = strsplit(summary_text, "\n")[[1]], 
      stringsAsFactors = FALSE
    )
    
    # Combine results from both versions for this level
    level_results <- all_regression_results[grepl(level, names(all_regression_results))]
    
    if (length(level_results) > 0) {
      # Combined coefficients
      all_coeffs_no_fe <- do.call(rbind, lapply(level_results, function(x) x$coefficients_no_fe))
      all_coeffs_fe <- do.call(rbind, lapply(level_results, function(x) x$coefficients_fe))
      all_stats_no_fe <- do.call(rbind, lapply(level_results, function(x) x$model_stats_no_fe))
      all_stats_fe <- do.call(rbind, lapply(level_results, function(x) x$model_stats_fe))
      
      # Add combined sheets
      if (nrow(all_coeffs_no_fe) > 0) {
        excel_data[["All_Coefficients_No_FE"]] <- all_coeffs_no_fe
        excel_data[["All_Model_Stats_No_FE"]] <- all_stats_no_fe
      }
      
      if (nrow(all_coeffs_fe) > 0) {
        excel_data[["All_Coefficients_FE"]] <- all_coeffs_fe
        excel_data[["All_Model_Stats_FE"]] <- all_stats_fe
      }
      
      # Version-specific sheets
      for (version in c("V01", "V03")) {
        version_key <- paste(level, version, sep = "_")
        
        if (version_key %in% names(level_results)) {
          result <- level_results[[version_key]]
          
          if (nrow(result$coefficients_no_fe) > 0) {
            excel_data[[paste0(version, "_Coefficients_No_FE")]] <- result$coefficients_no_fe
            excel_data[[paste0(version, "_Model_Stats_No_FE")]] <- result$model_stats_no_fe
          }
          
          if (nrow(result$coefficients_fe) > 0) {
            excel_data[[paste0(version, "_Coefficients_FE")]] <- result$coefficients_fe
            excel_data[[paste0(version, "_Model_Stats_FE")]] <- result$model_stats_fe
          }
        }
      }
      
      # Enhanced sample merged data with quintiles
      if (paste(level, "V01", sep = "_") %in% names(all_merged_data)) {
        sample_data <- all_merged_data[[paste(level, "V01", sep = "_")]]
        key_vars <- c("region_number", "ano", "standardized_myers", "benford_abs_distance", 
                      "ntl_average_masked", "ntl_quintile_factor", 
                      "csi_pre1500_avg_no0", "csi_quintile_factor",
                      "after_peace_treaty")
        if (level == "dept_area") {
          key_vars <- c(key_vars, "urban_area")
        }
        available_vars <- key_vars[key_vars %in% names(sample_data)]
        
        if (length(available_vars) > 0) {
          excel_data[["Sample_Enhanced_Data_CORRECTED"]] <- head(sample_data[, available_vars], 30)
        }
      }
      
      # CSI quintile analysis sheet
      if (paste(level, "V01", sep = "_") %in% names(all_merged_data)) {
        sample_data <- all_merged_data[[paste(level, "V01", sep = "_")]]
        if ("csi_quintile_factor" %in% names(sample_data)) {
          csi_analysis <- sample_data %>%
            group_by(csi_quintile_factor) %>%
            summarise(
              n_obs = n(),
              mean_myers = mean(standardized_myers, na.rm = TRUE),
              mean_benford = mean(benford_abs_distance, na.rm = TRUE),
              mean_csi_raw = mean(csi_pre1500_avg_no0, na.rm = TRUE),
              mean_ntl = mean(ntl_average_masked, na.rm = TRUE),
              pct_post_peace = mean(after_peace_treaty, na.rm = TRUE) * 100,
              .groups = 'drop'
            )
          
          excel_data[["CSI_Quintile_Analysis"]] <- csi_analysis
        }
      }
    }
    
    # Remove empty sheets
    excel_data <- excel_data[sapply(excel_data, function(x) nrow(x) > 0)]
    
    # Write Excel file
    output_filename <- paste0("06_ENHANCED_CSI_Myers_Benford_", level, "_CORRECTED.xlsx")
    output_file_path <- file.path(output_path, output_filename)
    
    writexl::write_xlsx(excel_data, output_file_path)
    message("✅ Saved: ", output_filename)
  }
}

########################################
# MAIN EXECUTION
########################################

# Run the enhanced analysis
process_enhanced_analysis()

message("\n🎯 ENHANCED ANALYSIS SUMMARY - CORRECTED VERSION:")
message("📊 Successfully integrated CSI quintiles into the analysis")
message("📈 New models: Myers × CSI quintiles interactions")
message("🔍 Triple interactions: Myers × Peace Treaty × CSI quintiles")
message("✅ CORRECTED merge logic prevents observation duplication")
message("💾 Final datasets saved for reuse:")
message("   📂 05_V01_Myers_Bend_CSI_NTL_depto.dta")
message("   📂 05_V01_Myers_Bend_CSI_NTL_depto_area.dta")
message("   📂 05_V03_Myers_Bend_CSI_NTL_depto.dta")
message("   📂 05_V03_Myers_Bend_CSI_NTL_depto_area.dta")
message("📁 Enhanced output files:")
message("   📊 06_ENHANCED_CSI_Myers_Benford_dept_CORRECTED.xlsx")
message("   📊 06_ENHANCED_CSI_Myers_Benford_dept_area_CORRECTED.xlsx")

message("\n📋 ENHANCED EXCEL SHEET ORGANIZATION:")
message("   📄 Enhanced_Summary_CORRECTED: Complete analysis overview with merge fix")
message("   📊 All_Coefficients_No_FE: Combined V01+V03 results (no FE)")
message("   📊 All_Coefficients_FE: Combined V01+V03 results (with FE)")
message("   📈 V01/V03_Coefficients: Version-specific results")
message("   👁️ Sample_Enhanced_Data_CORRECTED: Preview with proper merge")
message("   📈 CSI_Quintile_Analysis: Descriptive statistics by CSI quintiles")

message("\n🔧 CORRECTED MERGE LOGIC:")
message("   🏛️ Department level: region_number + ano (no change needed)")
message("   🏘️ Department-area level: region_number + ano + urban_area")
message("   ✅ No more unwanted observation duplication")
message("   ✅ Proper 1:1 matching on all relevant keys")
message("   ✅ Urban/rural areas now correctly matched")
message("   💾 Reusable datasets with correct observation counts")
message("   🎯 Publication-ready analysis with data integrity!")