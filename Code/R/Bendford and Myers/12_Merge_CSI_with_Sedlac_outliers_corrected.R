########################################
# CSI Integration with SEDLAC Colombia Analysis V2.0 - CORRECTED
########################################
# Author: Luis Castellanos Rodriguez
# Date: 2025-08-16
# Purpose: Integrate Caloric Suitability Indices with Multi-Version Myers-Benford analysis
# 
# This script:
# 1. Reads Excel outputs with "03_" prefix from regression analysis
# 2. Combines V01 and V03 versions into single datasets
# 3. Downloads and processes CSI .tif files (same as V1)
# 4. Extracts CSI values by Colombian departments
# 5. Merges CSI data with combined multi-version analysis
# 6. Saves integrated datasets as .dta files with "02_" prefix

########################################
# 0. SETUP AND CONFIGURATION
########################################

# Clean workspace
rm(list = ls())
gc()

# Install and load required packages
packages <- c("readxl", "haven", "dplyr", "sf", "terra", "exactextractr", 
              "geodata", "stringr", "tidyverse", "data.table")

# Check and install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}

# Load all packages
lapply(packages, library, character.only = TRUE)

########################################
# 1. FILE PATHS CONFIGURATION
########################################

# Input paths
excel_input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col"
csi_input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/CSI/CaloricSuitabilityIndex"

# Output path
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge"

# Create output directory if it doesn't exist
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# CSI file names (same as V1)
csi_files <- list(
  "pre1500_avg_no0" = "pre1500AverageCaloriesNo0.tif",
  "pre1500_max_no0" = "pre1500OptCaloriesNo0.tif",
  "post1500_avg_no0" = "post1500AverageCaloriesNo0.tif",
  "post1500_max_no0" = "post1500OptCaloriesNo0.tif"
)

# Display configuration
message("=== CSI Integration V2.0 Configuration ===")
message("Excel input path: ", excel_input_path)
message("CSI input path: ", csi_input_path)
message("Output path: ", output_path)
message("Strategy: Multi-version integration (auto-detect versions)")
message("==========================================")

########################################
# 2. HELPER FUNCTIONS
########################################

# Function to create department mapping (same as V1)
create_colombia_dept_mapping <- function() {
  dept_mapping <- data.frame(
    region_number = c(5, 8, 11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 
                      41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73, 76),
    region_corrected = c("Antioquia", "Atlántico", "Bogotá D.C.", "Bolívar", 
                         "Boyacá", "Caldas", "Caquetá", "Cauca", "Cesar", 
                         "Córdoba", "Cundinamarca", "Chocó", "Huila", 
                         "La Guajira", "Magdalena", "Meta", "Nariño", 
                         "Norte de Santander", "Quindío", "Risaralda", 
                         "Santander", "Sucre", "Tolima", "Valle"),
    gadm_name = c("Antioquia", "Atlántico", "Bogotá D.C.", "Bolívar",
                  "Boyacá", "Caldas", "Caquetá", "Cauca", "Cesar",
                  "Córdoba", "Cundinamarca", "Chocó", "Huila",
                  "La Guajira", "Magdalena", "Meta", "Nariño",
                  "Norte de Santander", "Quindío", "Risaralda",
                  "Santander", "Sucre", "Tolima", "Valle del Cauca"),
    stringsAsFactors = FALSE
  )
  return(dept_mapping)
}

# Function to find multi-version Excel files (auto-detect versions)
find_multiversion_excel_files <- function(path) {
  # Pattern to match: 03_Myers_Benford_Col_V##_reg-{tot|area}.xlsx
  pattern <- "^03_Myers_Benford_Col_V\\d+_reg-(tot|area)\\.xlsx$"
  
  # Get all Excel files in directory
  all_files <- list.files(path, pattern = "\\.xlsx$", full.names = FALSE)
  
  # Filter to match our pattern
  matching_files <- all_files[grepl(pattern, all_files)]
  
  if (length(matching_files) == 0) {
    message("No files matching pattern found in: ", path)
    message("Looking for files like: 03_Myers_Benford_Col_V##_reg-{tot|area}.xlsx")
    message("Available Excel files:")
    if (length(all_files) > 0) {
      for (file in all_files) message("  ", file)
    } else {
      message("  (no Excel files found)")
    }
    stop("No matching Myers-Benford files found with '03_' prefix.")
  }
  
  # Create list with full paths
  found_files <- list()
  for (file in matching_files) {
    found_files[[file]] <- file.path(path, file)
    message("  FOUND: ", file)
  }
  
  # Extract and report detected versions and levels
  versions <- unique(gsub(".*_V(\\d+)_.*", "\\1", matching_files))
  levels <- unique(gsub(".*_reg-(tot|area)\\.xlsx", "\\1", matching_files))
  
  message("  Detected versions: ", paste0("V", versions, collapse = ", "))
  message("  Detected levels: ", paste(levels, collapse = ", "))
  
  # Check if we have both levels for each version
  for (version in versions) {
    version_files <- matching_files[grepl(paste0("_V", version, "_"), matching_files)]
    has_tot <- any(grepl("reg-tot", version_files))
    has_area <- any(grepl("reg-area", version_files))
    
    if (!has_tot || !has_area) {
      message("  WARNING: Version V", version, " missing some levels (tot: ", has_tot, ", area: ", has_area, ")")
    } else {
      message("  ✓ Version V", version, " complete (both reg-tot and reg-area)")
    }
  }
  
  return(found_files)
}

# Function to extract version and level from filename
extract_file_metadata <- function(filename) {
  # Remove path and extension
  base_name <- basename(gsub("\\.xlsx$", "", filename))
  
  # Split by underscore: "03_Myers_Benford_Col_V01_reg-tot"
  parts <- strsplit(base_name, "_")[[1]]
  
  # Extract version (e.g., "V01", "V02")
  version <- parts[length(parts) - 1]
  
  # Extract level (e.g., "reg-tot", "reg-area")
  level <- parts[length(parts)]
  
  return(list(version = version, level = level))
}

# Function to read single Excel file and add metadata
read_excel_with_metadata <- function(file_path) {
  metadata <- extract_file_metadata(file_path)
  
  message("  Reading: ", basename(file_path), " (", metadata$version, ", ", metadata$level, ")")
  
  tryCatch({
    # Read the Results sheet
    data <- readxl::read_excel(file_path, sheet = "Results")
    
    if (nrow(data) == 0) {
      message("    WARNING: No data in Results sheet")
      return(NULL)
    }
    
    # Add metadata columns
    data$version <- metadata$version
    data$level <- metadata$level
    data$file_source <- basename(file_path)
    
    message("    Loaded: ", nrow(data), " observations")
    return(data)
    
  }, error = function(e) {
    message("    ERROR reading file: ", e$message)
    return(NULL)
  })
}

# Function to create version-specific variable names
create_version_variables <- function(data, version, variables_to_version) {
  versioned_data <- data
  
  for (var in variables_to_version) {
    if (var %in% names(data)) {
      # Create new versioned variable name
      new_var_name <- paste0(var, "_", version)
      versioned_data[[new_var_name]] <- data[[var]]
      
      # Remove original variable (will be replaced by versioned ones)
      versioned_data[[var]] <- NULL
    }
  }
  
  return(versioned_data)
}

# Function to combine multi-version data by level
combine_multiversion_data <- function(data_list, level_filter) {
  # Filter data by level
  level_data <- data_list[sapply(data_list, function(x) !is.null(x) && x$level[1] == level_filter)]
  
  if (length(level_data) == 0) {
    message("  No data found for level: ", level_filter)
    return(data.frame())
  }
  
  message("  Combining ", length(level_data), " datasets for level: ", level_filter)
  
  # Define variables that should be versioned (Myers-Benford specific)
  variables_to_version <- c(
    "n_observations_benford_with_zeros",
    "n_observations_benford_no_zeros",
    "benford_abs_distance_with_zeros",
    "benford_chi_square_with_zeros", 
    "benford_abs_distance_no_zeros",
    "benford_chi_square_no_zeros",
    "n_observations_myers",
    "traditional_myers",
    "standardized_myers",
    "residual_standardized_myers",
    "residual_benford_abs_distance"
  )
  
  # Variables that should NOT be versioned (geographic, time, demographic)
  common_variables <- c(
    "region_number",
    "region_corrected", 
    "ano",
    "urban_area",
    "urban_share"
  )
  
  # Process each dataset to create versioned variables
  processed_datasets <- list()
  
  for (i in seq_along(level_data)) {
    dataset <- level_data[[i]]
    version <- dataset$version[1]
    
    message("    Processing version: ", version)
    
    # Create versioned variables
    versioned_dataset <- create_version_variables(dataset, version, variables_to_version)
    
    # Keep only common variables + versioned variables + metadata
    keep_vars <- c(common_variables, 
                   names(versioned_dataset)[grepl(paste0("_", version, "$"), names(versioned_dataset))],
                   "version", "level", "file_source")
    
    # Filter to existing variables
    keep_vars <- keep_vars[keep_vars %in% names(versioned_dataset)]
    versioned_dataset <- versioned_dataset[, keep_vars, drop = FALSE]
    
    processed_datasets[[version]] <- versioned_dataset
  }
  
  # Determine merge key based on level
  if (level_filter == "reg-area") {
    merge_key <- c("region_number", "ano", "urban_area")
  } else {
    merge_key <- c("region_number", "ano")
  }
  
  message("    Merge key: ", paste(merge_key, collapse = " + "))
  
  # Start with first dataset
  combined_data <- processed_datasets[[1]]
  
  # Merge additional datasets
  if (length(processed_datasets) > 1) {
    for (i in 2:length(processed_datasets)) {
      dataset_to_merge <- processed_datasets[[i]]
      
      # Get variables to merge (exclude common merge key variables)
      vars_to_merge <- setdiff(names(dataset_to_merge), 
                               c(merge_key, "version", "level", "file_source"))
      merge_vars <- c(merge_key, vars_to_merge)
      
      combined_data <- merge(combined_data, 
                             dataset_to_merge[, merge_vars, drop = FALSE], 
                             by = merge_key, 
                             all = TRUE)
    }
  }
  
  # Add level identifier
  combined_data$analysis_level <- level_filter
  
  message("    Combined result: ", nrow(combined_data), " observations, ", ncol(combined_data), " variables")
  
  return(combined_data)
}

# Function to clean up merge artifacts and prepare for Stata
clean_merge_artifacts <- function(data) {
  message("  Cleaning merge artifacts...")
  
  # Get all variable names
  all_vars <- names(data)
  
  # Find variables with .x and .y suffixes
  x_vars <- all_vars[grepl("\\.x$", all_vars)]
  y_vars <- all_vars[grepl("\\.y$", all_vars)]
  
  # Handle .x variables (keep original name, remove .x suffix)
  for (x_var in x_vars) {
    base_name <- gsub("\\.x$", "", x_var)
    
    # Check if there's a corresponding .y variable
    y_var <- paste0(base_name, ".y")
    
    if (y_var %in% names(data)) {
      # Compare the two variables to see if they're identical
      if (all(data[[x_var]] == data[[y_var]], na.rm = TRUE) || 
          (all(is.na(data[[x_var]])) && all(is.na(data[[y_var]])))) {
        # If identical, keep the .x version and rename it
        data[[base_name]] <- data[[x_var]]
        data[[x_var]] <- NULL
        data[[y_var]] <- NULL
        message("    Resolved duplicate: ", base_name, " (identical values)")
      } else {
        # If different, keep the .x version (original data) and rename it
        data[[base_name]] <- data[[x_var]]
        data[[x_var]] <- NULL
        data[[y_var]] <- NULL
        message("    Resolved duplicate: ", base_name, " (kept .x version)")
      }
    } else {
      # No .y variable, just rename .x to base name
      data[[base_name]] <- data[[x_var]]
      data[[x_var]] <- NULL
      message("    Renamed: ", x_var, " → ", base_name)
    }
  }
  
  # Handle any remaining .y variables (shouldn't happen if above works correctly)
  remaining_y_vars <- names(data)[grepl("\\.y$", names(data))]
  for (y_var in remaining_y_vars) {
    base_name <- gsub("\\.y$", "", y_var)
    if (!base_name %in% names(data)) {
      data[[base_name]] <- data[[y_var]]
      data[[y_var]] <- NULL
      message("    Renamed remaining: ", y_var, " → ", base_name)
    } else {
      # Base name already exists, remove the .y version
      data[[y_var]] <- NULL
      message("    Removed duplicate: ", y_var)
    }
  }
  
  # Check for any remaining problematic characters in variable names
  problematic_vars <- names(data)[grepl("[^a-zA-Z0-9_]", names(data))]
  if (length(problematic_vars) > 0) {
    message("    WARNING: Variables with problematic characters: ", paste(problematic_vars, collapse = ", "))
    
    # Clean up problematic characters
    for (prob_var in problematic_vars) {
      new_name <- gsub("[^a-zA-Z0-9_]", "_", prob_var)
      if (new_name != prob_var && !new_name %in% names(data)) {
        names(data)[names(data) == prob_var] <- new_name
        message("    Cleaned: ", prob_var, " → ", new_name)
      }
    }
  }
  
  return(data)
}

# Function to shorten variable names for Stata compatibility (dynamic versions)
shorten_variable_names_v2 <- function(data) {
  # Get all variable names
  all_vars <- names(data)
  
  # Find all version suffixes in the data (e.g., _V01, _V02, _V03, etc.)
  version_pattern <- "_V\\d+$"
  versioned_vars <- all_vars[grepl(version_pattern, all_vars)]
  
  # Extract unique versions
  versions <- unique(gsub(".*(_V\\d+)$", "\\1", versioned_vars))
  
  message("Detected versions in data: ", paste(versions, collapse = ", "))
  
  # Create dynamic mapping for each version
  name_mapping <- c()
  
  for (version in versions) {
    # Define the base variable names and their shortened versions
    base_vars <- c(
      "n_observations_benford_with_zeros" = "n_obs_benf_w_zero",
      "n_observations_benford_no_zeros" = "n_obs_benf_no_zero",
      "benford_abs_distance_with_zeros" = "benf_abs_dist_w_zero",
      "benford_chi_square_with_zeros" = "benf_chi2_w_zero",
      "benford_abs_distance_no_zeros" = "benf_abs_dist_no_zero",
      "benford_chi_square_no_zeros" = "benf_chi2_no_zero",
      "n_observations_myers" = "n_obs_myers",
      "traditional_myers" = "myers_trad",
      "standardized_myers" = "myers_std",
      "residual_standardized_myers" = "resid_myers_std",
      "residual_benford_abs_distance" = "resid_benf_abs_dist"
    )
    
    # Create versioned mappings
    for (i in seq_along(base_vars)) {
      old_name <- paste0(names(base_vars)[i], version)
      new_name <- paste0(base_vars[i], version)
      name_mapping[old_name] <- new_name
    }
  }
  
  # Apply name mapping
  for (old_name in names(name_mapping)) {
    if (old_name %in% names(data)) {
      names(data)[names(data) == old_name] <- name_mapping[old_name]
      message("  Renamed: ", old_name, " → ", name_mapping[old_name])
    }
  }
  
  # Check for any remaining long names (>32 chars), excluding key variables we want to preserve
  preserve_vars <- c("region_corrected", "region_number", "analysis_level")
  long_names <- names(data)[nchar(names(data)) > 32 & !(names(data) %in% preserve_vars)]
  if (length(long_names) > 0) {
    message("WARNING: Still have long variable names: ", paste(long_names, collapse = ", "))
  }
  
  # Verify region_corrected is preserved
  if (!"region_corrected" %in% names(data)) {
    message("ERROR: region_corrected variable was lost during processing!")
  } else {
    message("✓ region_corrected variable preserved")
  }
  
  return(data)
}

# Functions for CSI processing (same as V1)
get_colombia_boundaries <- function() {
  message("Downloading Colombian department boundaries...")
  
  tryCatch({
    # Download from GADM
    colombia_admin <- geodata::gadm(country = "COL", level = 1, path = tempdir())
    colombia_sf <- sf::st_as_sf(colombia_admin)
    
    # Get department mapping
    dept_mapping <- create_colombia_dept_mapping()
    
    # Merge with mapping (handle potential name mismatches)
    colombia_sf <- merge(colombia_sf, dept_mapping, 
                         by.x = "NAME_1", by.y = "gadm_name", all.x = TRUE)
    
    # Check for unmapped departments
    unmapped <- colombia_sf[is.na(colombia_sf$region_number), ]
    if (nrow(unmapped) > 0) {
      message("WARNING: Unmapped departments found:")
      print(unmapped$NAME_1)
    }
    
    # Remove unmapped departments
    colombia_sf <- colombia_sf[!is.na(colombia_sf$region_number), ]
    
    message("Successfully loaded ", nrow(colombia_sf), " departments")
    return(colombia_sf)
    
  }, error = function(e) {
    message("ERROR downloading boundaries: ", e$message)
    stop("Could not download Colombian boundaries. Check internet connection.")
  })
}

extract_csi_by_department <- function(csi_file_path, colombia_sf, csi_name) {
  message("  Extracting ", csi_name, " from ", basename(csi_file_path))
  
  tryCatch({
    # Load the CSI raster
    csi_raster <- terra::rast(csi_file_path)
    message("    Raster loaded: ", terra::ncell(csi_raster), " cells")
    
    # Ensure same CRS
    colombia_sf_proj <- sf::st_transform(colombia_sf, terra::crs(csi_raster))
    
    # Extract mean values by department
    extracted_values <- exactextractr::exact_extract(
      csi_raster, 
      colombia_sf_proj, 
      "mean", 
      progress = FALSE
    )
    
    # Create results dataframe
    csi_results <- data.frame(
      region_number = colombia_sf$region_number,
      region_corrected = colombia_sf$region_corrected,
      csi_value = extracted_values,
      stringsAsFactors = FALSE
    )
    
    # Rename the CSI column
    names(csi_results)[3] <- paste0("csi_", csi_name)
    
    # Check for missing values
    missing_count <- sum(is.na(csi_results[[paste0("csi_", csi_name)]]))
    if (missing_count > 0) {
      message("    WARNING: ", missing_count, " departments have missing CSI values")
    }
    
    message("    Successfully extracted CSI for ", nrow(csi_results), " departments")
    return(csi_results)
    
  }, error = function(e) {
    message("    ERROR extracting ", csi_name, ": ", e$message)
    return(data.frame(
      region_number = colombia_sf$region_number,
      region_corrected = colombia_sf$region_corrected,
      csi_value = NA,
      stringsAsFactors = FALSE
    ))
  })
}

########################################
# 3. MAIN EXECUTION
########################################

message("\n=== Starting Multi-Version CSI Integration Process ===")

# Step 1: Find and read multi-version Excel files
message("\n1. Reading multi-version Excel files...")
excel_files <- find_multiversion_excel_files(excel_input_path)

# Read all Excel files
all_data <- list()
for (file_key in names(excel_files)) {
  file_path <- excel_files[[file_key]]
  data <- read_excel_with_metadata(file_path)
  if (!is.null(data)) {
    all_data[[file_key]] <- data
  }
}

if (length(all_data) == 0) {
  stop("No data could be loaded from Excel files")
}

message("Successfully loaded data from ", length(all_data), " files")

# Step 2: Combine data by level (reg-tot and reg-area)
message("\n2. Combining multi-version data...")

reg_tot_combined <- combine_multiversion_data(all_data, "reg-tot")
reg_area_combined <- combine_multiversion_data(all_data, "reg-area")

# Step 3: Get Colombian boundaries (same as V1)
message("\n3. Getting Colombian department boundaries...")
colombia_sf <- get_colombia_boundaries()

# Step 4: Check CSI files exist (same as V1)
message("\n4. Checking CSI files...")
missing_csi_files <- c()
for (csi_name in names(csi_files)) {
  csi_file_path <- file.path(csi_input_path, csi_files[[csi_name]])
  if (!file.exists(csi_file_path)) {
    missing_csi_files <- c(missing_csi_files, csi_files[[csi_name]])
    message("  MISSING: ", csi_files[[csi_name]])
  } else {
    message("  FOUND: ", csi_files[[csi_name]])
  }
}

if (length(missing_csi_files) > 0) {
  stop("Missing CSI files: ", paste(missing_csi_files, collapse = ", "), 
       ". Please check the CSI data path.")
}

# Step 5: Extract CSI data (same as V1)
message("\n5. Extracting CSI data by department...")
all_csi_data <- colombia_sf[, c("region_number", "region_corrected")]
all_csi_data <- sf::st_drop_geometry(all_csi_data)

for (csi_name in names(csi_files)) {
  csi_file_path <- file.path(csi_input_path, csi_files[[csi_name]])
  csi_data <- extract_csi_by_department(csi_file_path, colombia_sf, csi_name)
  
  # Merge with all_csi_data
  all_csi_data <- merge(all_csi_data, 
                        csi_data[, c("region_number", paste0("csi_", csi_name))],
                        by = "region_number", all.x = TRUE)
}

message("CSI extraction complete. Summary:")
print(summary(all_csi_data[, grep("csi_", names(all_csi_data))]))

# Step 6: Merge CSI with multi-version data
message("\n6. Merging CSI data with multi-version analysis...")

# Merge with regional level data
if (nrow(reg_tot_combined) > 0) {
  message("  Merging with regional level data...")
  reg_tot_merged <- merge(reg_tot_combined, all_csi_data, 
                          by = "region_number", all.x = TRUE)
  
  # Check merge success
  n_matched <- sum(!is.na(reg_tot_merged$csi_pre1500_avg_no0))
  message("    Successfully matched CSI for ", n_matched, "/", nrow(reg_tot_merged), " observations")
} else {
  reg_tot_merged <- data.frame()
}

# Merge with regional-area level data
if (nrow(reg_area_combined) > 0) {
  message("  Merging with regional-area level data...")
  reg_area_merged <- merge(reg_area_combined, all_csi_data, 
                           by = "region_number", all.x = TRUE)
  
  # Check merge success
  n_matched <- sum(!is.na(reg_area_merged$csi_pre1500_avg_no0))
  message("    Successfully matched CSI for ", n_matched, "/", nrow(reg_area_merged), " observations")
} else {
  reg_area_merged <- data.frame()
}

# Step 7: Save as .dta files with "02_" prefix
message("\n7. Saving integrated multi-version datasets...")

# Save regional level
if (nrow(reg_tot_merged) > 0) {
  # Clean merge artifacts first
  reg_tot_cleaned <- clean_merge_artifacts(reg_tot_merged)
  
  # Shorten variable names for Stata compatibility
  reg_tot_final <- shorten_variable_names_v2(reg_tot_cleaned)
  
  dept_file <- file.path(output_path, "02_col_merge_sedlac_CSI_dept.dta")
  haven::write_dta(reg_tot_final, dept_file)
  message("  Saved regional level: ", dept_file)
  message("    Observations: ", nrow(reg_tot_final))
  message("    Variables: ", ncol(reg_tot_final))
  
  # Display sample of key variables
  # Dynamically find available versioned variables
  all_vars <- names(reg_tot_final)
  myers_vars <- all_vars[grepl("myers_std_V\\d+", all_vars)]
  benford_vars <- all_vars[grepl("benf_abs_dist_no_zero_V\\d+", all_vars)]
  
  key_vars <- c("region_number", "region_corrected", "ano")
  key_vars <- c(key_vars, myers_vars[1:min(2, length(myers_vars))])  # Show up to 2 Myers versions
  key_vars <- c(key_vars, benford_vars[1:min(2, length(benford_vars))])  # Show up to 2 Benford versions  
  key_vars <- c(key_vars, "csi_pre1500_avg_no0", "csi_post1500_max_no0")
  
  available_vars <- key_vars[key_vars %in% names(reg_tot_final)]
  message("    Sample data (key variables):")
  if(length(available_vars) > 0) {
    print(head(reg_tot_final[, available_vars]))
  } else {
    message("    No key variables found - showing first 6 columns:")
    print(head(reg_tot_final[, 1:min(6, ncol(reg_tot_final))]))
  }
}

# Save regional-area level
if (nrow(reg_area_merged) > 0) {
  # Clean merge artifacts first
  reg_area_cleaned <- clean_merge_artifacts(reg_area_merged)
  
  # Shorten variable names for Stata compatibility
  reg_area_final <- shorten_variable_names_v2(reg_area_cleaned)
  
  dept_area_file <- file.path(output_path, "02_col_merge_sedlac_CSI_dept_area.dta")
  haven::write_dta(reg_area_final, dept_area_file)
  message("  Saved regional-area level: ", dept_area_file)
  message("    Observations: ", nrow(reg_area_final))
  message("    Variables: ", ncol(reg_area_final))
  
  # Display sample of key variables
  # Dynamically find available versioned variables
  all_vars <- names(reg_area_final)
  myers_vars <- all_vars[grepl("myers_std_V\\d+", all_vars)]
  benford_vars <- all_vars[grepl("benf_abs_dist_no_zero_V\\d+", all_vars)]
  
  key_vars <- c("region_number", "region_corrected", "ano", "urban_area")
  key_vars <- c(key_vars, myers_vars[1:min(2, length(myers_vars))])  # Show up to 2 Myers versions
  key_vars <- c(key_vars, benford_vars[1:min(2, length(benford_vars))])  # Show up to 2 Benford versions
  key_vars <- c(key_vars, "csi_pre1500_avg_no0", "csi_post1500_max_no0")
  
  available_vars <- key_vars[key_vars %in% names(reg_area_final)]
  message("    Sample data (key variables):")
  if(length(available_vars) > 0) {
    print(head(reg_area_final[, available_vars]))
  } else {
    message("    No key variables found - showing first 6 columns:")
    print(head(reg_area_final[, 1:min(6, ncol(reg_area_final))]))
  }
}

########################################
# 8. FINAL SUMMARY AND VALIDATION
########################################

message("\n=== Multi-Version CSI Integration Summary ===")
message("✅ Process completed successfully")
message("\nFiles created:")
if (nrow(reg_tot_merged) > 0 && exists("reg_tot_final")) {
  message("  📄 02_col_merge_sedlac_CSI_dept.dta (", nrow(reg_tot_final), " observations)")
  message("      Multi-version variables: Auto-detected versions for all Myers-Benford indices")
  message("      ✓ region_corrected preserved: ", "region_corrected" %in% names(reg_tot_final))
}
if (nrow(reg_area_merged) > 0 && exists("reg_area_final")) {
  message("  📄 02_col_merge_sedlac_CSI_dept_area.dta (", nrow(reg_area_final), " observations)")
  message("      Multi-version variables: Auto-detected versions for all Myers-Benford indices")
  message("      ✓ region_corrected preserved: ", "region_corrected" %in% names(reg_area_final))
}

message("\nCSI variables added (same as V1):")
message("  🌾 csi_pre1500_avg_no0: Pre-1500CE Average Calories (excluding zeros)")
message("  🌾 csi_pre1500_max_no0: Pre-1500CE Maximum Calories (excluding zeros)")
message("  🌾 csi_post1500_avg_no0: Post-1500CE Average Calories (excluding zeros)")
message("  🌾 csi_post1500_max_no0: Post-1500CE Maximum Calories (excluding zeros)")

message("\n🔄 Multi-Version Strategy:")
# Dynamically describe the versions found
if (exists("reg_tot_final")) {
  found_versions <- unique(gsub(".*(_V\\d+)$", "\\1", names(reg_tot_final)[grepl("_V\\d+$", names(reg_tot_final))]))
  if (length(found_versions) > 0) {
    for (version in found_versions) {
      version_num <- gsub("_V", "", version)
      message("  • ", version, ": Version ", version_num, " data processing method")
    }
    message("  • All Myers-Benford variables now have ", paste(found_versions, collapse = " and "), " versions")
  }
} else {
  message("  • Multiple versions integrated for robustness analysis")
}
message("  • CSI variables remain unchanged (version-independent)")
message("  • Enables robustness testing within single regression framework")

message("\n📝 Variable names shortened for Stata compatibility:")
# Show actual renamed variables instead of hardcoded examples
if (exists("reg_tot_final")) {
  renamed_examples <- c()
  all_vars <- names(reg_tot_final)
  
  # Find some example shortened variables
  if (any(grepl("myers_std_V\\d+", all_vars))) {
    versions <- unique(gsub("myers_std_(V\\d+)", "\\1", all_vars[grepl("myers_std_V\\d+", all_vars)]))
    if (length(versions) > 0) {
      renamed_examples <- c(renamed_examples, paste0("  • standardized_myers_", versions[1], " → myers_std_", versions[1]))
    }
  }
  
  if (any(grepl("benf_abs_dist_no_zero_V\\d+", all_vars))) {
    versions <- unique(gsub("benf_abs_dist_no_zero_(V\\d+)", "\\1", all_vars[grepl("benf_abs_dist_no_zero_V\\d+", all_vars)]))
    if (length(versions) > 0) {
      renamed_examples <- c(renamed_examples, paste0("  • benford_abs_distance_no_zeros_", versions[1], " → benf_abs_dist_no_zero_", versions[1]))
    }
  }
  
  if (length(renamed_examples) > 0) {
    for (example in renamed_examples) {
      message(example)
    }
  } else {
    message("  • Variable names shortened to fit Stata 32-character limit")
  }
} else {
  message("  • Variable names shortened to fit Stata 32-character limit")
}
message("  • (and similar for all other Myers-Benford variables)")

# Version comparison analysis
if (nrow(reg_tot_merged) > 0) {
  message("\n📊 Version Comparison (Regional Level):")
  if (exists("reg_tot_final")) {
    # Find all versioned variables
    all_versions <- unique(gsub(".*(_V\\d+)$", "\\1", names(reg_tot_final)[grepl("_V\\d+$", names(reg_tot_final))]))
    
    if (length(all_versions) >= 2) {
      # Compare key variables across versions
      myers_vars <- paste0("myers_std", all_versions)
      benford_vars <- paste0("benf_abs_dist_no_zero", all_versions)
      
      # Myers index correlations
      existing_myers <- myers_vars[myers_vars %in% names(reg_tot_final)]
      if (length(existing_myers) >= 2) {
        for (i in 1:(length(existing_myers)-1)) {
          for (j in (i+1):length(existing_myers)) {
            cor_val <- cor(reg_tot_final[[existing_myers[i]]], reg_tot_final[[existing_myers[j]]], use = "complete.obs")
            v1 <- gsub("myers_std_", "", existing_myers[i])
            v2 <- gsub("myers_std_", "", existing_myers[j])
            message("   • Myers Index correlation (", v1, " vs ", v2, "): ", round(cor_val, 3))
          }
        }
      }
      
      # Benford index correlations  
      existing_benford <- benford_vars[benford_vars %in% names(reg_tot_final)]
      if (length(existing_benford) >= 2) {
        for (i in 1:(length(existing_benford)-1)) {
          for (j in (i+1):length(existing_benford)) {
            cor_val <- cor(reg_tot_final[[existing_benford[i]]], reg_tot_final[[existing_benford[j]]], use = "complete.obs")
            v1 <- gsub("benf_abs_dist_no_zero_", "", existing_benford[i])
            v2 <- gsub("benf_abs_dist_no_zero_", "", existing_benford[j])
            message("   • Benford Index correlation (", v1, " vs ", v2, "): ", round(cor_val, 3))
          }
        }
      }
    } else {
      message("   Only one version detected: ", paste(all_versions, collapse = ", "))
    }
  }
}

# Final verification that region_corrected is preserved
message("\n🔍 Final verification:")
if (exists("reg_tot_final") && "region_corrected" %in% names(reg_tot_final)) {
  message("   ✅ region_corrected preserved in departmental dataset")
} else {
  message("   ❌ region_corrected MISSING in departmental dataset")
}

if (exists("reg_area_final") && "region_corrected" %in% names(reg_area_final)) {
  message("   ✅ region_corrected preserved in departmental-area dataset")
} else {
  message("   ❌ region_corrected MISSING in departmental-area dataset")
}

message("\n💡 Next steps:")
message("   1. Load the .dta files in your regression analysis")
message("   2. Compare V01 vs V03 results to assess robustness")
message("   3. Test for sensitivity to zero inclusion/exclusion")
message("   4. Test for sensitivity to outlier treatment methods")
message("   5. Explore relationships between CSI and both versions of Myers/Benford indices")
message("   6. Use version differences to validate main findings")

message("\n🎯 Multi-Version Datasets are ready for comprehensive robustness analysis!")
message("=====================================")

# Display any warnings
if(length(warnings()) > 0) {
  message("\n⚠️  Warnings encountered:")
  print(warnings())
}