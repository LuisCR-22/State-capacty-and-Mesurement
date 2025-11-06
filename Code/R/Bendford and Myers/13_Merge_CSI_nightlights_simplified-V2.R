########################################
# Nighttime Light Median Integration with SEDLAC Colombia Analysis
########################################
# Author: Luis Castellanos Rodriguez
# Date: 2025-08-17
# Purpose: Add median nighttime light data to existing datasets with average nighttime light
# 
# This script:
# 1. Reads existing .dta files with average nighttime light data (03_* files)
# 2. Processes VIIRS median_masked raster files (2013-2024) 
# 3. ADDS ntl_median_masked alongside existing ntl_average_masked values
# 4. Saves datasets with BOTH measures with "04_" prefix for robustness analysis
#
# RESULT: Datasets with TWO nighttime light variables for comparison

########################################
# 0. SETUP AND CONFIGURATION
########################################

# Clean workspace
rm(list = ls())
gc()

# Install and load required packages
packages <- c("haven", "dplyr", "sf", "terra", "exactextractr", 
              "geodata", "stringr", "tidyverse", "data.table", "R.utils")

# Check and install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
}

# Load all packages
lapply(packages, library, character.only = TRUE)

########################################
# 1. CONFIGURATION PARAMETERS
########################################

# PROCESSING MODE: Set to FALSE for full 2013-2024 processing
TEST_MODE <- FALSE

# Input paths
existing_data_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge"
nightlight_base_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/EOG"

# Output path
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge"

# Create output directory if it doesn't exist
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Year ranges based on mode
if (TEST_MODE) {
  years_to_process <- 2013:2014
  message("=== TEST MODE: Processing years 2013-2014 only ===")
} else {
  years_to_process <- 2013:2024
  message("=== FULL MODE: Processing years 2013-2024 ===")
}

message("=== MEDIAN NIGHTTIME LIGHT INTEGRATION ===")
message("Nighttime light base path: ", nightlight_base_path)
message("  V21 data (2013-2021): ", file.path(nightlight_base_path, "V21"))
message("  V22 data (2022-2024): ", file.path(nightlight_base_path, "V22"))
message("Existing data path: ", existing_data_path)
message("Output path: ", output_path)
message("Years to process: ", paste(years_to_process, collapse = ", "))

########################################
# 2. NIGHTTIME LIGHT FILE CONFIGURATION
########################################

# Define file naming patterns and paths for different years/versions
get_nightlight_file_info <- function(year) {
  if (year >= 2013 && year <= 2021) {
    version <- "V21"
    folder_path <- file.path(nightlight_base_path, "V21", as.character(year))
  } else if (year >= 2022 && year <= 2024) {
    version <- "V22"  
    folder_path <- file.path(nightlight_base_path, "V22", as.character(year))
  } else {
    stop("Year ", year, " not supported. Supported years: 2013-2024")
  }
  
  return(list(
    version = version,
    folder_path = folder_path
  ))
}

# Define the median file type we need for each year
get_nightlight_files <- function(year) {
  info <- get_nightlight_file_info(year)
  
  # Use only median_masked files (robustness analysis data)
  # These provide median aggregation instead of mean for validation
  file_types <- list(
    median_masked = "median_masked\\.dat\\.tif"
  )
  
  return(list(
    folder_path = info$folder_path,
    file_patterns = file_types,
    year = year,
    version = info$version
  ))
}

########################################
# 3. HELPER FUNCTIONS
########################################

# Function to create department mapping (reused from previous scripts)
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

# Function to get Colombian boundaries (reused from previous scripts)
get_colombia_boundaries <- function() {
  message("  Getting Colombian department boundaries...")
  
  tryCatch({
    # Download from GADM
    colombia_admin <- geodata::gadm(country = "COL", level = 1, path = tempdir())
    colombia_sf <- sf::st_as_sf(colombia_admin)
    
    # Get department mapping
    dept_mapping <- create_colombia_dept_mapping()
    
    # Merge with mapping
    colombia_sf <- merge(colombia_sf, dept_mapping, 
                         by.x = "NAME_1", by.y = "gadm_name", all.x = TRUE)
    
    # Check for unmapped departments
    unmapped <- colombia_sf[is.na(colombia_sf$region_number), ]
    if (nrow(unmapped) > 0) {
      message("  WARNING: Unmapped departments found:")
      print(unmapped$NAME_1)
    }
    
    # Remove unmapped departments
    colombia_sf <- colombia_sf[!is.na(colombia_sf$region_number), ]
    
    message("  Successfully loaded ", nrow(colombia_sf), " departments")
    return(colombia_sf)
    
  }, error = function(e) {
    message("  ERROR getting boundaries: ", e$message)
    stop("Could not get Colombian boundaries. Check internet connection.")
  })
}

# Function to find nighttime light files for a specific year (median files)
find_nightlight_files <- function(year) {
  file_info <- get_nightlight_files(year)
  folder_path <- file_info$folder_path
  
  if (!dir.exists(folder_path)) {
    message("  WARNING: Folder does not exist: ", folder_path)
    return(NULL)
  }
  
  # List all files in the folder
  all_files <- list.files(folder_path, full.names = TRUE)
  
  # Search for median_masked file only
  pattern <- file_info$file_patterns[["median_masked"]]
  matching_files <- all_files[grepl(pattern, basename(all_files))]
  
  if (length(matching_files) > 0) {
    found_file <- matching_files[1]  # Take first match
    message("    FOUND median_masked: ", basename(found_file))
    
    return(list(
      year = year,
      version = file_info$version,
      files = list(median_masked = found_file)
    ))
  } else {
    message("    MISSING median_masked file (pattern: ", pattern, ")")
    return(NULL)
  }
}

# Function to decompress .gz files if needed
decompress_if_needed <- function(file_path) {
  if (grepl("\\.gz$", file_path)) {
    temp_dir <- tempdir()
    decompressed_path <- file.path(temp_dir, gsub("\\.gz$", "", basename(file_path)))
    
    if (!file.exists(decompressed_path)) {
      message("    Decompressing: ", basename(file_path))
      tryCatch({
        R.utils::gunzip(file_path, decompressed_path, remove = FALSE, overwrite = TRUE)
      }, error = function(e) {
        message("    ERROR decompressing file: ", e$message)
        return(NULL)
      })
    }
    
    # Verify decompressed file exists and has reasonable size
    if (file.exists(decompressed_path)) {
      file_size <- file.info(decompressed_path)$size
      if (is.na(file_size) || file_size < 1000) {  # Less than 1KB is suspicious
        message("    WARNING: Decompressed file appears corrupted (size: ", file_size, " bytes)")
        return(NULL)
      }
      return(decompressed_path)
    } else {
      message("    ERROR: Decompressed file not found")
      return(NULL)
    }
  } else {
    return(file_path)
  }
}

# Function to extract nighttime light values by department
extract_nightlight_by_department <- function(file_path, colombia_sf, variable_name) {
  message("    Extracting ", variable_name, " from ", basename(file_path))
  
  tryCatch({
    # Decompress if needed
    actual_file_path <- decompress_if_needed(file_path)
    
    if (is.null(actual_file_path)) {
      stop("Failed to decompress or access file")
    }
    
    # Load the nighttime light raster
    nl_raster <- terra::rast(actual_file_path)
    message("      Raster loaded: ", terra::ncell(nl_raster), " cells")
    
    # Ensure same CRS
    colombia_sf_proj <- sf::st_transform(colombia_sf, terra::crs(nl_raster))
    
    # Extract mean values by department
    extracted_values <- exactextractr::exact_extract(
      nl_raster, 
      colombia_sf_proj, 
      "mean", 
      progress = FALSE
    )
    
    # Create results dataframe
    nl_results <- data.frame(
      region_number = colombia_sf$region_number,
      region_corrected = colombia_sf$region_corrected,
      stringsAsFactors = FALSE
    )
    
    # Add the nighttime light column
    nl_results[[variable_name]] <- extracted_values
    
    # Check for missing values
    missing_count <- sum(is.na(nl_results[[variable_name]]))
    if (missing_count > 0) {
      message("      WARNING: ", missing_count, " departments have missing values")
    }
    
    message("      Successfully extracted for ", nrow(nl_results), " departments")
    return(nl_results)
    
  }, error = function(e) {
    message("      ERROR extracting ", variable_name, ": ", e$message)
    # Return dataframe with NA values using correct variable name
    nl_results <- data.frame(
      region_number = colombia_sf$region_number,
      region_corrected = colombia_sf$region_corrected,
      stringsAsFactors = FALSE
    )
    nl_results[[variable_name]] <- NA
    return(nl_results)
  })
}

# Function to process nighttime light data for a single year (median files)
process_nightlight_year <- function(year, colombia_sf) {
  message("  Processing median nighttime light data for year ", year, "...")
  
  # Find files for this year
  year_files <- find_nightlight_files(year)
  
  if (is.null(year_files)) {
    message("    Skipping year ", year, " - median_masked file not found")
    return(NULL)
  }
  
  # Process only median_masked file
  year_data <- data.frame(
    region_number = colombia_sf$region_number,
    region_corrected = colombia_sf$region_corrected,
    ano = year,
    stringsAsFactors = FALSE
  )
  
  # Process median_masked file
  if ("median_masked" %in% names(year_files$files)) {
    med_data <- extract_nightlight_by_department(
      year_files$files$median_masked, 
      colombia_sf, 
      "ntl_median_masked"
    )
    if (!is.null(med_data) && "ntl_median_masked" %in% names(med_data)) {
      year_data <- merge(year_data, med_data[, c("region_number", "ntl_median_masked")], 
                         by = "region_number", all.x = TRUE)
      
      message("    Completed year ", year, " with ", nrow(year_data), " observations")
      message("    Median nighttime light successfully processed")
      
      return(year_data)
    } else {
      message("    Failed to process median_masked for year ", year)
      return(NULL)
    }
  } else {
    message("    No median_masked file found for year ", year)
    return(NULL)
  }
}

# Function to clean data for Stata compatibility
clean_stata_data <- function(data) {
  message("    Cleaning data for Stata compatibility...")
  
  # Clean numeric variables - replace Inf, -Inf, NaN with NA
  numeric_vars <- sapply(data, is.numeric)
  for(var in names(data)[numeric_vars]) {
    # Count problematic values
    inf_count <- sum(is.infinite(data[[var]]), na.rm = TRUE)
    nan_count <- sum(is.nan(data[[var]]), na.rm = TRUE)
    
    if(inf_count > 0 || nan_count > 0) {
      message("      Cleaning ", var, ": Inf=", inf_count, ", NaN=", nan_count, " → NA")
      data[[var]][is.infinite(data[[var]]) | is.nan(data[[var]])] <- NA
    }
  }
  
  # Ensure variable names are Stata-compatible (max 32 chars, no special chars)
  original_names <- names(data)
  clean_names <- gsub("[^a-zA-Z0-9_]", "_", original_names)
  clean_names <- substr(clean_names, 1, 32)
  
  # Check for name changes
  name_changes <- original_names != clean_names
  if(any(name_changes)) {
    message("      Variable name changes:")
    for(i in which(name_changes)) {
      message("        ", original_names[i], " → ", clean_names[i])
    }
    names(data) <- clean_names
  }
  
  return(data)
}

# Function to clean up merge artifacts and prepare for Stata
clean_merge_artifacts <- function(data) {
  message("    Cleaning merge artifacts...")
  
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
        message("      Resolved duplicate: ", base_name, " (identical values)")
      } else {
        # If different, keep the .x version (original data) and rename it
        data[[base_name]] <- data[[x_var]]
        data[[x_var]] <- NULL
        data[[y_var]] <- NULL
        message("      Resolved duplicate: ", base_name, " (kept .x version)")
      }
    } else {
      # No .y variable, just rename .x to base name
      data[[base_name]] <- data[[x_var]]
      data[[x_var]] <- NULL
      message("      Renamed: ", x_var, " → ", base_name)
    }
  }
  
  # Handle any remaining .y variables (shouldn't happen if above works correctly)
  remaining_y_vars <- names(data)[grepl("\\.y$", names(data))]
  for (y_var in remaining_y_vars) {
    base_name <- gsub("\\.y$", "", y_var)
    if (!base_name %in% names(data)) {
      data[[base_name]] <- data[[y_var]]
      data[[y_var]] <- NULL
      message("      Renamed remaining: ", y_var, " → ", base_name)
    } else {
      # Base name already exists, remove the .y version
      data[[y_var]] <- NULL
      message("      Removed duplicate: ", y_var)
    }
  }
  
  # Check for any remaining problematic characters in variable names
  problematic_vars <- names(data)[grepl("[^a-zA-Z0-9_]", names(data))]
  if (length(problematic_vars) > 0) {
    message("      WARNING: Variables with problematic characters: ", paste(problematic_vars, collapse = ", "))
    
    # Clean up problematic characters
    for (prob_var in problematic_vars) {
      new_name <- gsub("[^a-zA-Z0-9_]", "_", prob_var)
      if (new_name != prob_var && !new_name %in% names(data)) {
        names(data)[names(data) == prob_var] <- new_name
        message("      Cleaned: ", prob_var, " → ", new_name)
      }
    }
  }
  
  return(data)
}

########################################
# 4. MAIN EXECUTION
########################################

message("\n=== Starting Median Nighttime Light Integration Process ===")

# Step 1: Load existing datasets with average nighttime light data
message("\n1. Loading existing datasets with average nighttime light data...")

dept_file <- file.path(existing_data_path, "03_col_merge_sedlac_CSI_NTL_dept.dta")
dept_area_file <- file.path(existing_data_path, "03_col_merge_sedlac_CSI_NTL_dept_area.dta")

if (!file.exists(dept_file)) {
  stop("Department-level dataset not found: ", dept_file)
}

if (!file.exists(dept_area_file)) {
  stop("Department-area level dataset not found: ", dept_area_file)
}

# Load existing datasets
existing_dept_data <- haven::read_dta(dept_file)
existing_dept_area_data <- haven::read_dta(dept_area_file)

message("  Loaded department-level data: ", nrow(existing_dept_data), " observations")
message("  Loaded department-area data: ", nrow(existing_dept_area_data), " observations")

# Check if existing average nighttime light data exists
avg_ntl_count_dept <- sum(!is.na(existing_dept_data$ntl_average_masked))
avg_ntl_count_area <- sum(!is.na(existing_dept_area_data$ntl_average_masked))
message("  Existing average NTL data: ", avg_ntl_count_dept, " dept obs, ", avg_ntl_count_area, " area obs")

# Step 2: Get Colombian boundaries
message("\n2. Getting Colombian department boundaries...")
colombia_sf <- get_colombia_boundaries()

# Step 3: Process median nighttime light data for each year
message("\n3. Processing median nighttime light data...")

all_median_nightlight_data <- list()

for (year in years_to_process) {
  year_data <- process_nightlight_year(year, colombia_sf)
  if (!is.null(year_data)) {
    all_median_nightlight_data[[as.character(year)]] <- year_data
  }
}

if (length(all_median_nightlight_data) == 0) {
  stop("No median nighttime light data could be processed for any year")
}

# Combine all years
message("\n4. Combining median nighttime light data across years...")
combined_median_nightlight_data <- do.call(rbind, all_median_nightlight_data)

message("  Combined median nighttime light data: ", nrow(combined_median_nightlight_data), " observations")
message("  Years available: ", paste(sort(unique(combined_median_nightlight_data$ano)), collapse = ", "))

# Display summary statistics
message("  Summary of median nighttime light variable:")
if ("ntl_median_masked" %in% names(combined_median_nightlight_data)) {
  var_summary <- summary(combined_median_nightlight_data[["ntl_median_masked"]])
  message("    ntl_median_masked: Min=", round(var_summary[1], 3), 
          ", Mean=", round(var_summary[4], 3), 
          ", Max=", round(var_summary[6], 3))
}

# Step 4: Add median nighttime light alongside existing average nighttime light
message("\n4. Adding median nighttime light data to existing datasets...")

# For department-level data
message("  Updating department-level data...")

# Keep existing ntl_average_masked and add ntl_median_masked
merged_dept_data <- merge(existing_dept_data, combined_median_nightlight_data, 
                         by = c("region_number", "ano"), all.x = TRUE)

# Check that both variables exist
if ("ntl_average_masked" %in% names(merged_dept_data) && "ntl_median_masked" %in% names(merged_dept_data)) {
  message("    Successfully added median nighttime light alongside existing average data")
} else {
  message("    WARNING: Expected variables not found")
  message("      ntl_average_masked present: ", "ntl_average_masked" %in% names(merged_dept_data))
  message("      ntl_median_masked present: ", "ntl_median_masked" %in% names(merged_dept_data))
}

# Check merge success
n_matched_dept <- sum(!is.na(merged_dept_data$ntl_median_masked))
message("    Matched median nighttime light data for ", n_matched_dept, "/", nrow(merged_dept_data), " department observations")

# For department-area data
message("  Updating department-area data...")

# Keep existing ntl_average_masked and add ntl_median_masked
merged_dept_area_data <- merge(existing_dept_area_data, combined_median_nightlight_data,
                              by = c("region_number", "ano"), all.x = TRUE)

# Check that both variables exist
if ("ntl_average_masked" %in% names(merged_dept_area_data) && "ntl_median_masked" %in% names(merged_dept_area_data)) {
  message("    Successfully added median nighttime light alongside existing average data")
} else {
  message("    WARNING: Expected variables not found")
  message("      ntl_average_masked present: ", "ntl_average_masked" %in% names(merged_dept_area_data))
  message("      ntl_median_masked present: ", "ntl_median_masked" %in% names(merged_dept_area_data))
}

# Check merge success
n_matched_area <- sum(!is.na(merged_dept_area_data$ntl_median_masked))
message("    Matched median nighttime light data for ", n_matched_area, "/", nrow(merged_dept_area_data), " department-area observations")

# Step 5: Clean merge artifacts and save datasets
message("\n5. Cleaning merge artifacts and saving updated datasets...")

# Clean department-level data
message("  Cleaning department-level merge artifacts...")
clean_merged_dept_data <- clean_merge_artifacts(merged_dept_data)
clean_merged_dept_data <- clean_stata_data(clean_merged_dept_data)

# Clean department-area data
message("  Cleaning department-area merge artifacts...")
clean_merged_dept_area_data <- clean_merge_artifacts(merged_dept_area_data)
clean_merged_dept_area_data <- clean_stata_data(clean_merged_dept_area_data)

# Save department-level dataset
dept_output_file <- file.path(output_path, "04_col_merge_sedlac_CSI_NTL_dept.dta")
haven::write_dta(clean_merged_dept_data, dept_output_file)
message("  Saved department-level dataset: ", dept_output_file)
message("    Observations: ", nrow(clean_merged_dept_data))
message("    Variables: ", ncol(clean_merged_dept_data))

# Save department-area dataset
area_output_file <- file.path(output_path, "04_col_merge_sedlac_CSI_NTL_dept_area.dta")
haven::write_dta(clean_merged_dept_area_data, area_output_file)
message("  Saved department-area dataset: ", area_output_file)
message("    Observations: ", nrow(clean_merged_dept_area_data))
message("    Variables: ", ncol(clean_merged_dept_area_data))

########################################
# 6. VALIDATION AND SUMMARY
########################################

message("\n=== Median Nighttime Light Integration Summary ===")
message("✅ Process completed successfully")

if (TEST_MODE) {
  message("\n🧪 TEST MODE RESULTS:")
  message("  Processed years: ", paste(years_to_process, collapse = ", "))
  message("  Other years will have missing nighttime light values")
  message("  Set TEST_MODE = FALSE to process all years 2013-2024")
} else {
  message("\n🌍 FULL MODE RESULTS:")
  message("  Processed years: ", paste(sort(unique(combined_median_nightlight_data$ano)), collapse = ", "))
}

message("\nFiles created:")
message("  📄 04_col_merge_sedlac_CSI_NTL_dept.dta (", nrow(clean_merged_dept_data), " observations)")
message("  📄 04_col_merge_sedlac_CSI_NTL_dept_area.dta (", nrow(clean_merged_dept_area_data), " observations)")

message("\nNighttime light variables in dataset:")
message("  🌙 ntl_average_masked: ORIGINAL average radiance values (nW/cm2/sr)")
message("      Mean aggregation - from previous 03_* files")
message("  🌙 ntl_median_masked: NEW median radiance values (nW/cm2/sr)")
message("      Median aggregation - added for robustness analysis")

message("\n📊 Data Coverage:")
message("  Years with nighttime light data: ", paste(sort(unique(combined_median_nightlight_data$ano)), collapse = ", "))

# Check coverage for both variables
avg_count_dept <- sum(!is.na(clean_merged_dept_data$ntl_average_masked))
med_count_dept <- sum(!is.na(clean_merged_dept_data$ntl_median_masked))
avg_count_area <- sum(!is.na(clean_merged_dept_area_data$ntl_average_masked))
med_count_area <- sum(!is.na(clean_merged_dept_area_data$ntl_median_masked))

message("  Department observations - Average NTL: ", avg_count_dept, ", Median NTL: ", med_count_dept)
message("  Department-area observations - Average NTL: ", avg_count_area, ", Median NTL: ", med_count_area)

# Display sample of key variables including both nighttime light measures
key_vars <- c("region_number", "region_corrected", "ano", "ntl_average_masked", "ntl_median_masked", "csi_pre1500_avg_no0")
available_vars <- key_vars[key_vars %in% names(clean_merged_dept_data)]
if (length(available_vars) > 0) {
  message("\n📋 Sample data (first 6 rows, key variables):")
  sample_data <- head(clean_merged_dept_data[!is.na(clean_merged_dept_data$ntl_median_masked), available_vars])
  print(sample_data)
}

message("\n💡 Robustness Analysis (Both Variables in Same Dataset):")
message("  1. Direct comparison: corr ntl_average_masked ntl_median_masked")
message("  2. Test regression sensitivity to aggregation method")
message("  3. Examine outlier influence: scatter ntl_median_masked ntl_average_masked") 
message("  4. Compare coefficients using both measures as dependent/independent variables")
message("  5. Analyze relationships with CSI and Myers-Benford indices using both measures")

message("\n🎯 Enhanced dataset ready with both nighttime light measures!")
message("=====================================")

# Display any warnings
if(length(warnings()) > 0) {
  message("\n⚠️  Warnings encountered:")
  print(warnings())
}

########################################
# 7. FOLDER STRUCTURE REQUIREMENTS
########################################

# NOTE: This script uses the same folder structure as the previous script:
# 
# V21 (2013-2021):
# "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/EOG/V21/2013/"
# ... through 2021
#
# V22 (2022-2024):  
# "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/EOG/V22/2022/"
# ... through 2024
#
# Each folder should contain files with this identifying string in their names:
# - "median_masked.dat.tif" (with or without .gz compression) - Median aggregation data
#
# ROBUSTNESS APPROACH: Uses median_masked files for validation
# Provides alternative to mean aggregation for sensitivity analysis