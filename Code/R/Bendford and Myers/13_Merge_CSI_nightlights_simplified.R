########################################
# Nighttime Light Integration with SEDLAC Colombia Analysis
########################################
# Author: Luis Castellanos Rodriguez
# Date: 2025-08-17
# Purpose: Integrate VIIRS Nighttime Light data with existing CSI-Myers-Benford datasets
# 
# This script:
# 1. Reads existing .dta files with CSI and Myers-Benford data
# 2. Processes VIIRS nighttime light raster files (2013-2024) 
# 3. Extracts nighttime light values by Colombian departments
# 4. Merges nighttime light data with existing datasets
# 5. Saves integrated datasets with "03_" prefix
#
# SIMPLIFIED APPROACH: Uses only average_masked files (~270-313MB each)
# to avoid OneDrive sync issues with large cf_cvg files (~1.8-2.0GB each)

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

# TEST MODE: Set to TRUE for 2013-2014 only, FALSE for full 2013-2024
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

# Define the primary file type we need for each year (simplified for file size)
get_nightlight_files <- function(year) {
  info <- get_nightlight_file_info(year)
  
  # Use only average_masked files (primary analysis data, smaller file size)
  # Skip cf_cvg (1.8-2.0GB each) and median_masked to avoid OneDrive sync issues
  file_types <- list(
    average_masked = "average_masked\\.dat\\.tif"
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

# Function to create department mapping (reused from CSI script)
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

# Function to get Colombian boundaries (reused from CSI script)
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

# Function to find nighttime light files for a specific year (simplified)
find_nightlight_files <- function(year) {
  file_info <- get_nightlight_files(year)
  folder_path <- file_info$folder_path
  
  if (!dir.exists(folder_path)) {
    message("  WARNING: Folder does not exist: ", folder_path)
    return(NULL)
  }
  
  # List all files in the folder
  all_files <- list.files(folder_path, full.names = TRUE)
  
  # Search for average_masked file only
  pattern <- file_info$file_patterns[["average_masked"]]
  matching_files <- all_files[grepl(pattern, basename(all_files))]
  
  if (length(matching_files) > 0) {
    found_file <- matching_files[1]  # Take first match
    message("    FOUND average_masked: ", basename(found_file))
    
    return(list(
      year = year,
      version = file_info$version,
      files = list(average_masked = found_file)
    ))
  } else {
    message("    MISSING average_masked file (pattern: ", pattern, ")")
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

# Function to process nighttime light data for a single year
process_nightlight_year <- function(year, colombia_sf) {
  message("  Processing nighttime light data for year ", year, "...")
  
  # Find files for this year
  year_files <- find_nightlight_files(year)
  
  if (is.null(year_files)) {
    message("    Skipping year ", year, " - files not found or incomplete")
    return(NULL)
  }
  
  # Process each file type
  year_data <- data.frame(
    region_number = colombia_sf$region_number,
    region_corrected = colombia_sf$region_corrected,
    ano = year,
    stringsAsFactors = FALSE
  )
  
  # Process average_masked file
  if ("average_masked" %in% names(year_files$files)) {
    avg_data <- extract_nightlight_by_department(
      year_files$files$average_masked, 
      colombia_sf, 
      "ntl_average_masked"
    )
    if (!is.null(avg_data) && "ntl_average_masked" %in% names(avg_data)) {
      year_data <- merge(year_data, avg_data[, c("region_number", "ntl_average_masked")], 
                         by = "region_number", all.x = TRUE)
    } else {
      message("    Failed to process average_masked for year ", year)
      year_data$ntl_average_masked <- NA
    }
  } else {
    year_data$ntl_average_masked <- NA
  }
  
  # Process cf_cvg file
  if ("cf_cvg" %in% names(year_files$files)) {
    cvg_data <- extract_nightlight_by_department(
      year_files$files$cf_cvg, 
      colombia_sf, 
      "ntl_cf_cvg"
    )
    if (!is.null(cvg_data) && "ntl_cf_cvg" %in% names(cvg_data)) {
      year_data <- merge(year_data, cvg_data[, c("region_number", "ntl_cf_cvg")], 
                         by = "region_number", all.x = TRUE)
    } else {
      message("    Failed to process cf_cvg for year ", year)
      year_data$ntl_cf_cvg <- NA
    }
  } else {
    year_data$ntl_cf_cvg <- NA
  }
  
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
    } else {
      message("    Failed to process median_masked for year ", year)
      year_data$ntl_median_masked <- NA
    }
  } else {
    year_data$ntl_median_masked <- NA
  }
  
  message("    Completed year ", year, " with ", nrow(year_data), " observations")
  
  # Report success/failure status for this year
  files_processed <- c(
    paste0("average_masked: ", !all(is.na(year_data$ntl_average_masked))),
    paste0("cf_cvg: ", !all(is.na(year_data$ntl_cf_cvg))),
    paste0("median_masked: ", !all(is.na(year_data$ntl_median_masked)))
  )
  message("    Files processed - ", paste(files_processed, collapse = ", "))
  
  return(year_data)
}

########################################
# 4. MAIN EXECUTION
########################################

message("\n=== Starting Nighttime Light Integration Process ===")

# Step 1: Load existing datasets
message("\n1. Loading existing merged datasets...")

dept_file <- file.path(existing_data_path, "02_col_merge_sedlac_CSI_dept.dta")
dept_area_file <- file.path(existing_data_path, "02_col_merge_sedlac_CSI_dept_area.dta")

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

# Step 2: Get Colombian boundaries
message("\n2. Getting Colombian department boundaries...")
colombia_sf <- get_colombia_boundaries()

# Step 3: Process nighttime light data for each year
message("\n3. Processing nighttime light data...")

all_nightlight_data <- list()

for (year in years_to_process) {
  year_data <- process_nightlight_year(year, colombia_sf)
  if (!is.null(year_data)) {
    all_nightlight_data[[as.character(year)]] <- year_data
  }
}

if (length(all_nightlight_data) == 0) {
  stop("No nighttime light data could be processed for any year")
}

# Combine all years
message("\n4. Combining nighttime light data across years...")
combined_nightlight_data <- do.call(rbind, all_nightlight_data)

message("  Combined nighttime light data: ", nrow(combined_nightlight_data), " observations")
message("  Years available: ", paste(sort(unique(combined_nightlight_data$ano)), collapse = ", "))

# Display summary statistics
message("  Summary of nighttime light variable:")
if ("ntl_average_masked" %in% names(combined_nightlight_data)) {
  var_summary <- summary(combined_nightlight_data[["ntl_average_masked"]])
  message("    ntl_average_masked: Min=", round(var_summary[1], 3), 
          ", Mean=", round(var_summary[4], 3), 
          ", Max=", round(var_summary[6], 3))
}

# Step 4: Merge with existing datasets
message("\n4. Merging nighttime light data with existing datasets...")

# Merge with department-level data
message("  Merging with department-level data...")
merged_dept_data <- merge(existing_dept_data, combined_nightlight_data, 
                          by = c("region_number", "ano"), all.x = TRUE)

# Check merge success
n_matched_dept <- sum(!is.na(merged_dept_data$ntl_average_masked))
message("    Matched nighttime light data for ", n_matched_dept, "/", nrow(merged_dept_data), " department observations")

# Merge with department-area data  
message("  Merging with department-area data...")
merged_dept_area_data <- merge(existing_dept_area_data, combined_nightlight_data,
                               by = c("region_number", "ano"), all.x = TRUE)

# Check merge success
n_matched_area <- sum(!is.na(merged_dept_area_data$ntl_average_masked))
message("    Matched nighttime light data for ", n_matched_area, "/", nrow(merged_dept_area_data), " department-area observations")

# Step 5: Clean merge artifacts and save datasets
message("\n5. Cleaning merge artifacts and saving integrated datasets...")

# Clean department-level data
message("  Cleaning department-level merge artifacts...")
clean_merged_dept_data <- clean_merge_artifacts(merged_dept_data)
clean_merged_dept_data <- clean_stata_data(clean_merged_dept_data)

# Clean department-area data
message("  Cleaning department-area merge artifacts...")
clean_merged_dept_area_data <- clean_merge_artifacts(merged_dept_area_data)
clean_merged_dept_area_data <- clean_stata_data(clean_merged_dept_area_data)

# Save department-level dataset
dept_output_file <- file.path(output_path, "03_col_merge_sedlac_CSI_NTL_dept.dta")
haven::write_dta(clean_merged_dept_data, dept_output_file)
message("  Saved department-level dataset: ", dept_output_file)
message("    Observations: ", nrow(clean_merged_dept_data))
message("    Variables: ", ncol(clean_merged_dept_data))

# Save department-area dataset
area_output_file <- file.path(output_path, "03_col_merge_sedlac_CSI_NTL_dept_area.dta")
haven::write_dta(clean_merged_dept_area_data, area_output_file)
message("  Saved department-area dataset: ", area_output_file)
message("    Observations: ", nrow(clean_merged_dept_area_data))
message("    Variables: ", ncol(clean_merged_dept_area_data))

########################################
# 6. VALIDATION AND SUMMARY
########################################

message("\n=== Nighttime Light Integration Summary ===")
message("✅ Process completed successfully")

if (TEST_MODE) {
  message("\n🧪 TEST MODE RESULTS:")
  message("  Processed years: ", paste(years_to_process, collapse = ", "))
  message("  Other years will have missing nighttime light values")
  message("  Set TEST_MODE = FALSE to process all years 2013-2024")
} else {
  message("\n🌍 FULL MODE RESULTS:")
  message("  Processed years: ", paste(sort(unique(combined_nightlight_data$ano)), collapse = ", "))
}

message("\nFiles created:")
message("  📄 03_col_merge_sedlac_CSI_NTL_dept.dta (", nrow(clean_merged_dept_data), " observations)")
message("  📄 03_col_merge_sedlac_CSI_NTL_dept_area.dta (", nrow(clean_merged_dept_area_data), " observations)")

message("\nNighttime light variable added:")
message("  🌙 ntl_average_masked: Average radiance with background masked (nW/cm2/sr)")
message("      Primary analysis variable - background removed, outliers filtered")

message("\n📊 Data Coverage:")
message("  Years with nighttime light data: ", paste(sort(unique(combined_nightlight_data$ano)), collapse = ", "))
message("  Total department-year observations with NTL: ", sum(!is.na(clean_merged_dept_data$ntl_average_masked)))
message("  Total department-area-year observations with NTL: ", sum(!is.na(clean_merged_dept_area_data$ntl_average_masked)))

# Display sample of key variables
key_vars <- c("region_number", "region_corrected", "ano", "ntl_average_masked", "csi_pre1500_avg_no0")
available_vars <- key_vars[key_vars %in% names(clean_merged_dept_data)]
if (length(available_vars) > 0) {
  message("\n📋 Sample data (first 6 rows, key variables):")
  sample_data <- head(clean_merged_dept_data[!is.na(clean_merged_dept_data$ntl_average_masked), available_vars])
  print(sample_data)
}

message("\n💡 Next steps:")
message("  1. Validate nighttime light data patterns across years 2013-2024")
message("  2. Examine correlations between NTL and economic indicators")
message("  3. Analyze relationships between NTL, CSI, and Myers-Benford indices")
message("  4. Consider downloading cf_cvg/median_masked files later for robustness checks")

if (TEST_MODE) {
  message("\n🔄 To process all years (2013-2024):")
  message("  Set TEST_MODE = FALSE and rerun the script")
}

message("\n🎯 Integrated dataset ready for nighttime light analysis!")
message("=====================================")

# Display any warnings
if(length(warnings()) > 0) {
  message("\n⚠️  Warnings encountered:")
  print(warnings())
}

########################################
# 7. FOLDER STRUCTURE REQUIREMENTS
########################################

# NOTE: For the script to work, ensure the following folder structure exists:
# 
# V21 (2013-2021):
# "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/EOG/V21/2013/"
# "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/EOG/V21/2014/"
# ... through 2021
#
# V22 (2022-2024):  
# "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/EOG/V22/2022/"
# "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/EOG/V22/2023/"
# "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/EOG/V22/2024/"
#
# Each folder should contain files with this identifying string in their names:
# - "average_masked.dat.tif" (with or without .gz compression) - Primary analysis data
#
# SIMPLIFIED APPROACH: Only uses average_masked files (~270-313MB each) 
# to avoid OneDrive sync issues with large cf_cvg files (~1.8-2.0GB each).
# The average_masked files contain the primary nighttime light data with 
# background removed and outliers filtered.
#
# If needed later, cf_cvg and median_masked files can be processed separately
# for robustness checks once OneDrive sync issues are resolved.