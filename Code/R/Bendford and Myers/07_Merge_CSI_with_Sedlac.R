########################################
# CSI Integration with SEDLAC Colombia Analysis
########################################
# Author: Luis Castellanos Rodriguez
# Date: 2025-07-20
# Purpose: Integrate Caloric Suitability Indices with Myers-Benford analysis
# 
# This script:
# 1. Reads Excel outputs from previous Myers-Benford analysis
# 2. Downloads and processes CSI .tif files
# 3. Extracts CSI values by Colombian departments
# 4. Merges CSI data with existing analysis
# 5. Saves integrated datasets as .dta files

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

# Expected CSI file names
csi_files <- list(
  "pre1500_avg_no0" = "pre1500AverageCaloriesNo0.tif",
  "pre1500_max_no0" = "pre1500OptCaloriesNo0.tif",
  "post1500_avg_no0" = "post1500AverageCaloriesNo0.tif",
  "post1500_max_no0" = "post1500OptCaloriesNo0.tif"
)

# Display configuration
message("=== CSI Integration Configuration ===")
message("Excel input path: ", excel_input_path)
message("CSI input path: ", csi_input_path)
message("Output path: ", output_path)
message("=====================================")

########################################
# 2. HELPER FUNCTIONS
########################################

# Function to create department mapping
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

# Function to find specific Excel files
find_excel_files <- function(path) {
  # Look for the specific files mentioned by user
  reg_tot_file <- file.path(path, "01_Myers_Benford_Col_V03_reg-tot.xlsx")
  reg_area_file <- file.path(path, "01_Myers_Benford_Col_V03_reg-area.xlsx")
  
  # Check if files exist
  reg_tot_files <- if(file.exists(reg_tot_file)) reg_tot_file else character(0)
  reg_area_files <- if(file.exists(reg_area_file)) reg_area_file else character(0)
  
  return(list(reg_tot = reg_tot_files, reg_area = reg_area_files))
}

# Function to shorten variable names for Stata compatibility
shorten_variable_names <- function(data) {
  # Create mapping of long names to short names (max 32 chars for Stata)
  name_mapping <- c(
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
  
  # Apply name mapping
  for (old_name in names(name_mapping)) {
    if (old_name %in% names(data)) {
      names(data)[names(data) == old_name] <- name_mapping[old_name]
    }
  }
  
  # Check for any remaining long names (>32 chars), excluding key variables we want to preserve
  preserve_vars <- c("region_corrected", "region_number")
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

# Function to read and combine Excel results
read_excel_results <- function(excel_files, level_name) {
  combined_data <- data.frame()
  
  for (file in excel_files) {
    message("  Reading: ", basename(file))
    
    tryCatch({
      # Read the Results sheet
      data <- readxl::read_excel(file, sheet = "Results")
      
      # Extract version from filename
      version <- str_extract(basename(file), "V\\d+")
      data$version <- version
      data$file_source <- basename(file)
      
      # Combine data
      if (nrow(combined_data) == 0) {
        combined_data <- data
      } else {
        # Use bind_rows to handle potentially different column structures
        combined_data <- dplyr::bind_rows(combined_data, data)
      }
      
    }, error = function(e) {
      message("    ERROR reading ", basename(file), ": ", e$message)
    })
  }
  
  message("  Combined ", level_name, " data: ", nrow(combined_data), " observations")
  return(combined_data)
}

# Function to get Colombian department boundaries
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

# Function to extract CSI values by department
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

message("\n=== Starting CSI Integration Process ===")

# Step 1: Find and read specific Excel files
message("\n1. Reading specific Excel files...")
excel_files <- find_excel_files(excel_input_path)

message("Looking for specific files:")
message("  Regional level: 01_Myers_Benford_Col_V03_reg-tot.xlsx")
message("  Regional-area level: 01_Myers_Benford_Col_V03_reg-area.xlsx")

if (length(excel_files$reg_tot) == 0 && length(excel_files$reg_area) == 0) {
  message("Expected files not found. Checking directory contents...")
  all_files <- list.files(excel_input_path, pattern = "\\.xlsx$")
  message("Available Excel files:")
  for(f in all_files) message("  ", f)
  stop("Required Excel files not found in ", excel_input_path, 
       ". Please ensure the previous analysis completed successfully and created the expected files.")
}

# Read the data
reg_tot_data <- data.frame()
reg_area_data <- data.frame()

if (length(excel_files$reg_tot) > 0) {
  reg_tot_data <- read_excel_results(excel_files$reg_tot, "regional")
}

if (length(excel_files$reg_area) > 0) {
  reg_area_data <- read_excel_results(excel_files$reg_area, "regional-area")
}

# Step 2: Get Colombian boundaries
message("\n2. Getting Colombian department boundaries...")
colombia_sf <- get_colombia_boundaries()

# Step 3: Check CSI files exist
message("\n3. Checking CSI files...")
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

# Step 4: Extract CSI data
message("\n4. Extracting CSI data by department...")
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

# Step 5: Merge CSI with existing data
message("\n5. Merging CSI data with existing analysis...")

# Merge with regional level data
if (nrow(reg_tot_data) > 0) {
  message("  Merging with regional level data...")
  reg_tot_merged <- merge(reg_tot_data, all_csi_data, 
                          by = "region_number", all.x = TRUE)
  
  # Check merge success
  n_matched <- sum(!is.na(reg_tot_merged$csi_pre1500_avg_no0))
  message("    Successfully matched CSI for ", n_matched, "/", nrow(reg_tot_merged), " observations")
  
  # Handle duplicate region_corrected columns (keep the original, remove .x, .y versions)
  if ("region_corrected.x" %in% names(reg_tot_merged)) {
    reg_tot_merged$region_corrected <- reg_tot_merged$region_corrected.x
    reg_tot_merged$region_corrected.x <- NULL
  }
  if ("region_corrected.y" %in% names(reg_tot_merged)) {
    reg_tot_merged$region_corrected.y <- NULL
  }
  
  # Ensure region_corrected exists
  if (!"region_corrected" %in% names(reg_tot_merged)) {
    message("    WARNING: region_corrected not found, attempting to restore from CSI data")
    reg_tot_merged <- merge(reg_tot_merged, 
                            all_csi_data[, c("region_number", "region_corrected")],
                            by = "region_number", all.x = TRUE)
  }
} else {
  reg_tot_merged <- data.frame()
}

# Merge with regional-area level data
if (nrow(reg_area_data) > 0) {
  message("  Merging with regional-area level data...")
  reg_area_merged <- merge(reg_area_data, all_csi_data, 
                           by = "region_number", all.x = TRUE)
  
  # Check merge success
  n_matched <- sum(!is.na(reg_area_merged$csi_pre1500_avg_no0))
  message("    Successfully matched CSI for ", n_matched, "/", nrow(reg_area_merged), " observations")
  
  # Handle duplicate region_corrected columns (keep the original, remove .x, .y versions)
  if ("region_corrected.x" %in% names(reg_area_merged)) {
    reg_area_merged$region_corrected <- reg_area_merged$region_corrected.x
    reg_area_merged$region_corrected.x <- NULL
  }
  if ("region_corrected.y" %in% names(reg_area_merged)) {
    reg_area_merged$region_corrected.y <- NULL
  }
  
  # Ensure region_corrected exists
  if (!"region_corrected" %in% names(reg_area_merged)) {
    message("    WARNING: region_corrected not found, attempting to restore from CSI data")
    reg_area_merged <- merge(reg_area_merged, 
                             all_csi_data[, c("region_number", "region_corrected")],
                             by = "region_number", all.x = TRUE)
  }
} else {
  reg_area_merged <- data.frame()
}

# Step 6: Save as .dta files
message("\n6. Saving integrated datasets...")

# Save regional level
if (nrow(reg_tot_merged) > 0) {
  # Shorten variable names for Stata compatibility
  reg_tot_final <- shorten_variable_names(reg_tot_merged)
  
  dept_file <- file.path(output_path, "01_col_merge_sedlac_CSI_dept.dta")
  haven::write_dta(reg_tot_final, dept_file)
  message("  Saved regional level: ", dept_file)
  message("    Observations: ", nrow(reg_tot_final))
  message("    Variables: ", ncol(reg_tot_final))
  
  # Display sample of key variables (using new names)
  key_vars <- c("region_number", "region_corrected", "ano", "myers_std", 
                "benf_abs_dist_no_zero", "csi_pre1500_avg_no0", "csi_post1500_max_no0")
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
  # Shorten variable names for Stata compatibility
  reg_area_final <- shorten_variable_names(reg_area_merged)
  
  dept_area_file <- file.path(output_path, "01_col_merge_sedlac_CSI_dept_area.dta")
  haven::write_dta(reg_area_final, dept_area_file)
  message("  Saved regional-area level: ", dept_area_file)
  message("    Observations: ", nrow(reg_area_final))
  message("    Variables: ", ncol(reg_area_final))
  
  # Display sample of key variables (using new names)
  key_vars <- c("region_number", "region_corrected", "ano", "urban_area", "myers_std", 
                "benf_abs_dist_no_zero", "csi_pre1500_avg_no0", "csi_post1500_max_no0")
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
# 7. FINAL SUMMARY AND VALIDATION
########################################

message("\n=== CSI Integration Summary ===")
message("✅ Process completed successfully")
message("\nFiles created:")
if (nrow(reg_tot_merged) > 0 && exists("reg_tot_final")) {
  message("  📄 01_col_merge_sedlac_CSI_dept.dta (", nrow(reg_tot_final), " observations)")
  message("      Variables: ", paste(head(names(reg_tot_final), 10), collapse = ", "), "...")
  message("      ✓ region_corrected preserved: ", "region_corrected" %in% names(reg_tot_final))
}
if (nrow(reg_area_merged) > 0 && exists("reg_area_final")) {
  message("  📄 01_col_merge_sedlac_CSI_dept_area.dta (", nrow(reg_area_final), " observations)")
  message("      Variables: ", paste(head(names(reg_area_final), 10), collapse = ", "), "...")
  message("      ✓ region_corrected preserved: ", "region_corrected" %in% names(reg_area_final))
}

message("\nCSI variables added:")
message("  🌾 csi_pre1500_avg_no0: Pre-1500CE Average Calories (excluding zeros)")
message("  🌾 csi_pre1500_max_no0: Pre-1500CE Maximum Calories (excluding zeros)")
message("  🌾 csi_post1500_avg_no0: Post-1500CE Average Calories (excluding zeros)")
message("  🌾 csi_post1500_max_no0: Post-1500CE Maximum Calories (excluding zeros)")

message("\n📝 Variable names shortened for Stata compatibility:")
message("  • n_observations_benford_with_zeros → n_obs_benf_w_zero")
message("  • n_observations_benford_no_zeros → n_obs_benf_no_zero")
message("  • benford_abs_distance_with_zeros → benf_abs_dist_w_zero")
message("  • benford_chi_square_with_zeros → benf_chi2_w_zero")
message("  • benford_abs_distance_no_zeros → benf_abs_dist_no_zero")
message("  • benford_chi_square_no_zeros → benf_chi2_no_zero")
message("  • standardized_myers → myers_std")
message("  • traditional_myers → myers_trad")
message("  • n_observations_myers → n_obs_myers")

# Correlation analysis between CSI variants
if (nrow(reg_tot_merged) > 0) {
  message("\nCSI Variable Correlations (Regional Level):")
  # Use the final dataset with shortened names
  if (exists("reg_tot_final")) {
    csi_vars <- grep("csi_", names(reg_tot_final), value = TRUE)
    if (length(csi_vars) >= 2) {
      csi_cor <- cor(reg_tot_final[, csi_vars], use = "complete.obs")
      print(round(csi_cor, 3))
    }
  }
}

message("\n🎯 Datasets are ready for regression analysis!")
message("\n📊 About the CSI variables:")
message("   • All CSI values are spatial averages within each department boundary")
message("   • Units: millions of kilocalories per hectare per year")
message("   • Pre-1500CE: crops available before Columbian Exchange")
message("   • Post-1500CE: crops available after Columbian Exchange") 
message("   • Average: mean across all suitable crops")
message("   • Maximum: best single crop for each location")
message("   • 'No zeros': excludes areas with zero caloric potential")

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
message("   2. Explore relationships between CSI and Myers/Benford indices")
message("   3. Consider log-transforming CSI variables if highly skewed")
message("   4. Test for spatial autocorrelation in residuals")
message("   5. For dept-area level: both urban/rural have same CSI (dept-level average)")
message("=====================================")

# Display any warnings
if(length(warnings()) > 0) {
  message("\n⚠️  Warnings encountered:")
  print(warnings())
}