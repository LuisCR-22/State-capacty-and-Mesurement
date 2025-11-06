########################################
# Coca Crops Data Transformation Script
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-09-09
# Purpose: 
# Transform coca crops data from wide to long format
# Aggregate municipality-level data to department level
# Save as Stata file for further analysis

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Install and load required packages
packages <- c("haven", "dplyr", "tidyr", "readr", "stringr")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input file path
input_file_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/DANE/Detección_de_Cultivos_de_Coca_(hectáreas)_20250909.csv"

# Output file path  
output_file_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/DANE/02_Coca_crops_depto.dta"

########################################
# DATA LOADING AND INSPECTION
########################################

load_and_inspect_coca_data <- function() {
  message("Loading coca crops data from: ", basename(input_file_path))
  
  # Check if file exists
  if (!file.exists(input_file_path)) {
    stop("Input file not found: ", input_file_path)
  }
  
  # Read CSV file with proper encoding and handling
  tryCatch({
    # Try reading with different encodings if needed
    data <- read_csv(input_file_path, 
                     locale = locale(encoding = "UTF-8"),
                     col_types = cols(.default = "c"))  # Read everything as character first
    
    message("Data loaded successfully:")
    message("  - Observations: ", nrow(data))
    message("  - Variables: ", ncol(data))
    
    # Display column names
    message("  - Column names: ", paste(names(data)[1:10], collapse = ", "), "...")
    
    # Display first few rows for inspection
    message("\nFirst 3 rows:")
    print(head(data, 3))
    
    return(data)
    
  }, error = function(e) {
    message("Error reading CSV with UTF-8, trying Latin-1 encoding...")
    tryCatch({
      data <- read_csv(input_file_path, 
                       locale = locale(encoding = "Latin-1"),
                       col_types = cols(.default = "c"))
      
      message("Data loaded successfully with Latin-1 encoding:")
      message("  - Observations: ", nrow(data))
      message("  - Variables: ", ncol(data))
      
      return(data)
      
    }, error = function(e2) {
      stop("Failed to read CSV file with both UTF-8 and Latin-1 encodings. Error: ", e2$message)
    })
  })
}

########################################
# DATA CLEANING AND VALIDATION
########################################

clean_and_validate_data <- function(data) {
  message("\nCleaning and validating data...")
  
  # Check for required columns
  required_cols <- c("CODDEPTO", "DEPARTAMENTO", "CODMPIO", "MUNICIPIO")
  missing_cols <- required_cols[!required_cols %in% names(data)]
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Identify year columns (should be numeric and between 2000-2030)
  all_cols <- names(data)
  year_cols <- all_cols[!all_cols %in% required_cols]
  
  # Validate year columns are actually years
  potential_years <- suppressWarnings(as.numeric(year_cols))
  valid_year_mask <- !is.na(potential_years) & potential_years >= 2000 & potential_years <= 2030
  
  if (!all(valid_year_mask)) {
    invalid_cols <- year_cols[!valid_year_mask]
    message("  WARNING: Found non-year columns that will be excluded: ", paste(invalid_cols, collapse = ", "))
  }
  
  year_cols <- year_cols[valid_year_mask]
  message("  Year columns identified: ", paste(year_cols, collapse = ", "))
  
  # Keep only required columns and year columns
  data_clean <- data[, c(required_cols, year_cols)]
  
  # Validate department and municipality codes
  message("  Validating geographic codes...")
  
  # Check CODDEPTO format (should be numeric)
  data_clean$CODDEPTO <- suppressWarnings(as.numeric(data_clean$CODDEPTO))
  invalid_depto <- is.na(data_clean$CODDEPTO)
  
  if (any(invalid_depto)) {
    message("    WARNING: ", sum(invalid_depto), " rows with invalid CODDEPTO removed")
    data_clean <- data_clean[!invalid_depto, ]
  }
  
  # Check CODMPIO format (should be numeric) 
  data_clean$CODMPIO <- suppressWarnings(as.numeric(data_clean$CODMPIO))
  invalid_mpio <- is.na(data_clean$CODMPIO)
  
  if (any(invalid_mpio)) {
    message("    WARNING: ", sum(invalid_mpio), " rows with invalid CODMPIO removed")
    data_clean <- data_clean[!invalid_mpio, ]
  }
  
  message("  Data after cleaning: ", nrow(data_clean), " rows")
  
  # Display department distribution
  depto_summary <- data_clean %>%
    group_by(CODDEPTO, DEPARTAMENTO) %>%
    summarise(n_municipios = n(), .groups = 'drop') %>%
    arrange(CODDEPTO)
  
  message("  Departments found: ", nrow(depto_summary))
  message("  Total municipalities: ", sum(depto_summary$n_municipios))
  
  return(list(data = data_clean, year_cols = year_cols, depto_summary = depto_summary))
}

########################################
# WIDE TO LONG TRANSFORMATION
########################################

transform_to_long_format <- function(data, year_cols) {
  message("\nTransforming data from wide to long format...")
  
  # Function to clean numeric values (remove commas, handle decimals)
  clean_numeric <- function(x) {
    if (is.character(x)) {
      # Remove commas and handle decimal points
      x <- str_replace_all(x, ",", "")
      x <- str_replace_all(x, "\\s+", "")  # Remove spaces
      # Convert to numeric, NA for non-numeric values
      x <- suppressWarnings(as.numeric(x))
    }
    return(x)
  }
  
  # Convert year columns to numeric, handling commas and formatting issues
  message("  Converting year columns to numeric...")
  for (col in year_cols) {
    data[[col]] <- clean_numeric(data[[col]])
  }
  
  # Check for any completely empty year columns
  year_data_summary <- sapply(year_cols, function(x) sum(!is.na(data[[x]])))
  message("  Non-missing values per year: ")
  for (i in seq_along(year_data_summary)) {
    message("    ", names(year_data_summary)[i], ": ", year_data_summary[i])
  }
  
  # Transform to long format
  message("  Pivoting to long format...")
  data_long <- data %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "ano",
      values_to = "coca_hectares",
      names_transform = list(ano = as.numeric)
    ) %>%
    # Remove rows where coca_hectares is NA
    filter(!is.na(coca_hectares)) %>%
    # Ensure coca_hectares is non-negative (replace negative with 0)
    mutate(coca_hectares = pmax(coca_hectares, 0, na.rm = TRUE))
  
  message("  Long format data: ", nrow(data_long), " observations")
  message("  Year range: ", min(data_long$ano, na.rm = TRUE), " - ", max(data_long$ano, na.rm = TRUE))
  
  # Display summary statistics
  summary_stats <- data_long %>%
    summarise(
      total_obs = n(),
      years = n_distinct(ano),
      departments = n_distinct(CODDEPTO),
      municipalities = n_distinct(CODMPIO),
      total_hectares = sum(coca_hectares, na.rm = TRUE),
      mean_hectares = mean(coca_hectares, na.rm = TRUE),
      median_hectares = median(coca_hectares, na.rm = TRUE),
      max_hectares = max(coca_hectares, na.rm = TRUE)
    )
  
  message("  Summary statistics:")
  message("    Total observations: ", summary_stats$total_obs)
  message("    Years covered: ", summary_stats$years)
  message("    Departments: ", summary_stats$departments)  
  message("    Municipalities: ", summary_stats$municipalities)
  message("    Total hectares: ", round(summary_stats$total_hectares, 1))
  message("    Mean hectares: ", round(summary_stats$mean_hectares, 2))
  message("    Median hectares: ", round(summary_stats$median_hectares, 2))
  message("    Max hectares: ", round(summary_stats$max_hectares, 1))
  
  return(data_long)
}

########################################
# AGGREGATE TO DEPARTMENT LEVEL
########################################

aggregate_to_department_level <- function(data_long) {
  message("\nAggregating data to department level...")
  
  # Aggregate by department and year
  data_depto <- data_long %>%
    group_by(CODDEPTO, DEPARTAMENTO, ano) %>%
    summarise(
      coca_hectares_total = sum(coca_hectares, na.rm = TRUE),
      n_municipalities = n_distinct(CODMPIO),
      n_municipalities_with_coca = sum(coca_hectares > 0, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(CODDEPTO, ano)
  
  message("  Department-level data: ", nrow(data_depto), " observations")
  message("  Departments: ", n_distinct(data_depto$CODDEPTO))
  message("  Years: ", n_distinct(data_depto$ano))
  
  # Display top departments by total coca production
  top_departments <- data_depto %>%
    group_by(CODDEPTO, DEPARTAMENTO) %>%
    summarise(total_coca_all_years = sum(coca_hectares_total, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(total_coca_all_years)) %>%
    head(10)
  
  message("  Top 10 departments by total coca hectares:")
  for (i in 1:nrow(top_departments)) {
    message("    ", i, ". ", top_departments$DEPARTAMENTO[i], " (", 
            top_departments$CODDEPTO[i], "): ", 
            round(top_departments$total_coca_all_years[i], 1), " hectares")
  }
  
  # Year-by-year summary
  yearly_summary <- data_depto %>%
    group_by(ano) %>%
    summarise(
      total_hectares = sum(coca_hectares_total, na.rm = TRUE),
      departments_with_coca = sum(coca_hectares_total > 0, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(ano)
  
  message("  Yearly summary (total hectares):")
  for (i in 1:nrow(yearly_summary)) {
    message("    ", yearly_summary$ano[i], ": ", 
            round(yearly_summary$total_hectares[i], 1), " hectares in ", 
            yearly_summary$departments_with_coca[i], " departments")
  }
  
  return(data_depto)
}

########################################
# SAVE AS STATA FILE
########################################

save_as_stata <- function(data_depto, output_path) {
  message("\nSaving data as Stata file...")
  
  # Prepare data for Stata export
  # Ensure variable names are Stata-compatible
  data_final <- data_depto %>%
    rename(
      coddepto = CODDEPTO,
      departamento = DEPARTAMENTO,
      coca_hectares = coca_hectares_total,
      n_mpio = n_municipalities,
      n_mpio_coca = n_municipalities_with_coca
    ) %>%
    mutate(
      # Create additional useful variables
      coca_per_mpio = ifelse(n_mpio > 0, coca_hectares / n_mpio, 0),
      pct_mpio_with_coca = ifelse(n_mpio > 0, (n_mpio_coca / n_mpio) * 100, 0),
      # Create log variables (add 1 to handle zeros)
      log_coca_hectares = log(coca_hectares + 1),
      # Create binary indicator for coca presence
      coca_present = ifelse(coca_hectares > 0, 1, 0)
    )
  
  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Save as Stata file
  tryCatch({
    haven::write_dta(data_final, output_path)
    
    message("  ✅ Successfully saved: ", basename(output_path))
    message("  📁 Path: ", output_path)
    message("  📊 Final dataset: ", nrow(data_final), " observations, ", ncol(data_final), " variables")
    
    # Display variable list
    message("  📋 Variables created:")
    variable_info <- data.frame(
      Variable = names(data_final),
      Description = c(
        "Department code",
        "Department name", 
        "Year",
        "Total coca hectares in department",
        "Number of municipalities in department",
        "Number of municipalities with coca",
        "Average coca hectares per municipality",
        "Percentage of municipalities with coca",
        "Log(coca hectares + 1)",
        "Binary: 1 if coca present, 0 otherwise"
      )
    )
    
    for (i in 1:nrow(variable_info)) {
      message("    ", variable_info$Variable[i], ": ", variable_info$Description[i])
    }
    
    return(TRUE)
    
  }, error = function(e) {
    message("  ❌ Error saving Stata file: ", e$message)
    return(FALSE)
  })
}

########################################
# MAIN EXECUTION FUNCTION
########################################

run_coca_data_transformation <- function() {
  message("Starting Coca Crops Data Transformation")
  message(paste(rep("=", 60), collapse=""))
  
  # Step 1: Load and inspect data
  raw_data <- load_and_inspect_coca_data()
  
  # Step 2: Clean and validate data
  cleaned_result <- clean_and_validate_data(raw_data)
  cleaned_data <- cleaned_result$data
  year_cols <- cleaned_result$year_cols
  depto_summary <- cleaned_result$depto_summary
  
  # Step 3: Transform to long format
  long_data <- transform_to_long_format(cleaned_data, year_cols)
  
  # Step 4: Aggregate to department level
  depto_data <- aggregate_to_department_level(long_data)
  
  # Step 5: Save as Stata file
  success <- save_as_stata(depto_data, output_file_path)
  
  # Final summary
  message("\n", paste(rep("=", 60), collapse=""))
  message("TRANSFORMATION SUMMARY:")
  message("✅ Raw data loaded: ", nrow(raw_data), " municipality records")
  message("✅ Years processed: ", paste(year_cols, collapse = ", "))
  message("✅ Long format created: ", nrow(long_data), " municipality-year observations")
  message("✅ Department aggregation: ", nrow(depto_data), " department-year observations")
  
  if (success) {
    message("✅ Stata file created: ", basename(output_file_path))
  } else {
    message("❌ Failed to create Stata file")
  }
  
  message("\n📊 Data Ready for Analysis:")
  message("   🎯 Primary variable: coca_hectares (total hectares by department-year)")
  message("   📍 Geographic level: Department (", n_distinct(depto_data$coddepto), " departments)")
  message("   📅 Time coverage: ", min(depto_data$ano), "-", max(depto_data$ano))
  message("   📈 Additional variables: municipality counts, percentages, log transform")
  
  message("\n🔄 Next Steps:")
  message("   1. Merge with Myers-Benford dataset using coddepto and ano")
  message("   2. Use coca_hectares as dependent variable")
  message("   3. Run regression analysis with same model specifications")
  
  return(list(
    raw_data = raw_data,
    depto_data = depto_data,
    year_cols = year_cols,
    depto_summary = depto_summary,
    success = success
  ))
}

########################################
# EXECUTE TRANSFORMATION
########################################

# Run the transformation
results <- run_coca_data_transformation()

message("\n🎯 COCA CROPS DATA TRANSFORMATION COMPLETED")
message("📋 Output File: ", basename(output_file_path))
message("🔗 Ready to merge with existing Myers-Benford analysis")
message("📊 Use coca_hectares as new dependent variable")