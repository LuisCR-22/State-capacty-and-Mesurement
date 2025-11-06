########################################
# Colombia Myers-Benford Analysis - Excel-DTA Merger with V01/V03 Versions
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-08-18
# Purpose: 
# 1. Import Myers-Benford data from Excel files (02_ series)
# 2. Import CSI and nightlight data from DTA files
# 3. Merge datasets by region_number and ano
# 4. Run progressive analysis comparing V01 vs V03 versions

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

# Output path
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"

# Create output directory if it doesn't exist
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

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
                                     labels = c("Q1_Lowest", "Q2_Low", "Q3_Mid", "Q4_High", "Q5_Highest"))
  
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
    
    message("    Quintile distribution:")
    for(i in 1:nrow(quintile_summary)) {
      message("    ", quintile_summary$ntl_quintile_factor[i], ": ", quintile_summary$n_obs[i], 
              " obs, NTL range: ", round(quintile_summary$min_ntl[i], 2), "-", round(quintile_summary$max_ntl[i], 2))
    }
  }
  
  return(data)
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
    csi_vars <- names(data)[grepl("csi", names(data), ignore.case = TRUE)]
    ntl_vars <- names(data)[grepl("ntl", names(data), ignore.case = TRUE)]
    urban_vars <- names(data)[grepl("urban", names(data), ignore.case = TRUE)]
    
    message("  📊 Available variable types:")
    message("    - CSI variables: ", length(csi_vars), " (", paste(head(csi_vars, 3), collapse = ", "), "...)")
    message("    - NTL variables: ", length(ntl_vars), " (", paste(ntl_vars, collapse = ", "), ")")
    message("    - Urban variables: ", length(urban_vars), " (", paste(urban_vars, collapse = ", "), ")")
    
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

# Function to merge Myers-Benford with CSI-NTL data
merge_datasets <- function(myers_benford_data, csi_ntl_data, level, version) {
  message("Merging datasets for ", level, " level, version ", version, "...")
  
  if (is.null(myers_benford_data) || is.null(csi_ntl_data)) {
    message("  ❌ Cannot merge - one or both datasets are NULL")
    return(NULL)
  }
  
  # Check merge keys
  merge_keys <- c("region_number", "ano")
  
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
  
  # Perform merge
  merged_data <- merge(myers_benford_data, csi_ntl_data, 
                       by = merge_keys, all.x = TRUE, suffixes = c("_excel", "_dta"))
  
  message("  📊 Merge results:")
  message("    - Myers-Benford observations: ", nrow(myers_benford_data))
  message("    - CSI-NTL observations: ", nrow(csi_ntl_data))
  message("    - Merged observations: ", nrow(merged_data))
  message("    - Successful matches: ", sum(!is.na(merged_data$ntl_average_masked)))
  
  # Add version and merge information
  merged_data$version <- version
  merged_data$level <- level
  merged_data$merge_success <- !is.na(merged_data$ntl_average_masked)
  
  # Create after_peace_treaty variable
  merged_data$after_peace_treaty <- ifelse(merged_data$ano >= 2016, 1, 0)
  
  # Create nighttime lights quintiles
  merged_data <- create_ntl_quintiles(merged_data)
  
  message("  ✅ Successfully merged datasets")
  message("    - Complete observations (Myers + Benford + NTL): ", 
          sum(complete.cases(merged_data[, c("standardized_myers", "benford_abs_distance", "ntl_average_masked")])))
  
  return(merged_data)
}

########################################
# PROGRESSIVE MODEL SPECIFICATIONS
########################################

# Function to define model specifications
get_progressive_model_specs <- function(level = "dept", version = "V01") {
  
  myers_var <- "standardized_myers"
  benford_var <- "benford_abs_distance"
  
  if (level == "dept") {
    # Department level models
    models <- list(
      "Model 1: Simple Bivariate" = 
        paste(benford_var, "~", myers_var),
      
      "Model 2: + Peace Treaty" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty"),
      
      "Model 3: Myers × Peace Treaty" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty"),
      
      "Model 4: + NTL Quintiles" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty + ntl_quintile_factor"),
      
      "Model 5: Myers × NTL Quintiles" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty + ntl_quintile_factor +",
              myers_var, ": ntl_quintile_factor"),
      
      "Model 6: + CSI Controls" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty + ntl_quintile_factor +",
              myers_var, ": ntl_quintile_factor + csi_pre1500_avg_no0"),
      
      "Model 7: + Urban Share Control" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty + ntl_quintile_factor +",
              myers_var, ": ntl_quintile_factor + csi_pre1500_avg_no0 + urban_share"),
      
      "Model 8: Triple Interaction (Peace × NTL)" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty + ntl_quintile_factor + csi_pre1500_avg_no0 + urban_share +",
              myers_var, ": after_peace_treaty +", myers_var, ": ntl_quintile_factor +",
              "after_peace_treaty : ntl_quintile_factor +",
              myers_var, ": after_peace_treaty : ntl_quintile_factor")
    )
    
  } else if (level == "dept_area") {
    # Department-area level models
    models <- list(
      "Model 1: Simple Bivariate" = 
        paste(benford_var, "~", myers_var),
      
      "Model 2: + Peace Treaty" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty"),
      
      "Model 3: Myers × Peace Treaty" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty"),
      
      "Model 4: + NTL Quintiles" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty + ntl_quintile_factor"),
      
      "Model 5: Myers × NTL Quintiles" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty + ntl_quintile_factor +",
              myers_var, ": ntl_quintile_factor"),
      
      "Model 6: + CSI Controls" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty + ntl_quintile_factor +",
              myers_var, ": ntl_quintile_factor + csi_pre1500_avg_no0"),
      
      "Model 7: + Urban Control & Interaction" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty +", 
              myers_var, ": after_peace_treaty + ntl_quintile_factor +",
              myers_var, ": ntl_quintile_factor + csi_pre1500_avg_no0 + urban_area +", myers_var, ": urban_area"),
      
      "Model 8: Triple Interaction (Peace × NTL)" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty + ntl_quintile_factor + csi_pre1500_avg_no0 + urban_area +",
              myers_var, ": after_peace_treaty +", myers_var, ": ntl_quintile_factor +", myers_var, ": urban_area +",
              "after_peace_treaty : ntl_quintile_factor +",
              myers_var, ": after_peace_treaty : ntl_quintile_factor"),
      
      "Model 9: Quadruple Interaction (Peace × NTL × Urban)" = 
        paste(benford_var, "~", myers_var, "+ after_peace_treaty + ntl_quintile_factor + csi_pre1500_avg_no0 + urban_area +",
              myers_var, ": after_peace_treaty +", myers_var, ": ntl_quintile_factor +", myers_var, ": urban_area +",
              "after_peace_treaty : ntl_quintile_factor + after_peace_treaty : urban_area +",
              myers_var, ": after_peace_treaty : ntl_quintile_factor +", myers_var, ": after_peace_treaty : urban_area +",
              myers_var, ": after_peace_treaty : ntl_quintile_factor : urban_area")
    )
  }
  
  return(models)
}

########################################
# REGRESSION ANALYSIS FUNCTION
########################################

run_progressive_regression_merged <- function(data, level = "dept", version = "V01") {
  message("Running progressive regression analysis for ", level, " level, version ", version, "...")
  
  # Get model specifications
  model_specs <- get_progressive_model_specs(level, version)
  
  # Check clustering viability
  cluster_table <- table(data$region_number)
  min_cluster_size <- min(cluster_table)
  n_clusters <- length(cluster_table)
  
  message("  Clustering info: ", n_clusters, " regions, cluster sizes: ", 
          min(cluster_table), "-", max(cluster_table), " observations")
  
  # Function to run regression with clustered standard errors
  run_clustered_regression <- function(formula_str, data, include_fe = FALSE, model_name = "") {
    tryCatch({
      
      # Filter data for variables in the model
      model_vars <- all.vars(as.formula(formula_str))
      model_data <- data[complete.cases(data[, model_vars]), ]
      
      if (nrow(model_data) < 10) {
        message("    Insufficient data for ", model_name)
        return(NULL)
      }
      
      # Add fixed effects if requested
      if (include_fe && length(unique(model_data$region_number)) > 1 && length(unique(model_data$ano)) > 1) {
        if (grepl("after_peace_treaty", formula_str)) {
          formula_str <- paste(formula_str, "+ factor(region_number)")
          message("    Using only region FE for ", model_name, " (avoiding collinearity)")
        } else {
          formula_str <- paste(formula_str, "+ factor(region_number) + factor(ano)")
          message("    Using region + year FE for ", model_name)
        }
      }
      
      # Fit the model
      model <- lm(as.formula(formula_str), data = model_data)
      
      # Check for successful model fitting
      if (any(is.na(coef(model)))) {
        message("    Collinearity detected in ", model_name)
        return(NULL)
      }
      
      # Calculate clustered standard errors
      model_fitted_data <- model$model
      region_cluster <- model_data$region_number[as.numeric(rownames(model_fitted_data))]
      
      clustered_vcov <- vcovCL(model, cluster = region_cluster)
      clustered_se <- sqrt(diag(clustered_vcov))
      
      # Get coefficient estimates
      coefficients <- coef(model)
      
      # Calculate t-statistics and p-values
      t_stats <- coefficients / clustered_se
      p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)
      
      # Create results data frame
      results_df <- data.frame(
        term = names(coefficients),
        estimate = coefficients,
        std.error = clustered_se,
        statistic = t_stats,
        p.value = p_values,
        significance = sapply(p_values, add_significance_stars),
        stringsAsFactors = FALSE
      )
      
      # Add model statistics
      model_stats <- data.frame(
        r.squared = summary(model)$r.squared,
        adj.r.squared = summary(model)$adj.r.squared,
        nobs = nobs(model),
        stringsAsFactors = FALSE
      )
      
      return(list(coefficients = results_df, model_stats = model_stats))
      
    }, error = function(e) {
      message("    Error in ", model_name, ": ", e$message)
      return(NULL)
    })
  }
  
  # Run regressions without fixed effects
  results_no_fe <- data.frame()
  model_stats_no_fe <- data.frame()
  
  for (i in seq_along(model_specs)) {
    model_name <- names(model_specs)[i]
    formula_str <- model_specs[[i]]
    
    message("  Running ", model_name, " without FE...")
    result <- run_clustered_regression(formula_str, data, include_fe = FALSE, model_name)
    
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
  
  # Run regressions with fixed effects
  results_fe <- data.frame()
  model_stats_fe <- data.frame()
  
  if (length(unique(data$region_number)) > 1 && length(unique(data$ano)) > 1) {
    for (i in seq_along(model_specs)) {
      model_name <- names(model_specs)[i]
      formula_str <- model_specs[[i]]
      
      message("  Running ", model_name, " with FE...")
      result <- run_clustered_regression(formula_str, data, include_fe = TRUE, model_name)
      
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
  }
  
  return(list(
    coefficients_no_fe = results_no_fe,
    model_stats_no_fe = model_stats_no_fe,
    coefficients_fe = results_fe,
    model_stats_fe = model_stats_fe
  ))
}

########################################
# MAIN PROCESSING FUNCTION
########################################

process_complete_analysis <- function() {
  message("Starting complete Myers-Benford analysis with Excel-DTA merger...")
  
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
      
      # Merge datasets
      merged_data <- merge_datasets(myers_benford_data, csi_ntl_data, level, version)
      
      if (!is.null(merged_data)) {
        # Store merged data
        data_key <- paste(level, version, sep = "_")
        all_merged_data[[data_key]] <- merged_data
        
        # Run regression analysis
        reg_results <- run_progressive_regression_merged(merged_data, level, version)
        all_regression_results[[data_key]] <- reg_results
        
        message("✅ Completed processing for ", level, " level, version ", version)
        message("   - Final dataset: ", nrow(merged_data), " observations")
        message("   - Models without FE: ", length(unique(reg_results$coefficients_no_fe$Model)))
        message("   - Models with FE: ", length(unique(reg_results$coefficients_fe$Model)))
      }
    }
  }
  
  # Create comprehensive Excel outputs
  create_comprehensive_output(all_merged_data, all_regression_results)
  
  message("\n", paste(rep("=", 80), collapse=""))
  message("✅ COMPLETE ANALYSIS FINISHED")
  message(paste(rep("=", 80), collapse=""))
}

# Function to create comprehensive Excel output
create_comprehensive_output <- function(all_merged_data, all_regression_results) {
  message("\nCreating comprehensive Excel outputs...")
  
  # Create separate files for each level
  for (level in c("dept", "dept_area")) {
    
    message("Creating output for ", level, " level...")
    
    # Prepare Excel data
    excel_data <- list()
    
    # Summary sheet
    summary_text <- paste0("Colombia Myers-Benford Analysis - COMPLETE VERSION\n")
    summary_text <- paste0(summary_text, "Level: ", toupper(level), " | Date: ", Sys.Date(), "\n")
    summary_text <- paste0(summary_text, paste(rep("=", 80), collapse=""), "\n\n")
    summary_text <- paste0(summary_text, "DATA SOURCES:\n")
    summary_text <- paste0(summary_text, "1. Myers-Benford data: Excel files (02_ series)\n")
    summary_text <- paste0(summary_text, "2. CSI and Nightlight data: DTA files\n")
    summary_text <- paste0(summary_text, "3. Merged by region_number and ano\n\n")
    summary_text <- paste0(summary_text, "VERSION COMPARISON:\n")
    summary_text <- paste0(summary_text, "V01: Including outliers and zeros in income variable\n")
    summary_text <- paste0(summary_text, "V03: Excluding outliers (interquartile method), no zero income\n\n")
    summary_text <- paste0(summary_text, "KEY FEATURES:\n")
    summary_text <- paste0(summary_text, "- Progressive models from simple to complex interactions\n")
    summary_text <- paste0(summary_text, "- CSI controls (pre-1500 state capacity)\n")
    summary_text <- paste0(summary_text, "- Nighttime lights quintiles (development proxy)\n")
    summary_text <- paste0(summary_text, "- Peace treaty interactions (post-2016)\n")
    summary_text <- paste0(summary_text, "- Standard errors clustered by region\n")
    summary_text <- paste0(summary_text, "- Fixed effects handling collinearity\n\n")
    summary_text <- paste0(summary_text, "*** p<0.01, ** p<0.05, * p<0.10\n")
    
    excel_data[["Summary"]] <- data.frame(
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
      
      # Sample merged data
      if (paste(level, "V01", sep = "_") %in% names(all_merged_data)) {
        sample_data <- all_merged_data[[paste(level, "V01", sep = "_")]]
        key_vars <- c("region_number", "ano", "standardized_myers", "benford_abs_distance", 
                      "ntl_average_masked", "csi_pre1500_avg_no0", "after_peace_treaty", 
                      "ntl_quintile_factor")
        available_vars <- key_vars[key_vars %in% names(sample_data)]
        
        if (length(available_vars) > 0) {
          excel_data[["Sample_Merged_Data"]] <- head(sample_data[, available_vars], 20)
        }
      }
    }
    
    # Remove empty sheets
    excel_data <- excel_data[sapply(excel_data, function(x) nrow(x) > 0)]
    
    # Write Excel file
    output_filename <- paste0("05_COMPLETE_Myers_Benford_CSI_NTL_", level, ".xlsx")
    output_file_path <- file.path(output_path, output_filename)
    
    writexl::write_xlsx(excel_data, output_file_path)
    message("✅ Saved: ", output_filename)
  }
}

########################################
# MAIN EXECUTION
########################################

# Run the complete analysis
process_complete_analysis()

message("\n🎯 COMPLETE ANALYSIS SUMMARY:")
message("📊 Successfully merged Excel (Myers-Benford) with DTA (CSI-NTL) data")
message("📈 Compared V01 (with outliers) vs V03 (cleaned) versions")
message("🔍 Progressive models: Simple → Peace Treaty → NTL Quintiles → CSI Controls → Complex Interactions")
message("📁 Output files:")
message("   📊 05_COMPLETE_Myers_Benford_CSI_NTL_dept.xlsx")
message("   📊 05_COMPLETE_Myers_Benford_CSI_NTL_dept_area.xlsx")

message("\n📋 EXCEL SHEET ORGANIZATION:")
message("   📄 Summary: Complete analysis overview")
message("   📊 All_Coefficients_No_FE: Combined V01+V03 results (no FE)")
message("   📊 All_Coefficients_FE: Combined V01+V03 results (with FE)")
message("   📈 V01_Coefficients_No_FE: V01 results without FE")
message("   📈 V01_Coefficients_FE: V01 results with FE")
message("   📈 V03_Coefficients_No_FE: V03 results without FE")
message("   📈 V03_Coefficients_FE: V03 results with FE")
message("   👁️ Sample_Merged_Data: Preview of merged dataset")

message("\n💡 KEY INNOVATIONS:")
message("   🔗 Successfully merged Excel + DTA data sources")
message("   📊 Added CSI controls (pre-1500 state capacity)")
message("   🌙 Nighttime lights quintiles (development proxy)")
message("   🕊️ Peace treaty interactions (natural experiment)")
message("   📈 Robust V01 vs V03 comparison")
message("   🎯 Ready for publication-quality analysis!")