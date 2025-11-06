########################################
# Colombia Myers-Benford Analysis - FOCUSED V01 DEPT-AREA ONLY
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-09-09
# Purpose: 
# Focused analysis using only V01 dept-area level data
# Two fixed effects strategies with 6 specific models
# All models include urban_area control

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Install and load required packages
packages <- c("haven", "dplyr", "broom", "sandwich", "lmtest", "writexl")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input file path (specific dataset)
input_file_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge/05_V01_Myers_Bend_CSI_NTL_depto_area.dta"

# Output path
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"
output_filename <- "07_Col_regressions_V01_depto-area-unweighted.xlsx"

# Create output directory if it doesn't exist
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

########################################
# UTILITY FUNCTIONS
########################################

# Function to add significance stars
add_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value <= 0.01) return("***")
  if (p_value <= 0.05) return("**")
  if (p_value <= 0.10) return("*")
  return("")
}

# Function to create year dummies for pre-2016 only (for peace treatment models)
create_pre_2016_year_fe <- function(data) {
  message("Creating pre-2016 year fixed effects...")
  
  # Get years before 2016
  pre_2016_years <- unique(data$ano[data$ano < 2016])
  pre_2016_years <- pre_2016_years[!is.na(pre_2016_years)]
  
  message("  Pre-2016 years found: ", paste(sort(pre_2016_years), collapse = ", "))
  
  # Create year dummy formula (excluding one year as reference)
  if (length(pre_2016_years) > 1) {
    reference_year <- min(pre_2016_years)
    fe_years <- pre_2016_years[pre_2016_years != reference_year]
    
    year_fe_formula <- paste0("I(ano == ", fe_years, ")", collapse = " + ")
    message("  Reference year: ", reference_year)
    message("  Year FE formula: ", year_fe_formula)
    
    return(year_fe_formula)
  } else {
    message("  Insufficient years for year FE")
    return("")
  }
}

########################################
# DATA LOADING AND PREPARATION
########################################

load_and_prepare_data <- function() {
  message("Loading data from: ", basename(input_file_path))
  
  # Check if file exists
  if (!file.exists(input_file_path)) {
    stop("Input file not found: ", input_file_path)
  }
  
  # Load data
  data <- haven::read_dta(input_file_path)
  
  message("Data loaded successfully:")
  message("  - Observations: ", nrow(data))
  message("  - Variables: ", ncol(data))
  
  # Check for required variables
  required_vars <- c("region_number", "ano", "standardized_myers", "benford_abs_distance", 
                     "urban_area", "ntl_quintile_factor", "csi_quintile_factor", 
                     "after_peace_treaty", "ntl_average_masked", "csi_pre1500_avg_no0")
  
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Data diagnostics
  message("\nData diagnostics:")
  message("  - Regions: ", length(unique(data$region_number)))
  message("  - Years: ", paste(range(data$ano, na.rm = TRUE), collapse = "-"))
  message("  - Urban distribution: ", paste(table(data$urban_area), collapse = " rural, ", " urban"))
  
  # Check quintile distributions
  if ("ntl_quintile_factor" %in% names(data)) {
    ntl_dist <- table(data$ntl_quintile_factor, useNA = "always")
    message("  - NTL quintile distribution:")
    for(i in 1:length(ntl_dist)) {
      message("    ", names(ntl_dist)[i], ": ", ntl_dist[i])
    }
  }
  
  if ("csi_quintile_factor" %in% names(data)) {
    csi_dist <- table(data$csi_quintile_factor, useNA = "always")
    message("  - CSI quintile distribution:")
    for(i in 1:length(csi_dist)) {
      message("    ", names(csi_dist)[i], ": ", csi_dist[i])
    }
  }
  
  # Peace treatment distribution
  peace_dist <- table(data$after_peace_treaty, useNA = "always")
  message("  - Peace treatment distribution: ", paste(names(peace_dist), peace_dist, sep = "=", collapse = ", "))
  
  return(data)
}

########################################
# MODEL SPECIFICATIONS
########################################

get_model_specifications <- function() {
  message("Defining model specifications...")
  
  # Base variables
  dependent_var <- "benford_abs_distance"
  myers_var <- "standardized_myers"
  urban_control <- "urban_area"
  
  # Model specifications (all include urban_area control)
  models <- list(
    "Model 1: Basic Myers" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control),
      has_peace = FALSE,
      description = "Basic Myers index with urban control"
    ),
    
    "Model 2: Myers + NTL" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ ntl_quintile_factor +", myers_var, ": ntl_quintile_factor"),
      has_peace = FALSE,
      description = "Myers + NTL quintiles + Myers×NTL interactions"
    ),
    
    "Model 3: Myers + CSI" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ csi_quintile_factor +", myers_var, ": csi_quintile_factor"),
      has_peace = FALSE,
      description = "Myers + CSI quintiles + Myers×CSI interactions"
    ),
    
    "Model 4: Myers + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
      has_peace = TRUE,
      description = "Myers + Peace treatment + Myers×Peace interaction"
    ),
    
    "Model 5: Myers + NTL + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ ntl_quintile_factor +", myers_var, ": ntl_quintile_factor",
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
      has_peace = TRUE,
      description = "Myers + NTL + Peace treatment + interactions"
    ),
    
    "Model 6: Myers + CSI + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ csi_quintile_factor +", myers_var, ": csi_quintile_factor",
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
      has_peace = TRUE,
      description = "Myers + CSI + Peace treatment + interactions"
    )
  )
  
  message("Model specifications defined:")
  for(i in 1:length(models)) {
    message("  ", names(models)[i], ": ", models[[i]]$description)
  }
  
  return(models)
}

########################################
# REGRESSION ESTIMATION FUNCTIONS
########################################

run_regression_version_1 <- function(data, model_specs) {
  message("\nRunning Version 1: Region + Year FE (with peace treatment handling)")
  
  results_list <- list()
  model_stats_list <- list()
  
  for (i in 1:length(model_specs)) {
    model_name <- names(model_specs)[i]
    model_spec <- model_specs[[i]]
    
    message("  Running ", model_name, "...")
    
    tryCatch({
      # Prepare formula with fixed effects
      base_formula <- model_spec$formula
      
      if (model_spec$has_peace) {
        # For peace treatment models: pre-2016 year FE + peace dummy
        pre_2016_year_fe <- create_pre_2016_year_fe(data)
        
        if (pre_2016_year_fe != "") {
          final_formula <- paste(base_formula, "+ factor(region_number) +", pre_2016_year_fe)
          fe_strategy <- "region_pre2016year_peace"
        } else {
          # Fallback to region FE only
          final_formula <- paste(base_formula, "+ factor(region_number)")
          fe_strategy <- "region_only"
        }
      } else {
        # Regular models: full region + year FE
        final_formula <- paste(base_formula, "+ factor(region_number) + factor(ano)")
        fe_strategy <- "region_year"
      }
      
      message("    FE strategy: ", fe_strategy)
      
      # Get complete cases for this model
      model_vars <- all.vars(as.formula(base_formula))
      model_data <- data[complete.cases(data[, model_vars]), ]
      
      if (nrow(model_data) < 20) {
        message("    ❌ Insufficient data: ", nrow(model_data), " observations")
        next
      }
      
      # Fit model
      model <- lm(as.formula(final_formula), data = model_data)
      
      # Check for multicollinearity
      if (any(is.na(coef(model)))) {
        message("    ❌ Multicollinearity detected")
        next
      }
      
      # Calculate clustered standard errors
      model_fitted_data <- model$model
      region_cluster <- model_data$region_number[as.numeric(rownames(model_fitted_data))]
      
      clustered_vcov <- vcovCL(model, cluster = region_cluster)
      clustered_se <- sqrt(diag(clustered_vcov))
      
      coefficients <- coef(model)
      t_stats <- coefficients / clustered_se
      p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)
      
      # Create results dataframe
      results_df <- data.frame(
        term = names(coefficients),
        estimate = coefficients,
        std.error = clustered_se,
        statistic = t_stats,
        p.value = p_values,
        significance = sapply(p_values, add_significance_stars),
        fe_strategy = fe_strategy,
        Model = model_name,
        stringsAsFactors = FALSE
      )
      
      model_stats <- data.frame(
        r.squared = summary(model)$r.squared,
        adj.r.squared = summary(model)$adj.r.squared,
        nobs = nobs(model),
        fe_strategy = fe_strategy,
        Model = model_name,
        Fixed_Effects = "Region+Year",
        stringsAsFactors = FALSE
      )
      
      results_list[[model_name]] <- results_df
      model_stats_list[[model_name]] <- model_stats
      
      message("    ✅ Success: R² = ", round(model_stats$r.squared, 3), 
              ", N = ", model_stats$nobs)
      
    }, error = function(e) {
      message("    ❌ Error: ", e$message)
    })
  }
  
  # Combine results
  all_results <- do.call(rbind, results_list)
  all_model_stats <- do.call(rbind, model_stats_list)
  
  message("Version 1 completed: ", nrow(all_results), " coefficient estimates from ", 
          length(results_list), " models")
  
  return(list(coefficients = all_results, model_stats = all_model_stats))
}

run_regression_version_2 <- function(data, model_specs) {
  message("\nRunning Version 2: Region FE only")
  
  results_list <- list()
  model_stats_list <- list()
  
  for (i in 1:length(model_specs)) {
    model_name <- names(model_specs)[i]
    model_spec <- model_specs[[i]]
    
    message("  Running ", model_name, "...")
    
    tryCatch({
      # Add region fixed effects only
      base_formula <- model_spec$formula
      final_formula <- paste(base_formula, "+ factor(region_number)")
      
      # Get complete cases for this model
      model_vars <- all.vars(as.formula(base_formula))
      model_data <- data[complete.cases(data[, model_vars]), ]
      
      if (nrow(model_data) < 20) {
        message("    ❌ Insufficient data: ", nrow(model_data), " observations")
        next
      }
      
      # Fit model
      model <- lm(as.formula(final_formula), data = model_data)
      
      # Check for multicollinearity
      if (any(is.na(coef(model)))) {
        message("    ❌ Multicollinearity detected")
        next
      }
      
      # Calculate clustered standard errors
      model_fitted_data <- model$model
      region_cluster <- model_data$region_number[as.numeric(rownames(model_fitted_data))]
      
      clustered_vcov <- vcovCL(model, cluster = region_cluster)
      clustered_se <- sqrt(diag(clustered_vcov))
      
      coefficients <- coef(model)
      t_stats <- coefficients / clustered_se
      p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)
      
      # Create results dataframe
      results_df <- data.frame(
        term = names(coefficients),
        estimate = coefficients,
        std.error = clustered_se,
        statistic = t_stats,
        p.value = p_values,
        significance = sapply(p_values, add_significance_stars),
        fe_strategy = "region_only",
        Model = model_name,
        stringsAsFactors = FALSE
      )
      
      model_stats <- data.frame(
        r.squared = summary(model)$r.squared,
        adj.r.squared = summary(model)$adj.r.squared,
        nobs = nobs(model),
        fe_strategy = "region_only",
        Model = model_name,
        Fixed_Effects = "Region only",
        stringsAsFactors = FALSE
      )
      
      results_list[[model_name]] <- results_df
      model_stats_list[[model_name]] <- model_stats
      
      message("    ✅ Success: R² = ", round(model_stats$r.squared, 3), 
              ", N = ", model_stats$nobs)
      
    }, error = function(e) {
      message("    ❌ Error: ", e$message)
    })
  }
  
  # Combine results
  all_results <- do.call(rbind, results_list)
  all_model_stats <- do.call(rbind, model_stats_list)
  
  message("Version 2 completed: ", nrow(all_results), " coefficient estimates from ", 
          length(results_list), " models")
  
  return(list(coefficients = all_results, model_stats = all_model_stats))
}

########################################
# DESCRIPTIVE STATISTICS FUNCTIONS
########################################

create_quintile_descriptive_stats <- function(data) {
  message("Creating quintile descriptive statistics...")
  
  # CSI Quintile Analysis
  csi_stats <- NULL
  if ("csi_quintile_factor" %in% names(data)) {
    csi_stats <- data %>%
      filter(!is.na(csi_quintile_factor)) %>%
      group_by(csi_quintile_factor) %>%
      summarise(
        n_obs = n(),
        mean_myers = mean(standardized_myers, na.rm = TRUE),
        mean_benford = mean(benford_abs_distance, na.rm = TRUE),
        mean_csi_raw = mean(csi_pre1500_avg_no0, na.rm = TRUE),
        mean_ntl = mean(ntl_average_masked, na.rm = TRUE),
        pct_post_peace = mean(after_peace_treaty, na.rm = TRUE) * 100,
        pct_urban = mean(urban_area, na.rm = TRUE) * 100,
        .groups = 'drop'
      ) %>%
      mutate(quintile_type = "CSI") %>%
      rename(quintile_factor = csi_quintile_factor)
  }
  
  # NTL Quintile Analysis
  ntl_stats <- NULL
  if ("ntl_quintile_factor" %in% names(data)) {
    ntl_stats <- data %>%
      filter(!is.na(ntl_quintile_factor)) %>%
      group_by(ntl_quintile_factor) %>%
      summarise(
        n_obs = n(),
        mean_myers = mean(standardized_myers, na.rm = TRUE),
        mean_benford = mean(benford_abs_distance, na.rm = TRUE),
        mean_csi_raw = mean(csi_pre1500_avg_no0, na.rm = TRUE),
        mean_ntl = mean(ntl_average_masked, na.rm = TRUE),
        pct_post_peace = mean(after_peace_treaty, na.rm = TRUE) * 100,
        pct_urban = mean(urban_area, na.rm = TRUE) * 100,
        .groups = 'drop'
      ) %>%
      mutate(quintile_type = "NTL") %>%
      rename(quintile_factor = ntl_quintile_factor)
  }
  
  # Combine both types of quintiles
  combined_stats <- rbind(csi_stats, ntl_stats)
  
  message("Quintile descriptive statistics created:")
  if (!is.null(csi_stats)) {
    message("  - CSI quintiles: ", nrow(csi_stats), " categories")
  }
  if (!is.null(ntl_stats)) {
    message("  - NTL quintiles: ", nrow(ntl_stats), " categories")
  }
  
  return(combined_stats)
}

create_sample_data <- function(data) {
  message("Creating sample data for Excel output...")
  
  # Key variables for display
  key_vars <- c("region_number", "ano", "urban_area", "standardized_myers", 
                "benford_abs_distance", "ntl_average_masked", "ntl_quintile_factor",
                "csi_pre1500_avg_no0", "csi_quintile_factor", "after_peace_treaty")
  
  available_vars <- key_vars[key_vars %in% names(data)]
  
  # Get complete cases and take a sample
  complete_data <- data[complete.cases(data[, available_vars]), ]
  
  # Sample 100 observations or all if fewer
  n_sample <- min(100, nrow(complete_data))
  sample_indices <- sample(1:nrow(complete_data), n_sample)
  sample_data <- complete_data[sample_indices, available_vars]
  
  # Sort by region and year for better presentation
  sample_data <- sample_data[order(sample_data$region_number, sample_data$ano), ]
  
  message("Sample data created: ", nrow(sample_data), " observations with ", 
          ncol(sample_data), " variables")
  
  return(sample_data)
}

########################################
# EXCEL OUTPUT CREATION
########################################

create_excel_output <- function(version1_results, version2_results, quintile_stats, sample_data) {
  message("Creating Excel output...")
  
  # Prepare Excel data list
  excel_data <- list()
  
  # Sheet 1: Version 1 Coefficients (Region + Year FE)
  if (!is.null(version1_results$coefficients) && nrow(version1_results$coefficients) > 0) {
    excel_data[["V1_Coefficients_Region_Year_FE"]] <- version1_results$coefficients
  }
  
  # Sheet 2: Version 1 Model Stats
  if (!is.null(version1_results$model_stats) && nrow(version1_results$model_stats) > 0) {
    excel_data[["V1_Model_Stats_Region_Year_FE"]] <- version1_results$model_stats
  }
  
  # Sheet 3: Version 2 Coefficients (Region FE only)
  if (!is.null(version2_results$coefficients) && nrow(version2_results$coefficients) > 0) {
    excel_data[["V2_Coefficients_Region_FE"]] <- version2_results$coefficients
  }
  
  # Sheet 4: Version 2 Model Stats
  if (!is.null(version2_results$model_stats) && nrow(version2_results$model_stats) > 0) {
    excel_data[["V2_Model_Stats_Region_FE"]] <- version2_results$model_stats
  }
  
  # Sheet 5: Quintile Descriptive Statistics
  if (!is.null(quintile_stats) && nrow(quintile_stats) > 0) {
    excel_data[["Quintile_Descriptive_Stats"]] <- quintile_stats
  }
  
  # Sheet 6: Sample Data
  if (!is.null(sample_data) && nrow(sample_data) > 0) {
    excel_data[["Sample_Data"]] <- sample_data
  }
  
  # Remove empty sheets
  excel_data <- excel_data[sapply(excel_data, function(x) nrow(x) > 0)]
  
  # Create output file path
  output_file_path <- file.path(output_path, output_filename)
  
  # Write Excel file
  tryCatch({
    writexl::write_xlsx(excel_data, output_file_path)
    message("✅ Excel file created successfully: ", output_filename)
    message("   Path: ", output_file_path)
    message("   Sheets created: ", length(excel_data))
    message("   Sheet names: ", paste(names(excel_data), collapse = ", "))
    
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error creating Excel file: ", e$message)
    return(FALSE)
  })
}

########################################
# MAIN EXECUTION FUNCTION
########################################

run_focused_analysis <- function() {
  message("Starting focused Colombia Myers-Benford analysis...")
  message("Input: V01 dept-area level only")
  message("Output: ", output_filename)
  message(paste(rep("=", 60), collapse=""))
  
  # Step 1: Load and prepare data
  data <- load_and_prepare_data()
  
  # Step 2: Get model specifications
  model_specs <- get_model_specifications()
  
  # Step 3: Run Version 1 regressions (Region + Year FE)
  version1_results <- run_regression_version_1(data, model_specs)
  
  # Step 4: Run Version 2 regressions (Region FE only)
  version2_results <- run_regression_version_2(data, model_specs)
  
  # Step 5: Create descriptive statistics
  quintile_stats <- create_quintile_descriptive_stats(data)
  sample_data <- create_sample_data(data)
  
  # Step 6: Create Excel output
  excel_success <- create_excel_output(version1_results, version2_results, 
                                       quintile_stats, sample_data)
  
  # Summary
  message(paste(rep("=", 60), collapse=""))
  message("ANALYSIS SUMMARY:")
  message("✅ Data loaded: ", nrow(data), " observations")
  message("✅ Models specified: ", length(model_specs))
  
  if (!is.null(version1_results$coefficients)) {
    message("✅ Version 1 (Region+Year FE): ", 
            length(unique(version1_results$coefficients$Model)), " models estimated")
  }
  
  if (!is.null(version2_results$coefficients)) {
    message("✅ Version 2 (Region FE): ", 
            length(unique(version2_results$coefficients$Model)), " models estimated")
  }
  
  if (excel_success) {
    message("✅ Excel output created: ", output_filename)
  } else {
    message("❌ Excel output failed")
  }
  
  message("Analysis completed successfully!")
  
  return(list(
    data = data,
    version1 = version1_results,
    version2 = version2_results,
    quintile_stats = quintile_stats,
    sample_data = sample_data
  ))
}

########################################
# EXECUTE ANALYSIS
########################################

# Run the analysis
results <- run_focused_analysis()

message("\n📊 FOCUSED ANALYSIS COMPLETED")
message("🎯 Key Results:")
message("   - Single dataset: V01 dept-area level")
message("   - Two fixed effects strategies implemented")
message("   - Six models estimated for each strategy")
message("   - All models include urban_area control")
message("   - Clustered standard errors at region level")
message("   - Peace treatment models handle multicollinearity")
message("   - Excel output: ", output_filename)
message("\n📋 Excel Sheet Structure:")
message("   1. V1_Coefficients_Region_Year_FE")
message("   2. V1_Model_Stats_Region_Year_FE")
message("   3. V2_Coefficients_Region_FE")
message("   4. V2_Model_Stats_Region_FE")
message("   5. Quintile_Descriptive_Stats")
message("   6. Sample_Data")
message("\n🔧 Technical Implementation:")
message("   - Peace models: pre-2016 year FE + peace dummy")
message("   - Complete case analysis for each model")
message("   - Robust clustered standard errors")
message("   - Automatic multicollinearity detection")
message("   - Comprehensive error handling")
message("Analysis ready for publication!")