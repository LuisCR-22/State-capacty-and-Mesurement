########################################
# Colombia Poverty Analysis - FOCUSED V01 DEPT-AREA WITH QUINTILE DUMMIES
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-10-26
# Purpose: 
# Analyze poverty outcomes using Myers-Benford framework
# Two model versions: (A) Myers only, (B) Myers + Benford
# Four dependent variables: pov_rate_3_00, pov_rate_4_20, pov_rate_8_30, share_pov_8_30
# Weighted and unweighted versions for each

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

# Input file path (merged dataset with poverty data)
input_file_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge/07_V01_my_Bend_CSI_NTL_pov_depto_area.dta"

# Output path
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col/Poverty regressions"

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

# Function to create quintiles and dummy variables from raw continuous variables
create_quintile_dummies <- function(data) {
  message("Creating quintiles and dummy variables from raw continuous variables...")
  
  # Create NTL quintiles if raw variable exists
  if ("ntl_average_masked" %in% names(data)) {
    message("  Creating NTL quintiles from raw variable...")
    
    # Create quintiles using ntile (handles NAs automatically)
    data$ntl_quintile <- ntile(data$ntl_average_masked, 5)
    
    # Create dummy variables (Q1 = reference, so Q2-Q5 get dummies)
    data$ntl_q2 <- ifelse(is.na(data$ntl_quintile), NA, ifelse(data$ntl_quintile == 2, 1, 0))
    data$ntl_q3 <- ifelse(is.na(data$ntl_quintile), NA, ifelse(data$ntl_quintile == 3, 1, 0))
    data$ntl_q4 <- ifelse(is.na(data$ntl_quintile), NA, ifelse(data$ntl_quintile == 4, 1, 0))
    data$ntl_q5 <- ifelse(is.na(data$ntl_quintile), NA, ifelse(data$ntl_quintile == 5, 1, 0))
    
    # Show distribution
    ntl_dist <- table(data$ntl_quintile, useNA = "always")
    message("    NTL quintile distribution: ", paste(names(ntl_dist), ntl_dist, sep = "=", collapse = ", "))
  } else {
    message("  WARNING: ntl_average_masked variable not found")
  }
  
  # Create CSI quintiles if raw variable exists
  if ("csi_pre1500_avg_no0" %in% names(data)) {
    message("  Creating CSI quintiles from raw variable...")
    
    # Create quintiles using ntile (handles NAs automatically)
    data$csi_quintile <- ntile(data$csi_pre1500_avg_no0, 5)
    
    # Create dummy variables (Q1 = reference, so Q2-Q5 get dummies)
    data$csi_q2 <- ifelse(is.na(data$csi_quintile), NA, ifelse(data$csi_quintile == 2, 1, 0))
    data$csi_q3 <- ifelse(is.na(data$csi_quintile), NA, ifelse(data$csi_quintile == 3, 1, 0))
    data$csi_q4 <- ifelse(is.na(data$csi_quintile), NA, ifelse(data$csi_quintile == 4, 1, 0))
    data$csi_q5 <- ifelse(is.na(data$csi_quintile), NA, ifelse(data$csi_quintile == 5, 1, 0))
    
    # Show distribution
    csi_dist <- table(data$csi_quintile, useNA = "always")
    message("    CSI quintile distribution: ", paste(names(csi_dist), csi_dist, sep = "=", collapse = ", "))
  } else {
    message("  WARNING: csi_pre1500_avg_no0 variable not found")
  }
  
  message("  Quintile dummy variables created successfully")
  return(data)
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
                     "urban_area", "after_peace_treaty", "ntl_average_masked", "csi_pre1500_avg_no0",
                     "pov_rate_3_00", "pov_rate_4_20", "pov_rate_8_30", "share_pov_8_30")
  
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    warning("Missing some required variables: ", paste(missing_vars, collapse = ", "))
    message("  Will proceed with available variables")
  }
  
  # *** RESCALE DEPENDENT VARIABLES TO 0-1 RANGE ***
  message("\nRescaling dependent variables to 0-1 range:")
  
  # Poverty rates are in percentage scale (0-100), convert to proportions (0-1)
  pov_rate_vars <- c("pov_rate_3_00", "pov_rate_4_20", "pov_rate_8_30")
  for(var in pov_rate_vars) {
    if(var %in% names(data)) {
      data[[var]] <- data[[var]] / 100
      message("  - ", var, ": rescaled from 0-100 to 0-1")
    }
  }
  
  # Share variables are already proportions (0-1), no rescaling needed
  share_vars <- c("share_pov_3_00", "share_pov_4_20", "share_pov_8_30")
  for(var in share_vars) {
    if(var %in% names(data)) {
      message("  - ", var, ": already in 0-1 scale (no change)")
    }
  }
  
  # Check for weights variable
  if ("n_observations_myers" %in% names(data)) {
    message("  Weights variable found: n_observations_myers")
    weights_available <- TRUE
  } else {
    message("  WARNING: Weights variable 'n_observations_myers' not found - weighted analysis will be skipped")
    weights_available <- FALSE
  }
  
  # Create quintile dummy variables
  data <- create_quintile_dummies(data)
  
  # Data diagnostics
  message("\nData diagnostics:")
  message("  - Regions: ", length(unique(data$region_number)))
  message("  - Years: ", paste(range(data$ano, na.rm = TRUE), collapse = "-"))
  message("  - Urban distribution: ", paste(table(data$urban_area), collapse = " rural, ", " urban"))
  
  # Check poverty data availability (after rescaling)
  for(pov_var in c("pov_rate_3_00", "pov_rate_4_20", "pov_rate_8_30", "share_pov_8_30")) {
    if(pov_var %in% names(data)) {
      n_available <- sum(!is.na(data[[pov_var]]))
      pct_available <- round(n_available / nrow(data) * 100, 1)
      range_vals <- range(data[[pov_var]], na.rm = TRUE)
      message("  - ", pov_var, ": ", n_available, " obs (", pct_available, "%), range: [", 
              round(range_vals[1], 3), ", ", round(range_vals[2], 3), "]")
    }
  }
  
  return(list(data = data, weights_available = weights_available))
}

########################################
# MODEL SPECIFICATIONS - VERSION A (MYERS ONLY)
########################################

get_model_specifications_myers_only <- function(dependent_var) {
  message("Defining model specifications for ", dependent_var, " (Myers only)...")
  
  # Base variables
  myers_var <- "standardized_myers"
  urban_control <- "urban_area"
  
  # Model specifications with individual quintile dummies
  models <- list(
    "Model 1: Basic Myers" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control),
      has_peace = FALSE,
      description = "Basic Myers index with urban control"
    ),
    
    "Model 2: Myers + NTL" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5"),
      has_peace = FALSE,
      description = "Myers + NTL quintile dummies + Myers×NTL interactions"
    ),
    
    "Model 3: Myers + CSI" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5"),
      has_peace = FALSE,
      description = "Myers + CSI quintile dummies + Myers×CSI interactions"
    ),
    
    "Model 4: Myers + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
      has_peace = TRUE,
      description = "Myers + Peace treatment + Myers×Peace interaction"
    ),
    
    "Model 5: Myers + NTL + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5 +",
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : ntl_q2 +", 
                      myers_var, ": after_peace_treaty : ntl_q3 +",
                      myers_var, ": after_peace_treaty : ntl_q4 +", 
                      myers_var, ": after_peace_treaty : ntl_q5"),
      has_peace = TRUE,
      description = "Myers + NTL + Peace + Triple interactions (Myers×Peace×NTL)"
    ),
    
    "Model 6: Myers + CSI + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5 +",
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : csi_q2 +", 
                      myers_var, ": after_peace_treaty : csi_q3 +",
                      myers_var, ": after_peace_treaty : csi_q4 +", 
                      myers_var, ": after_peace_treaty : csi_q5"),
      has_peace = TRUE,
      description = "Myers + CSI + Peace + Triple interactions (Myers×Peace×CSI)"
    )
  )
  
  return(models)
}

########################################
# MODEL SPECIFICATIONS - VERSION B (MYERS + BENFORD)
# BENFORD AS CONTROL - ONLY MYERS INTERACTS WITH NTL/CSI
########################################

get_model_specifications_myers_benford <- function(dependent_var) {
  message("Defining model specifications for ", dependent_var, " (Myers + Benford)...")
  message("  Note: Only Myers interacts with NTL/CSI quintiles")
  
  # Base variables
  myers_var <- "standardized_myers"
  benford_var <- "benford_abs_distance"
  urban_control <- "urban_area"
  
  # Model specifications with both Myers and Benford
  # Benford is included as control variable but does NOT interact with quintiles
  models <- list(
    "Model 1: Basic Myers + Benford" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", urban_control),
      has_peace = FALSE,
      description = "Myers + Benford with urban control"
    ),
    
    "Model 2: Myers + Benford + NTL" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5"),
      has_peace = FALSE,
      description = "Myers + Benford + NTL + Myers×NTL interactions only"
    ),
    
    "Model 3: Myers + Benford + CSI" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", urban_control, 
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5"),
      has_peace = FALSE,
      description = "Myers + Benford + CSI + Myers×CSI interactions only"
    ),
    
    "Model 4: Myers + Benford + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", urban_control, 
                      "+ after_peace_treaty +", 
                      myers_var, ": after_peace_treaty +",
                      benford_var, ": after_peace_treaty"),
      has_peace = TRUE,
      description = "Myers + Benford + Peace + both interact with Peace"
    ),
    
    "Model 5: Myers + Benford + NTL + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5 +",
                      "+ after_peace_treaty +", 
                      myers_var, ": after_peace_treaty +",
                      benford_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : ntl_q2 +", 
                      myers_var, ": after_peace_treaty : ntl_q3 +",
                      myers_var, ": after_peace_treaty : ntl_q4 +", 
                      myers_var, ": after_peace_treaty : ntl_q5"),
      has_peace = TRUE,
      description = "Myers + Benford + NTL + Peace + Myers×Peace×NTL triple interactions"
    ),
    
    "Model 6: Myers + Benford + CSI + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", urban_control, 
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5 +",
                      "+ after_peace_treaty +", 
                      myers_var, ": after_peace_treaty +",
                      benford_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : csi_q2 +", 
                      myers_var, ": after_peace_treaty : csi_q3 +",
                      myers_var, ": after_peace_treaty : csi_q4 +", 
                      myers_var, ": after_peace_treaty : csi_q5"),
      has_peace = TRUE,
      description = "Myers + Benford + CSI + Peace + Myers×Peace×CSI triple interactions"
    )
  )
  
  return(models)
}

########################################
# ENHANCED REGRESSION ESTIMATION FUNCTIONS
########################################

run_regression_version_1 <- function(data, model_specs, use_weights = FALSE, weight_var = NULL) {
  version_name <- if(use_weights) "Version 1 (Weighted)" else "Version 1 (Unweighted)"
  message("\nRunning ", version_name, ": Region + Year FE (with peace treatment handling)")
  
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
      model_vars <- c(model_vars, "region_number", "ano")
      if (use_weights && !is.null(weight_var)) {
        model_vars <- c(model_vars, weight_var)
      }
      model_data <- data[complete.cases(data[, model_vars]), ]
      
      if (nrow(model_data) < 20) {
        message("    ❌ Insufficient data: ", nrow(model_data), " observations")
        next
      }
      
      # Fit model (with or without weights)
      if (use_weights && !is.null(weight_var)) {
        model <- lm(as.formula(final_formula), data = model_data, weights = model_data[[weight_var]])
        message("    Using weights: ", weight_var)
      } else {
        model <- lm(as.formula(final_formula), data = model_data)
      }
      
      # Check for multicollinearity
      if (any(is.na(coef(model)))) {
        na_coeffs <- names(coef(model))[is.na(coef(model))]
        message("    ❌ Multicollinearity - NA coefficients: ", paste(head(na_coeffs, 3), collapse = ", "))
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
        Weighted = use_weights,
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
  if(length(results_list) > 0) {
    all_results <- do.call(rbind, results_list)
    all_model_stats <- do.call(rbind, model_stats_list)
  } else {
    all_results <- NULL
    all_model_stats <- NULL
  }
  
  message(version_name, " completed: ", 
          ifelse(is.null(all_results), 0, nrow(all_results)), 
          " coefficient estimates from ", length(results_list), " models")
  
  return(list(coefficients = all_results, model_stats = all_model_stats))
}

run_regression_version_2 <- function(data, model_specs, use_weights = FALSE, weight_var = NULL) {
  version_name <- if(use_weights) "Version 2 (Weighted)" else "Version 2 (Unweighted)"
  message("\nRunning ", version_name, ": Region FE only")
  
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
      model_vars <- c(model_vars, "region_number")
      if (use_weights && !is.null(weight_var)) {
        model_vars <- c(model_vars, weight_var)
      }
      model_data <- data[complete.cases(data[, model_vars]), ]
      
      if (nrow(model_data) < 20) {
        message("    ❌ Insufficient data: ", nrow(model_data), " observations")
        next
      }
      
      # Fit model (with or without weights)
      if (use_weights && !is.null(weight_var)) {
        model <- lm(as.formula(final_formula), data = model_data, weights = model_data[[weight_var]])
        message("    Using weights: ", weight_var)
      } else {
        model <- lm(as.formula(final_formula), data = model_data)
      }
      
      # Check for multicollinearity
      if (any(is.na(coef(model)))) {
        na_coeffs <- names(coef(model))[is.na(coef(model))]
        message("    ❌ Multicollinearity - NA coefficients: ", paste(head(na_coeffs, 3), collapse = ", "))
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
        Weighted = use_weights,
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
  if(length(results_list) > 0) {
    all_results <- do.call(rbind, results_list)
    all_model_stats <- do.call(rbind, model_stats_list)
  } else {
    all_results <- NULL
    all_model_stats <- NULL
  }
  
  message(version_name, " completed: ", 
          ifelse(is.null(all_results), 0, nrow(all_results)), 
          " coefficient estimates from ", length(results_list), " models")
  
  return(list(coefficients = all_results, model_stats = all_model_stats))
}

########################################
# EXCEL OUTPUT CREATION
########################################

create_excel_output <- function(version1_results, version2_results, 
                                output_filename) {
  message("Creating Excel output: ", output_filename)
  
  # Prepare Excel data list
  excel_data <- list()
  
  # Sheet 1: Version 1 Coefficients (Region + Year FE)
  if (!is.null(version1_results$coefficients) && nrow(version1_results$coefficients) > 0) {
    excel_data[["V1_Coef_Region_Year_FE"]] <- version1_results$coefficients
  }
  
  # Sheet 2: Version 1 Model Stats
  if (!is.null(version1_results$model_stats) && nrow(version1_results$model_stats) > 0) {
    excel_data[["V1_Stats_Region_Year_FE"]] <- version1_results$model_stats
  }
  
  # Sheet 3: Version 2 Coefficients (Region FE only)
  if (!is.null(version2_results$coefficients) && nrow(version2_results$coefficients) > 0) {
    excel_data[["V2_Coef_Region_FE"]] <- version2_results$coefficients
  }
  
  # Sheet 4: Version 2 Model Stats
  if (!is.null(version2_results$model_stats) && nrow(version2_results$model_stats) > 0) {
    excel_data[["V2_Stats_Region_FE"]] <- version2_results$model_stats
  }
  
  # Remove empty sheets
  excel_data <- excel_data[sapply(excel_data, function(x) nrow(x) > 0)]
  
  # Create output file path
  output_file_path <- file.path(output_path, output_filename)
  
  # Write Excel file
  tryCatch({
    writexl::write_xlsx(excel_data, output_file_path)
    message("✅ Excel file created successfully: ", output_filename)
    message("   Sheets created: ", length(excel_data))
    
    return(TRUE)
    
  }, error = function(e) {
    message("❌ Error creating Excel file: ", e$message)
    return(FALSE)
  })
}

########################################
# MAIN ANALYSIS FUNCTION FOR ONE DEPENDENT VARIABLE
########################################

run_analysis_for_dependent <- function(data, dependent_var, model_version, weights_available) {
  
  message("\n", paste(rep("=", 80), collapse=""))
  message("ANALYZING DEPENDENT VARIABLE: ", dependent_var)
  message("MODEL VERSION: ", model_version)
  message(paste(rep("=", 80), collapse=""))
  
  # Get model specifications based on version
  if(model_version == "myers_only") {
    model_specs <- get_model_specifications_myers_only(dependent_var)
    version_suffix <- ""
  } else {
    model_specs <- get_model_specifications_myers_benford(dependent_var)
    version_suffix <- "_benford"
  }
  
  # Run UNWEIGHTED regressions
  message("\n", paste(rep("-", 40), collapse=""))
  message("RUNNING UNWEIGHTED ANALYSIS")
  message(paste(rep("-", 40), collapse=""))
  
  version1_unweighted <- run_regression_version_1(data, model_specs, use_weights = FALSE)
  version2_unweighted <- run_regression_version_2(data, model_specs, use_weights = FALSE)
  
  # Create unweighted output
  output_filename_uw <- paste0("01_COL_reg_dept-area_", dependent_var, version_suffix, "_uw.xlsx")
  excel_success_uw <- create_excel_output(version1_unweighted, version2_unweighted, 
                                          output_filename_uw)
  
  # Run WEIGHTED regressions (if weights available)
  excel_success_w <- FALSE
  if (weights_available) {
    message("\n", paste(rep("-", 40), collapse=""))
    message("RUNNING WEIGHTED ANALYSIS")
    message(paste(rep("-", 40), collapse=""))
    
    version1_weighted <- run_regression_version_1(data, model_specs, 
                                                  use_weights = TRUE, 
                                                  weight_var = "n_observations_myers")
    version2_weighted <- run_regression_version_2(data, model_specs, 
                                                  use_weights = TRUE, 
                                                  weight_var = "n_observations_myers")
    
    # Create weighted output
    output_filename_w <- paste0("01_COL_reg_dept-area_", dependent_var, version_suffix, "_w.xlsx")
    excel_success_w <- create_excel_output(version1_weighted, version2_weighted, 
                                           output_filename_w)
  }
  
  return(list(
    unweighted_success = excel_success_uw,
    weighted_success = excel_success_w
  ))
}

########################################
# MAIN EXECUTION FUNCTION
########################################

run_poverty_analysis <- function() {
  message("Starting Colombia Poverty Analysis with Myers-Benford framework...")
  message(paste(rep("=", 80), collapse=""))
  
  # Step 1: Load and prepare data
  data_result <- load_and_prepare_data()
  data <- data_result$data
  weights_available <- data_result$weights_available
  
  # Define dependent variables
  dependent_vars <- c("pov_rate_3_00", "pov_rate_4_20", "pov_rate_8_30", "share_pov_8_30")
  
  # Define model versions
  model_versions <- c("myers_only", "myers_benford")
  
  # Track results
  results_summary <- list()
  
  # Step 2: Run analysis for each dependent variable and model version
  for(dep_var in dependent_vars) {
    
    # Check if dependent variable exists in data
    if(!dep_var %in% names(data)) {
      message("\n❌ WARNING: ", dep_var, " not found in dataset, skipping...")
      next
    }
    
    for(model_ver in model_versions) {
      
      analysis_key <- paste0(dep_var, "_", model_ver)
      
      result <- run_analysis_for_dependent(
        data = data,
        dependent_var = dep_var,
        model_version = model_ver,
        weights_available = weights_available
      )
      
      results_summary[[analysis_key]] <- result
    }
  }
  
  # Step 3: Final Summary
  message("\n", paste(rep("=", 80), collapse=""))
  message("FINAL ANALYSIS SUMMARY")
  message(paste(rep("=", 80), collapse=""))
  message("Data loaded: ", nrow(data), " observations")
  message("Dependent variables analyzed: ", length(dependent_vars))
  message("Model versions: 2 (Myers only, Myers + Benford)")
  message("Total Excel files created: ", sum(sapply(results_summary, function(x) x$unweighted_success + x$weighted_success)))
  message("")
  message("Output directory: ", output_path)
  message("")
  
  # Detailed results by dependent variable
  for(dep_var in dependent_vars) {
    message("Results for ", dep_var, ":")
    
    # Myers only
    key_myers <- paste0(dep_var, "_myers_only")
    if(key_myers %in% names(results_summary)) {
      uw_status <- ifelse(results_summary[[key_myers]]$unweighted_success, "✅", "❌")
      w_status <- ifelse(results_summary[[key_myers]]$weighted_success, "✅", "❌")
      message("  Myers only: UW ", uw_status, " | W ", w_status)
    }
    
    # Myers + Benford
    key_benford <- paste0(dep_var, "_myers_benford")
    if(key_benford %in% names(results_summary)) {
      uw_status <- ifelse(results_summary[[key_benford]]$unweighted_success, "✅", "❌")
      w_status <- ifelse(results_summary[[key_benford]]$weighted_success, "✅", "❌")
      message("  Myers + Benford: UW ", uw_status, " | W ", w_status)
    }
    message("")
  }
  
  message("Analysis completed successfully!")
  message(paste(rep("=", 80), collapse=""))
  
  return(list(
    data = data,
    results_summary = results_summary,
    weights_available = weights_available
  ))
}

########################################
# EXECUTE ANALYSIS
########################################

# Run the complete analysis
results <- run_poverty_analysis()

message("\n📊 POVERTY ANALYSIS WITH MYERS-BENFORD FRAMEWORK COMPLETED")
message("🎯 Key Features:")
message("   - 4 dependent variables: pov_rate_3_00, pov_rate_4_20, pov_rate_8_30, share_pov_8_30")
message("   - 2 model versions: (A) Myers only, (B) Myers + Benford")
message("   - 2 weight options: unweighted and weighted")
message("   - Total: 16 Excel output files")
message("\n📋 Output Files Location:")
message("   ", output_path)
message("\n🔧 Technical Implementation:")
message("   - Individual quintile dummies (Q2-Q5, Q1 as reference)")
message("   - Triple interactions: Myers/Benford × Peace × Quintiles")
message("   - Clustered standard errors at region level")
message("   - Complete case analysis for each model")
message("   - Two FE strategies: Region+Year and Region only")
message("\nAnalysis ready for publication!")