########################################
# Colombia Gini Inequality Analysis - WITH MYERS×BENFORD INTERACTION
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-10-27
# Purpose: 
# Analyze Gini coefficient outcomes with Myers-Benford interaction framework
# Two model versions: (A) Basic Interaction, (B) Expanded Interaction
# One dependent variable: gini (region-area level Gini coefficient)
# Weighted and unweighted versions

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

# Input file path (merged dataset with Gini and all control variables)
input_file_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge/08_V01_my_Bend_CSI_NTL_pov_depto_area-V2.dta"

# Output path - INTERACTION FOLDER
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col/Gini Regressions/Interaction"

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
                     "gini")
  
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    warning("Missing some required variables: ", paste(missing_vars, collapse = ", "))
    message("  Will proceed with available variables")
  }
  
  # Check Gini coefficient data
  message("\nChecking Gini coefficient variable:")
  if("gini" %in% names(data)) {
    n_available <- sum(!is.na(data$gini))
    pct_available <- round(n_available / nrow(data) * 100, 1)
    range_vals <- range(data$gini, na.rm = TRUE)
    message("  - gini: ", n_available, " obs (", pct_available, "%), range: [", 
            round(range_vals[1], 4), ", ", round(range_vals[2], 4), "]")
    
    # Gini should already be in 0-1 scale, no rescaling needed
    message("  - Gini coefficient is already in 0-1 scale (no rescaling needed)")
  } else {
    stop("ERROR: Gini coefficient variable 'gini' not found in dataset!")
  }
  
  # Check for weights variable
  if ("n_observations_myers" %in% names(data)) {
    message("\n  Weights variable found: n_observations_myers")
    weights_available <- TRUE
  } else {
    message("\n  WARNING: Weights variable 'n_observations_myers' not found - weighted analysis will be skipped")
    weights_available <- FALSE
  }
  
  # Create quintile dummy variables
  data <- create_quintile_dummies(data)
  
  # Data diagnostics
  message("\nData diagnostics:")
  message("  - Regions: ", length(unique(data$region_number)))
  message("  - Years: ", paste(range(data$ano, na.rm = TRUE), collapse = "-"))
  message("  - Urban distribution: ", paste(table(data$urban_area), collapse = " rural, ", " urban"))
  
  return(list(data = data, weights_available = weights_available))
}

########################################
# MODEL SPECIFICATIONS - VERSION A (BASIC INTERACTION)
########################################

get_model_specifications_interaction_basic <- function() {
  message("Defining model specifications for Gini coefficient (Myers + Benford + Interaction - Basic)...")
  
  # Base variables
  dependent_var <- "gini"
  myers_var <- "standardized_myers"
  benford_var <- "benford_abs_distance"
  urban_control <- "urban_area"
  
  # Model specifications with Myers×Benford interaction
  models <- list(
    "Model 1: Myers + Benford + Interaction" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control),
      has_peace = FALSE,
      description = "Myers + Benford + Myers×Benford interaction with urban control"
    ),
    
    "Model 2: Myers + Benford + Interaction + NTL" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5"),
      has_peace = FALSE,
      description = "Myers + Benford + Myers×Benford + NTL + Myers×NTL interactions"
    ),
    
    "Model 3: Myers + Benford + Interaction + CSI" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5"),
      has_peace = FALSE,
      description = "Myers + Benford + Myers×Benford + CSI + Myers×CSI interactions"
    ),
    
    "Model 4: Myers + Benford + Interaction + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
                      "+ after_peace_treaty +", 
                      myers_var, ": after_peace_treaty +",
                      benford_var, ": after_peace_treaty"),
      has_peace = TRUE,
      description = "Myers + Benford + Myers×Benford + Peace + interactions with Peace"
    ),
    
    "Model 5: Myers + Benford + Interaction + NTL + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
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
      description = "Full model with Myers×Benford + NTL + Peace + Triple interactions"
    ),
    
    "Model 6: Myers + Benford + Interaction + CSI + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
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
      description = "Full model with Myers×Benford + CSI + Peace + Triple interactions"
    )
  )
  
  return(models)
}

########################################
# MODEL SPECIFICATIONS - VERSION B (EXPANDED INTERACTION)
########################################

get_model_specifications_interaction_expanded <- function() {
  message("Defining model specifications for Gini coefficient (Myers + Benford + Interaction - Expanded)...")
  
  # Base variables
  dependent_var <- "gini"
  myers_var <- "standardized_myers"
  benford_var <- "benford_abs_distance"
  urban_control <- "urban_area"
  
  # Expanded model specifications with additional Benford interactions
  models <- list(
    "Model 1: Myers + Benford + Interaction" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control),
      has_peace = FALSE,
      description = "Myers + Benford + Myers×Benford interaction"
    ),
    
    "Model 2: Myers + Benford + Interaction + NTL (Both)" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5 +",
                      benford_var, ": ntl_q2 +", benford_var, ": ntl_q3 +",
                      benford_var, ": ntl_q4 +", benford_var, ": ntl_q5"),
      has_peace = FALSE,
      description = "Myers×Benford + NTL + Both Myers×NTL and Benford×NTL interactions"
    ),
    
    "Model 3: Myers + Benford + Interaction + CSI (Both)" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5 +",
                      benford_var, ": csi_q2 +", benford_var, ": csi_q3 +",
                      benford_var, ": csi_q4 +", benford_var, ": csi_q5"),
      has_peace = FALSE,
      description = "Myers×Benford + CSI + Both Myers×CSI and Benford×CSI interactions"
    ),
    
    "Model 4: Myers + Benford + Interaction + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
                      "+ after_peace_treaty +", 
                      myers_var, ": after_peace_treaty +",
                      benford_var, ": after_peace_treaty +",
                      myers_var, ":", benford_var, ": after_peace_treaty"),
      has_peace = TRUE,
      description = "Myers×Benford + Peace + Myers×Benford×Peace triple interaction"
    ),
    
    "Model 5: Full with NTL (Both interact)" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5 +",
                      benford_var, ": ntl_q2 +", benford_var, ": ntl_q3 +",
                      benford_var, ": ntl_q4 +", benford_var, ": ntl_q5 +",
                      "+ after_peace_treaty +", 
                      myers_var, ": after_peace_treaty +",
                      benford_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : ntl_q2 +", 
                      myers_var, ": after_peace_treaty : ntl_q3 +",
                      myers_var, ": after_peace_treaty : ntl_q4 +", 
                      myers_var, ": after_peace_treaty : ntl_q5 +",
                      benford_var, ": after_peace_treaty : ntl_q2 +", 
                      benford_var, ": after_peace_treaty : ntl_q3 +",
                      benford_var, ": after_peace_treaty : ntl_q4 +", 
                      benford_var, ": after_peace_treaty : ntl_q5"),
      has_peace = TRUE,
      description = "Full model: Myers×Benford + NTL + Peace with all interactions"
    ),
    
    "Model 6: Full with CSI (Both interact)" = list(
      formula = paste(dependent_var, "~", myers_var, "+", benford_var, "+", 
                      myers_var, ":", benford_var, "+", urban_control, 
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5 +",
                      benford_var, ": csi_q2 +", benford_var, ": csi_q3 +",
                      benford_var, ": csi_q4 +", benford_var, ": csi_q5 +",
                      "+ after_peace_treaty +", 
                      myers_var, ": after_peace_treaty +",
                      benford_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : csi_q2 +", 
                      myers_var, ": after_peace_treaty : csi_q3 +",
                      myers_var, ": after_peace_treaty : csi_q4 +", 
                      myers_var, ": after_peace_treaty : csi_q5 +",
                      benford_var, ": after_peace_treaty : csi_q2 +", 
                      benford_var, ": after_peace_treaty : csi_q3 +",
                      benford_var, ": after_peace_treaty : csi_q4 +", 
                      benford_var, ": after_peace_treaty : csi_q5"),
      has_peace = TRUE,
      description = "Full model: Myers×Benford + CSI + Peace with all interactions"
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
# MAIN ANALYSIS FUNCTION
########################################

run_gini_analysis <- function(data, model_version, weights_available) {
  
  message("\n", paste(rep("=", 80), collapse=""))
  message("ANALYZING GINI COEFFICIENT")
  message("MODEL VERSION: ", model_version)
  message(paste(rep("=", 80), collapse=""))
  
  # Get model specifications based on version
  if(model_version == "interaction_basic") {
    model_specs <- get_model_specifications_interaction_basic()
    version_suffix <- "_interaction_basic"
  } else {
    model_specs <- get_model_specifications_interaction_expanded()
    version_suffix <- "_interaction_expanded"
  }
  
  # Run UNWEIGHTED regressions
  message("\n", paste(rep("-", 40), collapse=""))
  message("RUNNING UNWEIGHTED ANALYSIS")
  message(paste(rep("-", 40), collapse=""))
  
  version1_unweighted <- run_regression_version_1(data, model_specs, use_weights = FALSE)
  version2_unweighted <- run_regression_version_2(data, model_specs, use_weights = FALSE)
  
  # Create unweighted output
  output_filename_uw <- paste0("01_COL_reg_dept-area_gini", version_suffix, "_uw.xlsx")
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
    output_filename_w <- paste0("01_COL_reg_dept-area_gini", version_suffix, "_w.xlsx")
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

run_complete_gini_analysis <- function() {
  message("Starting Colombia Gini Inequality Analysis with Myers×Benford INTERACTION framework...")
  message(paste(rep("=", 80), collapse=""))
  
  # Step 1: Load and prepare data
  data_result <- load_and_prepare_data()
  data <- data_result$data
  weights_available <- data_result$weights_available
  
  # Define model versions
  model_versions <- c("interaction_basic", "interaction_expanded")
  
  # Track results
  results_summary <- list()
  
  # Step 2: Run analysis for each model version
  for(model_ver in model_versions) {
    
    result <- run_gini_analysis(
      data = data,
      model_version = model_ver,
      weights_available = weights_available
    )
    
    results_summary[[model_ver]] <- result
  }
  
  # Step 3: Final Summary
  message("\n", paste(rep("=", 80), collapse=""))
  message("FINAL ANALYSIS SUMMARY - MYERS×BENFORD INTERACTION VERSION")
  message(paste(rep("=", 80), collapse=""))
  message("Data loaded: ", nrow(data), " observations")
  message("Dependent variable: Gini coefficient (region-area level inequality)")
  message("Model versions: 2 (Basic Interaction, Expanded Interaction)")
  message("Total Excel files created: ", sum(sapply(results_summary, function(x) x$unweighted_success + x$weighted_success)))
  message("")
  message("Output directory: ", output_path)
  message("")
  
  # Detailed results
  message("Results summary:")
  
  # Basic interaction
  if("interaction_basic" %in% names(results_summary)) {
    uw_status <- ifelse(results_summary[["interaction_basic"]]$unweighted_success, "✅", "❌")
    w_status <- ifelse(results_summary[["interaction_basic"]]$weighted_success, "✅", "❌")
    message("  Basic Interaction: UW ", uw_status, " | W ", w_status)
  }
  
  # Expanded interaction
  if("interaction_expanded" %in% names(results_summary)) {
    uw_status <- ifelse(results_summary[["interaction_expanded"]]$unweighted_success, "✅", "❌")
    w_status <- ifelse(results_summary[["interaction_expanded"]]$weighted_success, "✅", "❌")
    message("  Expanded Interaction: UW ", uw_status, " | W ", w_status)
  }
  message("")
  
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

# Run the complete Gini analysis
results <- run_complete_gini_analysis()

message("\n📊 GINI INEQUALITY ANALYSIS WITH MYERS×BENFORD INTERACTION COMPLETED")
message("🎯 Key Features:")
message("   - 1 dependent variable: Gini coefficient (region-area level)")
message("   - 2 model versions:")
message("     A) Basic: Myers×Benford interaction + standard controls")
message("     B) Expanded: Myers×Benford + both Myers×Quintile AND Benford×Quintile")
message("   - 2 weight options: unweighted and weighted")
message("   - Total: 4 Excel output files")
message("\n📋 Output Files:")
message("   1. 01_COL_reg_dept-area_gini_interaction_basic_uw.xlsx")
message("   2. 01_COL_reg_dept-area_gini_interaction_basic_w.xlsx")
message("   3. 01_COL_reg_dept-area_gini_interaction_expanded_uw.xlsx")
message("   4. 01_COL_reg_dept-area_gini_interaction_expanded_w.xlsx")
message("\n📁 Output Location:")
message("   ", output_path)
message("\n🔧 Technical Implementation:")
message("   - MAIN FEATURE: Myers×Benford interaction in ALL models")
message("   - Individual quintile dummies (Q2-Q5, Q1 as reference)")
message("   - Triple interactions available in complex models")
message("   - Clustered standard errors at region level")
message("   - Complete case analysis for each model")
message("   - Two FE strategies: Region+Year and Region only")
message("\nGini inequality analysis with interactions ready for publication!")