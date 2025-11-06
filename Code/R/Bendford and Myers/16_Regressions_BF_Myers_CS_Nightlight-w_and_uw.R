########################################
# Colombia Myers-Benford Analysis - FOCUSED V01 DEPT-AREA WITH QUINTILE DUMMIES
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-09-09
# Purpose: 
# Focused analysis using only V01 dept-area level data
# Two fixed effects strategies with 6 specific models
# Individual quintile dummies and triple interactions
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

# Input file path (specific dataset)
input_file_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge/05_V01_Myers_Bend_CSI_NTL_depto_area.dta"

# Output path
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"
output_filename_unweighted <- "07_Col_regressions_V01_depto-area-unweighted.xlsx"
output_filename_weighted <- "07_Col_regressions_V01_depto-area-weighted.xlsx"

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
    
    ntl_dummy_dist <- sapply(c("ntl_q2", "ntl_q3", "ntl_q4", "ntl_q5"), 
                             function(x) sum(data[[x]], na.rm = TRUE))
    message("    NTL dummy distribution: Q2=", ntl_dummy_dist[1], ", Q3=", ntl_dummy_dist[2], 
            ", Q4=", ntl_dummy_dist[3], ", Q5=", ntl_dummy_dist[4])
    
    # Show quintile ranges for interpretation
    ntl_ranges <- data %>%
      filter(!is.na(ntl_quintile)) %>%
      group_by(ntl_quintile) %>%
      summarise(min_val = min(ntl_average_masked, na.rm = TRUE),
                max_val = max(ntl_average_masked, na.rm = TRUE),
                .groups = 'drop')
    
    message("    NTL quintile ranges:")
    for(i in 1:nrow(ntl_ranges)) {
      message("      Q", ntl_ranges$ntl_quintile[i], ": ", 
              round(ntl_ranges$min_val[i], 3), " - ", round(ntl_ranges$max_val[i], 3))
    }
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
    
    csi_dummy_dist <- sapply(c("csi_q2", "csi_q3", "csi_q4", "csi_q5"), 
                             function(x) sum(data[[x]], na.rm = TRUE))
    message("    CSI dummy distribution: Q2=", csi_dummy_dist[1], ", Q3=", csi_dummy_dist[2], 
            ", Q4=", csi_dummy_dist[3], ", Q5=", csi_dummy_dist[4])
    
    # Show quintile ranges for interpretation
    csi_ranges <- data %>%
      filter(!is.na(csi_quintile)) %>%
      group_by(csi_quintile) %>%
      summarise(min_val = min(csi_pre1500_avg_no0, na.rm = TRUE),
                max_val = max(csi_pre1500_avg_no0, na.rm = TRUE),
                .groups = 'drop')
    
    message("    CSI quintile ranges:")
    for(i in 1:nrow(csi_ranges)) {
      message("      Q", csi_ranges$csi_quintile[i], ": ", 
              round(csi_ranges$min_val[i], 3), " - ", round(csi_ranges$max_val[i], 3))
    }
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
                     "urban_area", "ntl_quintile_factor", "csi_quintile_factor", 
                     "after_peace_treaty", "ntl_average_masked", "csi_pre1500_avg_no0")
  
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
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
  
  # Check quintile dummy distributions
  if ("ntl_q2" %in% names(data)) {
    ntl_dummy_dist <- sapply(c("ntl_q2", "ntl_q3", "ntl_q4", "ntl_q5"), function(x) sum(data[[x]], na.rm = TRUE))
    message("  - NTL quintile dummies: Q2=", ntl_dummy_dist[1], ", Q3=", ntl_dummy_dist[2], 
            ", Q4=", ntl_dummy_dist[3], ", Q5=", ntl_dummy_dist[4])
  }
  
  if ("csi_q2" %in% names(data)) {
    csi_dummy_dist <- sapply(c("csi_q2", "csi_q3", "csi_q4", "csi_q5"), function(x) sum(data[[x]], na.rm = TRUE))
    message("  - CSI quintile dummies: Q2=", csi_dummy_dist[1], ", Q3=", csi_dummy_dist[2], 
            ", Q4=", csi_dummy_dist[3], ", Q5=", csi_dummy_dist[4])
  }
  
  # Peace treatment distribution
  peace_dist <- table(data$after_peace_treaty, useNA = "always")
  message("  - Peace treatment distribution: ", paste(names(peace_dist), peace_dist, sep = "=", collapse = ", "))
  
  # Weights distribution
  if (weights_available) {
    message("  - Weights summary: min=", round(min(data$n_observations_myers, na.rm = TRUE), 2),
            ", mean=", round(mean(data$n_observations_myers, na.rm = TRUE), 2),
            ", max=", round(max(data$n_observations_myers, na.rm = TRUE), 2))
  }
  
  return(list(data = data, weights_available = weights_available))
}

########################################
# MODEL SPECIFICATIONS WITH QUINTILE DUMMIES
########################################

get_model_specifications_with_dummies <- function() {
  message("Defining model specifications with quintile dummies...")
  
  # Base variables
  dependent_var <- "benford_abs_distance"
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
  
  message("Model specifications defined:")
  for(i in 1:length(models)) {
    message("  ", names(models)[i], ": ", models[[i]]$description)
  }
  
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
  all_results <- do.call(rbind, results_list)
  all_model_stats <- do.call(rbind, model_stats_list)
  
  message(version_name, " completed: ", nrow(all_results), " coefficient estimates from ", 
          length(results_list), " models")
  
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
  all_results <- do.call(rbind, results_list)
  all_model_stats <- do.call(rbind, model_stats_list)
  
  message(version_name, " completed: ", nrow(all_results), " coefficient estimates from ", 
          length(results_list), " models")
  
  return(list(coefficients = all_results, model_stats = all_model_stats))
}

########################################
# DESCRIPTIVE STATISTICS FUNCTIONS
########################################

create_quintile_descriptive_stats <- function(data) {
  message("Creating quintile descriptive statistics...")
  
  # CSI Quintile Analysis using original factor variable
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
  
  # NTL Quintile Analysis using original factor variable
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
  
  # Add weights if available
  if ("n_observations_myers" %in% names(data)) {
    key_vars <- c(key_vars, "n_observations_myers")
  }
  
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

create_excel_output <- function(version1_results, version2_results, quintile_stats, 
                                sample_data, output_filename, is_weighted = FALSE) {
  message("Creating Excel output: ", output_filename)
  
  # Prepare Excel data list
  excel_data <- list()
  
  weight_suffix <- if(is_weighted) "_Weighted" else "_Unweighted"
  
  # Sheet 1: Version 1 Coefficients (Region + Year FE)
  if (!is.null(version1_results$coefficients) && nrow(version1_results$coefficients) > 0) {
    sheet_name <- paste0("V1_Coefficients_Region_Year_FE", weight_suffix)
    excel_data[[sheet_name]] <- version1_results$coefficients
  }
  
  # Sheet 2: Version 1 Model Stats
  if (!is.null(version1_results$model_stats) && nrow(version1_results$model_stats) > 0) {
    sheet_name <- paste0("V1_Model_Stats_Region_Year_FE", weight_suffix)
    excel_data[[sheet_name]] <- version1_results$model_stats
  }
  
  # Sheet 3: Version 2 Coefficients (Region FE only)
  if (!is.null(version2_results$coefficients) && nrow(version2_results$coefficients) > 0) {
    sheet_name <- paste0("V2_Coefficients_Region_FE", weight_suffix)
    excel_data[[sheet_name]] <- version2_results$coefficients
  }
  
  # Sheet 4: Version 2 Model Stats
  if (!is.null(version2_results$model_stats) && nrow(version2_results$model_stats) > 0) {
    sheet_name <- paste0("V2_Model_Stats_Region_FE", weight_suffix)
    excel_data[[sheet_name]] <- version2_results$model_stats
  }
  
  # Sheet 5: Quintile Descriptive Statistics (only include once, in unweighted file)
  if (!is_weighted && !is.null(quintile_stats) && nrow(quintile_stats) > 0) {
    excel_data[["Quintile_Descriptive_Stats"]] <- quintile_stats
  }
  
  # Sheet 6: Sample Data (only include once, in unweighted file)
  if (!is_weighted && !is.null(sample_data) && nrow(sample_data) > 0) {
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
  message("Starting focused Colombia Myers-Benford analysis with quintile dummies...")
  message("Input: V01 dept-area level only")
  message("Output: Unweighted and Weighted versions")
  message(paste(rep("=", 60), collapse=""))
  
  # Step 1: Load and prepare data
  data_result <- load_and_prepare_data()
  data <- data_result$data
  weights_available <- data_result$weights_available
  
  # Step 2: Get model specifications with quintile dummies
  model_specs <- get_model_specifications_with_dummies()
  
  # Step 3: Run UNWEIGHTED regressions
  message("\n", paste(rep("-", 40), collapse=""))
  message("RUNNING UNWEIGHTED ANALYSIS")
  message(paste(rep("-", 40), collapse=""))
  
  # Version 1 (Region + Year FE) - Unweighted
  version1_unweighted <- run_regression_version_1(data, model_specs, use_weights = FALSE)
  
  # Version 2 (Region FE only) - Unweighted
  version2_unweighted <- run_regression_version_2(data, model_specs, use_weights = FALSE)
  
  # Step 4: Run WEIGHTED regressions (if weights available)
  version1_weighted <- NULL
  version2_weighted <- NULL
  
  if (weights_available) {
    message("\n", paste(rep("-", 40), collapse=""))
    message("RUNNING WEIGHTED ANALYSIS")
    message(paste(rep("-", 40), collapse=""))
    
    # Version 1 (Region + Year FE) - Weighted
    version1_weighted <- run_regression_version_1(data, model_specs, 
                                                  use_weights = TRUE, weight_var = "n_observations_myers")
    
    # Version 2 (Region FE only) - Weighted
    version2_weighted <- run_regression_version_2(data, model_specs, 
                                                  use_weights = TRUE, weight_var = "n_observations_myers")
  } else {
    message("\n❌ SKIPPING WEIGHTED ANALYSIS - weights variable not found")
  }
  
  # Step 5: Create descriptive statistics (only once)
  quintile_stats <- create_quintile_descriptive_stats(data)
  sample_data <- create_sample_data(data)
  
  # Step 6: Create Excel outputs
  
  # Unweighted results
  excel_success_unweighted <- create_excel_output(version1_unweighted, version2_unweighted, 
                                                  quintile_stats, sample_data, 
                                                  output_filename_unweighted, is_weighted = FALSE)
  
  # Weighted results (if available)
  excel_success_weighted <- FALSE
  if (weights_available) {
    excel_success_weighted <- create_excel_output(version1_weighted, version2_weighted, 
                                                  NULL, NULL,  # Don't duplicate descriptive stats
                                                  output_filename_weighted, is_weighted = TRUE)
  }
  
  # Summary
  message("\n", paste(rep("=", 60), collapse=""))
  message("ANALYSIS SUMMARY:")
  message("✅ Data loaded: ", nrow(data), " observations")
  message("✅ Models specified: ", length(model_specs), " (with quintile dummies and triple interactions)")
  
  if (!is.null(version1_unweighted$coefficients)) {
    message("✅ Version 1 Unweighted (Region+Year FE): ", 
            length(unique(version1_unweighted$coefficients$Model)), " models estimated")
  }
  
  if (!is.null(version2_unweighted$coefficients)) {
    message("✅ Version 2 Unweighted (Region FE): ", 
            length(unique(version2_unweighted$coefficients$Model)), " models estimated")
  }
  
  if (weights_available) {
    if (!is.null(version1_weighted$coefficients)) {
      message("✅ Version 1 Weighted (Region+Year FE): ", 
              length(unique(version1_weighted$coefficients$Model)), " models estimated")
    }
    
    if (!is.null(version2_weighted$coefficients)) {
      message("✅ Version 2 Weighted (Region FE): ", 
              length(unique(version2_weighted$coefficients$Model)), " models estimated")
    }
  }
  
  if (excel_success_unweighted) {
    message("✅ Unweighted Excel output created: ", output_filename_unweighted)
  } else {
    message("❌ Unweighted Excel output failed")
  }
  
  if (excel_success_weighted) {
    message("✅ Weighted Excel output created: ", output_filename_weighted)
  } else if (weights_available) {
    message("❌ Weighted Excel output failed")
  }
  
  message("Analysis completed successfully!")
  
  return(list(
    data = data,
    unweighted = list(version1 = version1_unweighted, version2 = version2_unweighted),
    weighted = list(version1 = version1_weighted, version2 = version2_weighted),
    quintile_stats = quintile_stats,
    sample_data = sample_data,
    weights_available = weights_available
  ))
}

########################################
# EXECUTE ANALYSIS
########################################

# Run the analysis
results <- run_focused_analysis()

message("\n📊 FOCUSED ANALYSIS WITH QUINTILE DUMMIES COMPLETED")
message("🎯 Key Enhancements:")
message("   - Individual quintile dummies (Q2-Q5, Q1 as reference)")
message("   - Triple interactions: Myers × Peace × Quintiles")
message("   - Both unweighted and weighted versions")
message("   - All models include urban_area control")
message("   - Clustered standard errors at region level")
message("   - Peace treatment models handle multicollinearity")
message("\n📋 Excel Outputs:")
message("   📄 ", output_filename_unweighted)
message("   📄 ", output_filename_weighted, if(results$weights_available) "" else " (SKIPPED - no weights)")
message("\n🔧 Technical Implementation:")
message("   - Quintile dummies with interpretable coefficients")
message("   - Peace models: pre-2016 year FE + peace dummy") 
message("   - Complete case analysis for each model")
message("   - Robust clustered standard errors")
message("   - Automatic multicollinearity detection")
message("   - Comprehensive error handling")
message("Analysis ready for publication with enhanced interpretability!")