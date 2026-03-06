########################################
# Colombia Myers-Benford Analysis - GRD INTEGRATION V02
########################################
# Purpose: 
# Focused analysis using V01 dept-area level data merged with GRD
# Replaces CSI with GRD continuous/binary variables
# Handles 2014 GRD cutoff timeframe constraints

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

packages <- c("haven", "dplyr", "broom", "sandwich", "lmtest", "writexl")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input file path (Merged GRD dataset from Script 2)
input_file_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge/08_V01_my_Bend_CSI_NTL_pov_GRD_depto_area.dta"

# Output path
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"
output_filename_unweighted <- "09_Col_regressions_V01_GRD_depto-area-unweighted.xlsx"
output_filename_weighted <- "09_Col_regressions_V01_GRD_depto-area-weighted.xlsx"

# Create output directory if it doesn't exist
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

########################################
# UTILITY FUNCTIONS
########################################

add_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value <= 0.01) return("***")
  if (p_value <= 0.05) return("**")
  if (p_value <= 0.10) return("*")
  return("")
}

create_pre_2016_year_fe <- function(data) {
  pre_2016_years <- unique(data$ano[data$ano < 2016])
  pre_2016_years <- pre_2016_years[!is.na(pre_2016_years)]
  
  if (length(pre_2016_years) > 1) {
    reference_year <- min(pre_2016_years)
    fe_years <- pre_2016_years[pre_2016_years != reference_year]
    return(paste0("I(ano == ", fe_years, ")", collapse = " + "))
  } else {
    return("")
  }
}

create_quintile_dummies <- function(data) {
  message("Creating NTL quintiles and dummy variables...")
  
  if ("ntl_average_masked" %in% names(data)) {
    data$ntl_quintile <- ntile(data$ntl_average_masked, 5)
    data$ntl_q2 <- ifelse(is.na(data$ntl_quintile), NA, ifelse(data$ntl_quintile == 2, 1, 0))
    data$ntl_q3 <- ifelse(is.na(data$ntl_quintile), NA, ifelse(data$ntl_quintile == 3, 1, 0))
    data$ntl_q4 <- ifelse(is.na(data$ntl_quintile), NA, ifelse(data$ntl_quintile == 4, 1, 0))
    data$ntl_q5 <- ifelse(is.na(data$ntl_quintile), NA, ifelse(data$ntl_quintile == 5, 1, 0))
  }
  # CSI quintiles completely removed as requested
  return(data)
}

########################################
# DATA LOADING AND PREPARATION
########################################

load_and_prepare_data <- function() {
  message("Loading data from: ", basename(input_file_path))
  
  if (!file.exists(input_file_path)) stop("Input file not found: ", input_file_path)
  data <- haven::read_dta(input_file_path)
  
  # Check for required variables including new GRD vars
  required_vars <- c("region_number", "ano", "standardized_myers", "benford_abs_distance", 
                     "urban_area", "after_peace_treaty", 
                     "grd_log_wb_value", "grd_has_resource", "grd_has_lootable")
  
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  
  weights_available <- "n_observations_myers" %in% names(data)
  data <- create_quintile_dummies(data)
  
  return(list(data = data, weights_available = weights_available))
}

########################################
# MODEL SPECIFICATIONS 
########################################

get_model_specifications <- function() {
  message("Defining model specifications with GRD variables...")
  
  dependent_var <- "benford_abs_distance"
  myers_var <- "standardized_myers"
  urban_control <- "urban_area"
  
  models <- list(
    "Model 1: Basic Myers" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control),
      has_peace = FALSE
    ),
    
    "Model 2: Myers + NTL Quintiles" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5"),
      has_peace = FALSE
    ),
    
    "Model 3: Myers + GRD Log Value" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ grd_log_wb_value +", myers_var, ": grd_log_wb_value"),
      has_peace = FALSE
    ),
    
    "Model 4: Myers + GRD Resource Bin" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ grd_has_resource +", myers_var, ": grd_has_resource"),
      has_peace = FALSE
    ),
    
    "Model 5: Myers + GRD Lootable Bin" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ grd_has_lootable +", myers_var, ": grd_has_lootable"),
      has_peace = FALSE
    ),
    
    "Model 6: Myers + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
      has_peace = TRUE
    ),
    
    "Model 7: Myers + NTL + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control, 
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5 +",
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : ntl_q2 +", 
                      myers_var, ": after_peace_treaty : ntl_q3 +",
                      myers_var, ": after_peace_treaty : ntl_q4 +", 
                      myers_var, ": after_peace_treaty : ntl_q5"),
      has_peace = TRUE
    )
    # GRD + Peace triple interactions omitted because GRD NAs post-2014 destroy peace variation
  )
  
  return(models)
}

########################################
# REGRESSION RUNNERS (V1 & V2)
########################################

run_regression <- function(data, model_specs, version_name, use_weights = FALSE, weight_var = NULL, use_year_fe = TRUE) {
  message("\nRunning ", version_name)
  
  results_list <- list()
  model_stats_list <- list()
  
  for (i in 1:length(model_specs)) {
    model_name <- names(model_specs)[i]
    model_spec <- model_specs[[i]]
    
    tryCatch({
      base_formula <- model_spec$formula
      
      if (use_year_fe) {
        if (model_spec$has_peace) {
          pre_2016_year_fe <- create_pre_2016_year_fe(data)
          final_formula <- if(pre_2016_year_fe != "") paste(base_formula, "+ factor(region_number) +", pre_2016_year_fe) else paste(base_formula, "+ factor(region_number)")
          fe_strategy <- "region_pre2016year_peace"
        } else {
          final_formula <- paste(base_formula, "+ factor(region_number) + factor(ano)")
          fe_strategy <- "region_year"
        }
      } else {
        final_formula <- paste(base_formula, "+ factor(region_number)")
        fe_strategy <- "region_only"
      }
      
      model_vars <- all.vars(as.formula(base_formula))
      if (use_weights && !is.null(weight_var)) model_vars <- c(model_vars, weight_var)
      
      # Complete cases will automatically subset GRD models to 2008-2014
      model_data <- data[complete.cases(data[, model_vars]), ]
      
      if (nrow(model_data) < 20) {
        message("  ❌ Skipped ", model_name, ": Insufficient data")
        next
      }
      
      model <- if(use_weights) lm(as.formula(final_formula), data = model_data, weights = model_data[[weight_var]]) else lm(as.formula(final_formula), data = model_data)
      
      if (any(is.na(coef(model)))) {
        message("  ❌ Multicollinearity in ", model_name)
        next
      }
      
      clustered_vcov <- vcovCL(model, cluster = model_data$region_number)
      clustered_se <- sqrt(diag(clustered_vcov))
      coefficients <- coef(model)
      t_stats <- coefficients / clustered_se
      p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)
      
      results_list[[model_name]] <- data.frame(
        term = names(coefficients), estimate = coefficients, std.error = clustered_se,
        statistic = t_stats, p.value = p_values, significance = sapply(p_values, add_significance_stars),
        fe_strategy = fe_strategy, Model = model_name, stringsAsFactors = FALSE
      )
      
      model_stats_list[[model_name]] <- data.frame(
        r.squared = summary(model)$r.squared, adj.r.squared = summary(model)$adj.r.squared,
        nobs = nobs(model), fe_strategy = fe_strategy, Model = model_name,
        Fixed_Effects = if(use_year_fe) "Region+Year" else "Region only",
        Weighted = use_weights, stringsAsFactors = FALSE
      )
      message("  ✅ Success: ", model_name, " (N = ", nobs(model), ")")
    }, error = function(e) { message("  ❌ Error in ", model_name, ": ", e$message) })
  }
  
  return(list(coefficients = do.call(rbind, results_list), model_stats = do.call(rbind, model_stats_list)))
}

########################################
# EXPORT & MAIN EXECUTION
########################################

create_excel_output <- function(v1, v2, filename, is_weighted) {
  excel_data <- list()
  w_suf <- if(is_weighted) "_Weighted" else "_Unweighted"
  
  if (!is.null(v1$coefficients)) excel_data[[paste0("V1_Coef_Reg_Year", w_suf)]] <- v1$coefficients
  if (!is.null(v1$model_stats)) excel_data[[paste0("V1_Stats_Reg_Year", w_suf)]] <- v1$model_stats
  if (!is.null(v2$coefficients)) excel_data[[paste0("V2_Coef_Reg", w_suf)]] <- v2$coefficients
  if (!is.null(v2$model_stats)) excel_data[[paste0("V2_Stats_Reg", w_suf)]] <- v2$model_stats
  
  if(length(excel_data) > 0) writexl::write_xlsx(excel_data, file.path(output_path, filename))
  return(length(excel_data) > 0)
}

run_focused_analysis <- function() {
  data_result <- load_and_prepare_data()
  data <- data_result$data
  model_specs <- get_model_specifications()
  
  v1_unw <- run_regression(data, model_specs, "V1 (Unweighted, Region+Year FE)", FALSE, NULL, TRUE)
  v2_unw <- run_regression(data, model_specs, "V2 (Unweighted, Region FE)", FALSE, NULL, FALSE)
  create_excel_output(v1_unw, v2_unw, output_filename_unweighted, FALSE)
  
  if (data_result$weights_available) {
    v1_w <- run_regression(data, model_specs, "V1 (Weighted, Region+Year FE)", TRUE, "n_observations_myers", TRUE)
    v2_w <- run_regression(data, model_specs, "V2 (Weighted, Region FE)", TRUE, "n_observations_myers", FALSE)
    create_excel_output(v1_w, v2_w, output_filename_weighted, TRUE)
  }
}

run_focused_analysis()