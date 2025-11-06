########################################
# Colombia Secuestros Analysis - FOCUSED V01 DEPT-AREA WITH QUINTILE DUMMIES
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-09-09
# Purpose:
# Focused analysis using kidnappings (secuestros) as dependent variable
# Merged Myers-Benford dataset with secuestros data at dept-year level
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
packages <- c("haven", "dplyr", "broom", "sandwich", "lmtest", "writexl", "tidyr")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input file paths (MISMO Myers-Benford del script original)
myers_benford_file <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge/05_V01_Myers_Bend_CSI_NTL_depto_area.dta"

# Secuestros agregado depto-año que generaste antes
secuestros_file <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/DANE/03_secuestros.dta"

# Output path y nombres: ahora con prefijo 09_
output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"
output_filename_unweighted <- "09_secuestros_regressions_unweighted.xlsx"
output_filename_weighted   <- "09_secuestros_regressions_weighted.xlsx"
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
  message("Creating pre-2016 year fixed effects...")
  pre_2016_years <- unique(data$ano[data$ano < 2016])
  pre_2016_years <- pre_2016_years[!is.na(pre_2016_years)]
  message("  Pre-2016 years found: ", paste(sort(pre_2016_years), collapse = ", "))
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

create_quintile_dummies <- function(data) {
  message("Creating quintiles and dummy variables from raw continuous variables...")
  # NTL
  if ("ntl_average_masked" %in% names(data)) {
    data$ntl_quintile <- dplyr::ntile(data$ntl_average_masked, 5)
    data$ntl_q2 <- ifelse(is.na(data$ntl_quintile), NA, as.integer(data$ntl_quintile == 2))
    data$ntl_q3 <- ifelse(is.na(data$ntl_quintile), NA, as.integer(data$ntl_quintile == 3))
    data$ntl_q4 <- ifelse(is.na(data$ntl_quintile), NA, as.integer(data$ntl_quintile == 4))
    data$ntl_q5 <- ifelse(is.na(data$ntl_quintile), NA, as.integer(data$ntl_quintile == 5))
  } else {
    message("  WARNING: ntl_average_masked variable not found")
  }
  # CSI
  if ("csi_pre1500_avg_no0" %in% names(data)) {
    data$csi_quintile <- dplyr::ntile(data$csi_pre1500_avg_no0, 5)
    data$csi_q2 <- ifelse(is.na(data$csi_quintile), NA, as.integer(data$csi_quintile == 2))
    data$csi_q3 <- ifelse(is.na(data$csi_quintile), NA, as.integer(data$csi_quintile == 3))
    data$csi_q4 <- ifelse(is.na(data$csi_quintile), NA, as.integer(data$csi_quintile == 4))
    data$csi_q5 <- ifelse(is.na(data$csi_quintile), NA, as.integer(data$csi_quintile == 5))
  } else {
    message("  WARNING: csi_pre1500_avg_no0 variable not found")
  }
  message("  Quintile dummy variables created successfully")
  return(data)
}

########################################
# DATA LOADING AND MERGING (SE CARGA SECUESTROS EN LUGAR DE COCA)
########################################

load_and_merge_data <- function() {
  message("Loading and merging data...")
  message("Myers-Benford file: ", basename(myers_benford_file))
  message("Secuestros file: ", basename(secuestros_file))
  
  if (!file.exists(myers_benford_file)) stop("Myers-Benford file not found: ", myers_benford_file)
  if (!file.exists(secuestros_file))   stop("Secuestros file not found: ", secuestros_file)
  
  myers_data <- haven::read_dta(myers_benford_file)
  message("Myers-Benford data loaded: ", nrow(myers_data), " rows, ", ncol(myers_data), " variables")
  
  sec_data <- haven::read_dta(secuestros_file)
  message("Secuestros data loaded: ", nrow(sec_data), " rows, ", ncol(sec_data), " variables")
  
  # Alinear claves y tipos para merge depto-año
  # Myers: region_number (prob. num) y ano (num)
  # Secuestros: cod_depto (char, 2 dígitos), anio (num), secuestros (num)
  # Creamos código depto de 2 dígitos en Myers y lo usamos para join:
  myers_data <- myers_data %>%
    mutate(
      # region_number puede venir numérico; generamos versión de 2 dígitos char:
      region_code2 = stringr::str_pad(as.character(as.integer(region_number)), width = 2, side = "left", pad = "0"),
      ano = as.integer(ano)
    )
  
  sec_data <- sec_data %>%
    rename(cod_depto = cod_depto, anio = anio, secuestros = secuestros) %>%
    mutate(
      cod_depto = stringr::str_pad(as.character(cod_depto), width = 2, side = "left", pad = "0"),
      anio = as.integer(anio),
      secuestros = as.numeric(secuestros),
      secuestros = ifelse(is.na(secuestros), 0, secuestros),
      log_secuestros = log(secuestros + 1),
      secuestros_present = as.integer(secuestros > 0)
    )
  
  # Diagnósticos de cruce
  common_deptos <- intersect(unique(myers_data$region_code2), unique(sec_data$cod_depto))
  common_years  <- intersect(unique(myers_data$ano), unique(sec_data$anio))
  message("  Common departments: ", length(common_deptos))
  message("  Common years: ", length(common_years), " (", paste(range(common_years), collapse = "-"), ")")
  
  # Merge manteniendo todas las obs de Myers (como en el original)
  merged_data <- myers_data %>%
    left_join(sec_data %>% select(cod_depto, anio, secuestros, log_secuestros, secuestros_present),
              by = c("region_code2" = "cod_depto", "ano" = "anio"))
  
  message("Merged data: ", nrow(merged_data), " rows, ", ncol(merged_data), " variables")
  
  # Resumen secuestros
  sec_summary <- merged_data %>%
    summarise(
      n_obs = n(),
      n_with_sec = sum(!is.na(secuestros)),
      total_secuestros = sum(secuestros, na.rm = TRUE),
      mean_secuestros = mean(secuestros, na.rm = TRUE),
      depts_with_sec = sum(secuestros_present, na.rm = TRUE),
      pct_depts_with_sec = mean(secuestros_present, na.rm = TRUE) * 100
    )
  message("\nSecuestros summary in merged dataset:")
  message("  Total observations: ", sec_summary$n_obs)
  message("  With secuestros data: ", sec_summary$n_with_sec)
  message("  Total secuestros: ", round(sec_summary$total_secuestros, 0))
  message("  Mean secuestros: ", round(sec_summary$mean_secuestros, 2))
  message("  Departments with secuestros>0: ", sec_summary$depts_with_sec,
          " (", round(sec_summary$pct_depts_with_sec, 1), "%)")
  
  return(merged_data)
}

load_and_prepare_data <- function() {
  message("Loading and preparing data...")
  data <- load_and_merge_data()
  
  required_vars <- c("region_number","ano","standardized_myers","urban_area",
                     "ntl_average_masked","csi_pre1500_avg_no0","secuestros")
  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) > 0) stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  
  if ("n_observations_myers" %in% names(data)) {
    message("  Weights variable found: n_observations_myers")
    weights_available <- TRUE
  } else {
    message("  WARNING: Weights variable 'n_observations_myers' not found - weighted analysis will be skipped")
    weights_available <- FALSE
  }
  
  data <- create_quintile_dummies(data)
  
  message("\nData diagnostics:")
  message("  - Regions: ", length(unique(data$region_number)))
  message("  - Years: ", paste(range(data$ano, na.rm = TRUE), collapse = "-"))
  message("  - Urban distribution: ", paste(table(data$urban_area), collapse = " | "))
  message("  - Secuestros: min=", round(min(data$secuestros, na.rm = TRUE), 2),
          ", mean=", round(mean(data$secuestros, na.rm = TRUE), 2),
          ", max=", round(max(data$secuestros, na.rm = TRUE), 1))
  message("  - Zero secuestros observations: ",
          sum(data$secuestros == 0, na.rm = TRUE), " (",
          round(mean(data$secuestros == 0, na.rm = TRUE) * 100, 1), "%)")
  
  if ("ntl_q2" %in% names(data)) {
    ntl_dummy_dist <- sapply(c("ntl_q2","ntl_q3","ntl_q4","ntl_q5"), function(x) sum(data[[x]], na.rm = TRUE))
    message("  - NTL quintile dummies: Q2=", ntl_dummy_dist[1], ", Q3=", ntl_dummy_dist[2],
            ", Q4=", ntl_dummy_dist[3], ", Q5=", ntl_dummy_dist[4])
  }
  if ("csi_q2" %in% names(data)) {
    csi_dummy_dist <- sapply(c("csi_q2","csi_q3","csi_q4","csi_q5"), function(x) sum(data[[x]], na.rm = TRUE))
    message("  - CSI quintile dummies: Q2=", csi_dummy_dist[1], ", Q3=", csi_dummy_dist[2],
            ", Q4=", csi_dummy_dist[3], ", Q5=", csi_dummy_dist[4])
  }
  peace_dist <- table(data$after_peace_treaty, useNA = "always")
  message("  - Peace treatment distribution: ", paste(names(peace_dist), peace_dist, sep = "=", collapse = ", "))
  
  if (weights_available) {
    message("  - Weights summary: min=", round(min(data$n_observations_myers, na.rm = TRUE), 2),
            ", mean=", round(mean(data$n_observations_myers, na.rm = TRUE), 2),
            ", max=", round(max(data$n_observations_myers, na.rm = TRUE), 2))
  }
  return(list(data = data, weights_available = weights_available))
}

########################################
# MODEL SPECIFICATIONS WITH QUINTILE DUMMIES - SECUESTROS
########################################

get_model_specifications_with_dummies <- function() {
  message("Defining model specifications with quintile dummies for secuestros analysis...")
  dependent_var <- "secuestros"
  myers_var <- "standardized_myers"
  urban_control <- "urban_area"
  
  models <- list(
    "Model 1: Basic Myers" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control),
      has_peace = FALSE,
      description = "Basic Myers index with urban control (Dep: Secuestros)"
    ),
    "Model 2: Myers + NTL" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control,
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5"),
      has_peace = FALSE,
      description = "Myers + NTL quintile dummies + interactions (Dep: Secuestros)"
    ),
    "Model 3: Myers + CSI" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control,
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5"),
      has_peace = FALSE,
      description = "Myers + CSI quintile dummies + interactions (Dep: Secuestros)"
    ),
    "Model 4: Myers + Peace" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control,
                      "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
      has_peace = TRUE,
      description = "Myers + Peace treatment + interaction (Dep: Secuestros)"
    ),
    "Model 5: Myers + NTL + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control,
                      "+ ntl_q2 + ntl_q3 + ntl_q4 + ntl_q5 +",
                      myers_var, ": ntl_q2 +", myers_var, ": ntl_q3 +",
                      myers_var, ": ntl_q4 +", myers_var, ": ntl_q5 +",
                      "after_peace_treaty +", myers_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : ntl_q2 +",
                      myers_var, ": after_peace_treaty : ntl_q3 +",
                      myers_var, ": after_peace_treaty : ntl_q4 +",
                      myers_var, ": after_peace_treaty : ntl_q5"),
      has_peace = TRUE,
      description = "Myers + NTL + Peace + Triple interactions (Dep: Secuestros)"
    ),
    "Model 6: Myers + CSI + Peace + Triple" = list(
      formula = paste(dependent_var, "~", myers_var, "+", urban_control,
                      "+ csi_q2 + csi_q3 + csi_q4 + csi_q5 +",
                      myers_var, ": csi_q2 +", myers_var, ": csi_q3 +",
                      myers_var, ": csi_q4 +", myers_var, ": csi_q5 +",
                      "after_peace_treaty +", myers_var, ": after_peace_treaty +",
                      myers_var, ": after_peace_treaty : csi_q2 +",
                      myers_var, ": after_peace_treaty : csi_q3 +",
                      myers_var, ": after_peace_treaty : csi_q4 +",
                      myers_var, ": after_peace_treaty : csi_q5"),
      has_peace = TRUE,
      description = "Myers + CSI + Peace + Triple interactions (Dep: Secuestros)"
    )
  )
  for (i in seq_along(models)) message("  ", names(models)[i], ": ", models[[i]]$description)
  return(models)
}

########################################
# REGRESSION HELPERS (idénticos a los del original)
########################################

run_regression_version_1 <- function(data, model_specs, use_weights = FALSE, weight_var = NULL) {
  version_name <- if(use_weights) "Version 1 (Weighted)" else "Version 1 (Unweighted)"
  message("\nRunning ", version_name, ": Region + Year FE (with peace treatment handling)")
  results_list <- list(); model_stats_list <- list()
  for (i in 1:length(model_specs)) {
    model_name <- names(model_specs)[i]; model_spec <- model_specs[[i]]
    message("  Running ", model_name, "...")
    tryCatch({
      base_formula <- model_spec$formula
      if (model_spec$has_peace) {
        pre_2016_year_fe <- create_pre_2016_year_fe(data)
        if (pre_2016_year_fe != "") {
          final_formula <- paste(base_formula, "+ factor(region_number) +", pre_2016_year_fe)
          fe_strategy <- "region_pre2016year_peace"
        } else {
          final_formula <- paste(base_formula, "+ factor(region_number)")
          fe_strategy <- "region_only"
        }
      } else {
        final_formula <- paste(base_formula, "+ factor(region_number) + factor(ano)")
        fe_strategy <- "region_year"
      }
      model_vars <- all.vars(as.formula(base_formula))
      if (use_weights && !is.null(weight_var)) model_vars <- c(model_vars, weight_var)
      model_data <- data[complete.cases(data[, model_vars]), ]
      if (nrow(model_data) < 20) { message("    ❌ Insufficient data: ", nrow(model_data)); next }
      if (use_weights && !is.null(weight_var)) {
        model <- lm(as.formula(final_formula), data = model_data, weights = model_data[[weight_var]])
        message("    Using weights: ", weight_var)
      } else model <- lm(as.formula(final_formula), data = model_data)
      if (any(is.na(coef(model)))) { 
        na_coeffs <- names(coef(model))[is.na(coef(model))]; 
        message("    ❌ Multicollinearity - NA coefficients: ", paste(head(na_coeffs,3), collapse=", ")); 
        next 
      }
      model_fitted_data <- model$model
      region_cluster <- model_data$region_number[as.numeric(rownames(model_fitted_data))]
      clustered_vcov <- sandwich::vcovCL(model, cluster = region_cluster)
      clustered_se <- sqrt(diag(clustered_vcov))
      coefficients <- coef(model)
      t_stats <- coefficients / clustered_se
      p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)
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
        nobs = stats::nobs(model),
        fe_strategy = fe_strategy,
        Model = model_name,
        Fixed_Effects = "Region+Year",
        Weighted = use_weights,
        stringsAsFactors = FALSE
      )
      results_list[[model_name]] <- results_df
      model_stats_list[[model_name]] <- model_stats
      message("    ✅ Success: R² = ", round(model_stats$r.squared, 3), ", N = ", model_stats$nobs)
    }, error = function(e) { message("    ❌ Error: ", e$message) })
  }
  all_results <- if (length(results_list)) dplyr::bind_rows(results_list) else NULL
  all_model_stats <- if (length(model_stats_list)) dplyr::bind_rows(model_stats_list) else NULL
  message(version_name, " completed.")
  return(list(coefficients = all_results, model_stats = all_model_stats))
}

run_regression_version_2 <- function(data, model_specs, use_weights = FALSE, weight_var = NULL) {
  version_name <- if(use_weights) "Version 2 (Weighted)" else "Version 2 (Unweighted)"
  message("\nRunning ", version_name, ": Region FE only")
  results_list <- list(); model_stats_list <- list()
  for (i in 1:length(model_specs)) {
    model_name <- names(model_specs)[i]; model_spec <- model_specs[[i]]
    message("  Running ", model_name, "...")
    tryCatch({
      base_formula <- model_spec$formula
      final_formula <- paste(base_formula, "+ factor(region_number)")
      model_vars <- all.vars(as.formula(base_formula))
      if (use_weights && !is.null(weight_var)) model_vars <- c(model_vars, weight_var)
      model_data <- data[complete.cases(data[, model_vars]), ]
      if (nrow(model_data) < 20) { message("    ❌ Insufficient data: ", nrow(model_data)); next }
      if (use_weights && !is.null(weight_var)) {
        model <- lm(as.formula(final_formula), data = model_data, weights = model_data[[weight_var]])
        message("    Using weights: ", weight_var)
      } else model <- lm(as.formula(final_formula), data = model_data)
      if (any(is.na(coef(model)))) { 
        na_coeffs <- names(coef(model))[is.na(coef(model))]; 
        message("    ❌ Multicollinearity - NA coefficients: ", paste(head(na_coeffs,3), collapse=", ")); 
        next 
      }
      model_fitted_data <- model$model
      region_cluster <- model_data$region_number[as.numeric(rownames(model_fitted_data))]
      clustered_vcov <- sandwich::vcovCL(model, cluster = region_cluster)
      clustered_se <- sqrt(diag(clustered_vcov))
      coefficients <- coef(model)
      t_stats <- coefficients / clustered_se
      p_values <- 2 * pt(abs(t_stats), df = model$df.residual, lower.tail = FALSE)
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
        nobs = stats::nobs(model),
        fe_strategy = "region_only",
        Model = model_name,
        Fixed_Effects = "Region only",
        Weighted = use_weights,
        stringsAsFactors = FALSE
      )
      results_list[[model_name]] <- results_df
      model_stats_list[[model_name]] <- model_stats
      message("    ✅ Success: R² = ", round(model_stats$r.squared, 3), ", N = ", model_stats$nobs)
    }, error = function(e) { message("    ❌ Error: ", e$message) })
  }
  all_results <- if (length(results_list)) dplyr::bind_rows(results_list) else NULL
  all_model_stats <- if (length(model_stats_list)) dplyr::bind_rows(model_stats_list) else NULL
  message(version_name, " completed.")
  return(list(coefficients = all_results, model_stats = all_model_stats))
}

########################################
# DESCRIPTIVE STATS (ajustadas para secuestros)
########################################

create_quintile_descriptive_stats <- function(data) {
  message("Creating quintile descriptive statistics for secuestros analysis...")
  csi_stats <- NULL; ntl_stats <- NULL
  
  if ("csi_quintile_factor" %in% names(data)) {
    csi_stats <- data %>%
      dplyr::filter(!is.na(csi_quintile_factor)) %>%
      group_by(csi_quintile_factor) %>%
      summarise(
        n_obs = n(),
        mean_myers = mean(standardized_myers, na.rm = TRUE),
        mean_secuestros = mean(secuestros, na.rm = TRUE),
        median_secuestros = median(secuestros, na.rm = TRUE),
        pct_with_secuestros = mean(secuestros_present, na.rm = TRUE) * 100,
        mean_csi_raw = mean(csi_pre1500_avg_no0, na.rm = TRUE),
        mean_ntl = mean(ntl_average_masked, na.rm = TRUE),
        pct_post_peace = mean(after_peace_treaty, na.rm = TRUE) * 100,
        pct_urban = mean(urban_area, na.rm = TRUE) * 100,
        .groups = 'drop'
      ) %>% mutate(quintile_type = "CSI") %>%
      rename(quintile_factor = csi_quintile_factor)
  }
  if ("ntl_quintile_factor" %in% names(data)) {
    ntl_stats <- data %>%
      dplyr::filter(!is.na(ntl_quintile_factor)) %>%
      group_by(ntl_quintile_factor) %>%
      summarise(
        n_obs = n(),
        mean_myers = mean(standardized_myers, na.rm = TRUE),
        mean_secuestros = mean(secuestros, na.rm = TRUE),
        median_secuestros = median(secuestros, na.rm = TRUE),
        pct_with_secuestros = mean(secuestros_present, na.rm = TRUE) * 100,
        mean_csi_raw = mean(csi_pre1500_avg_no0, na.rm = TRUE),
        mean_ntl = mean(ntl_average_masked, na.rm = TRUE),
        pct_post_peace = mean(after_peace_treaty, na.rm = TRUE) * 100,
        pct_urban = mean(urban_area, na.rm = TRUE) * 100,
        .groups = 'drop'
      ) %>% mutate(quintile_type = "NTL") %>%
      rename(quintile_factor = ntl_quintile_factor)
  }
  combined_stats <- dplyr::bind_rows(csi_stats, ntl_stats)
  if (!is.null(combined_stats) && nrow(combined_stats) > 0) {
    message("  Quintile descriptive stats created (", nrow(combined_stats), " rows).")
  }
  return(combined_stats)
}

create_sample_data <- function(data) {
  message("Creating sample data for Excel output (secuestros analysis)...")
  key_vars <- c("region_number","ano","urban_area","standardized_myers",
                "secuestros","log_secuestros","secuestros_present",
                "ntl_average_masked","ntl_quintile_factor",
                "csi_pre1500_avg_no0","csi_quintile_factor","after_peace_treaty")
  if ("n_observations_myers" %in% names(data)) key_vars <- c(key_vars,"n_observations_myers")
  avail <- key_vars[key_vars %in% names(data)]
  complete_data <- data[stats::complete.cases(data[, avail]), ]
  n_sample <- min(100, nrow(complete_data))
  if (n_sample == 0) return(data.frame())
  set.seed(123)
  sample_indices <- sample(seq_len(nrow(complete_data)), n_sample)
  sample_data <- complete_data[sample_indices, avail]
  sample_data <- sample_data[order(sample_data$region_number, sample_data$ano), ]
  message("  Sample data: ", nrow(sample_data), " rows.")
  return(sample_data)
}

########################################
# EXCEL OUTPUT CREATION
########################################

create_excel_output <- function(version1_results, version2_results, quintile_stats, 
                                sample_data, output_filename, is_weighted = FALSE) {
  message("Creating Excel output: ", output_filename)
  excel_data <- list()
  weight_suffix <- if(is_weighted) "_Weighted" else "_Unweighted"
  if (!is.null(version1_results$coefficients) && nrow(version1_results$coefficients) > 0)
    excel_data[[paste0("V1_Coefficients_Region_Year_FE", weight_suffix)]] <- version1_results$coefficients
  if (!is.null(version1_results$model_stats) && nrow(version1_results$model_stats) > 0)
    excel_data[[paste0("V1_Model_Stats_Region_Year_FE", weight_suffix)]] <- version1_results$model_stats
  if (!is.null(version2_results$coefficients) && nrow(version2_results$coefficients) > 0)
    excel_data[[paste0("V2_Coefficients_Region_FE", weight_suffix)]] <- version2_results$coefficients
  if (!is.null(version2_results$model_stats) && nrow(version2_results$model_stats) > 0)
    excel_data[[paste0("V2_Model_Stats_Region_FE", weight_suffix)]] <- version2_results$model_stats
  if (!is_weighted && !is.null(quintile_stats) && nrow(quintile_stats) > 0)
    excel_data[["Quintile_Descriptive_Stats"]] <- quintile_stats
  if (!is_weighted && !is.null(sample_data) && nrow(sample_data) > 0)
    excel_data[["Sample_Data"]] <- sample_data
  excel_data <- excel_data[sapply(excel_data, function(x) nrow(x) > 0)]
  output_file_path <- file.path(output_path, output_filename)
  ok <- FALSE
  tryCatch({ writexl::write_xlsx(excel_data, output_file_path); ok <- TRUE },
           error = function(e) message("❌ Error creating Excel file: ", e$message))
  if (ok) {
    message("✅ Excel created: ", output_file_path)
    message("   Sheets: ", paste(names(excel_data), collapse = ", "))
  }
  return(ok)
}

########################################
# MAIN EXECUTION FUNCTION - SECUESTROS
########################################

run_focused_analysis <- function() {
  message("Starting focused Colombia Secuestros analysis with quintile dummies...")
  message("Input: V01 dept-area Myers-Benford data merged with secuestros data")
  message("Output: Unweighted and Weighted versions")
  message(paste(rep("=", 60), collapse=""))
  
  data_result <- load_and_prepare_data()
  data <- data_result$data
  weights_available <- data_result$weights_available
  
  model_specs <- get_model_specifications_with_dummies()
  
  message("\n", paste(rep("-", 40), collapse=""))
  message("RUNNING UNWEIGHTED ANALYSIS")
  message(paste(rep("-", 40), collapse=""))
  version1_unweighted <- run_regression_version_1(data, model_specs, use_weights = FALSE)
  version2_unweighted <- run_regression_version_2(data, model_specs, use_weights = FALSE)
  
  version1_weighted <- NULL; version2_weighted <- NULL
  if (weights_available) {
    message("\n", paste(rep("-", 40), collapse=""))
    message("RUNNING WEIGHTED ANALYSIS")
    message(paste(rep("-", 40), collapse=""))
    version1_weighted <- run_regression_version_1(data, model_specs, TRUE, "n_observations_myers")
    version2_weighted <- run_regression_version_2(data, model_specs, TRUE, "n_observations_myers")
  } else {
    message("\n❌ SKIPPING WEIGHTED ANALYSIS - weights variable not found")
  }
  
  quintile_stats <- create_quintile_descriptive_stats(data)
  sample_data <- create_sample_data(data)
  
  excel_success_unweighted <- create_excel_output(version1_unweighted, version2_unweighted,
                                                  quintile_stats, sample_data,
                                                  output_filename_unweighted, FALSE)
  excel_success_weighted <- FALSE
  if (weights_available) {
    excel_success_weighted <- create_excel_output(version1_weighted, version2_weighted,
                                                  NULL, NULL,
                                                  output_filename_weighted, TRUE)
  }
  
  message("\n", paste(rep("=", 60), collapse=""))
  message("SECUESTROS ANALYSIS SUMMARY:")
  message("✅ Data loaded and merged: ", nrow(data), " observations")
  message("✅ Models specified: ", length(model_specs), " (with quintile dummies and triple interactions)")
  message("✅ Dependent variable: secuestros (total by department-year)")
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
  if (excel_success_unweighted) message("✅ Unweighted Excel: ", output_filename_unweighted) else message("❌ Unweighted Excel failed")
  if (weights_available && excel_success_weighted) message("✅ Weighted Excel: ", output_filename_weighted)
  message("Secuestros analysis completed successfully!")
  
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

results <- run_focused_analysis()

message("\n📊 SECUESTROS ANALYSIS WITH QUINTILE DUMMIES COMPLETED")
message("🎯 Key Features:")
message("   - Dependent variable: secuestros (count by dept-year); log_secuestros available for descriptives")
message("   - Individual quintile dummies (Q2-Q5, Q1 as reference)")
message("   - Triple interactions: Myers × Peace × Quintiles")
message("   - Both unweighted and weighted versions (if weights present)")
message("   - All models include urban_area control")
message("   - Clustered standard errors at region level")
