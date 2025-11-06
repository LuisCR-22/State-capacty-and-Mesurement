########################################
# Colombia Myers-Benford Analysis - Regression Only V3.0
########################################
# Author: Luis Castellanos Rodriguez
# Modified: 2025-08-16
# Purpose: Re-run regressions with new model specifications using existing data
# Uses output from previous "02_" files as input

# METHODOLOGY:
# - Uses peace treaty (2016) as natural experiment treatment
# - Runs 4 nested regression models with increasing complexity
# - Tests at regional (24 departments) and regional-area (48 units) levels
# - Uses clustered standard errors and fixed effects for robustness
#
# KEY MODELS:
# Model 1: benford_distance ~ myers_index (baseline relationship)
# Model 2: + peace_treaty + myers×peace_treaty (treatment effect)
# Model 3: + rural/urban controls (+ myers×rural for reg-area only)
# Model 4: Full model (+ myers×peace_treaty; triple interaction only for reg-area)
#
# TECHNICAL FEATURES:
# - Clustered SEs by department to handle spatial correlation
# - Smart fixed effects strategy to avoid perfect multicollinearity:
#   * Models without peace treaty: Region + Year FE
#   * Models with peace treaty: Region FE only (avoids collinearity)
# - Enhanced error handling for collinearity detection
# - Processes existing "02_" files and outputs "03_" results
# - Handles two data versions (V01: raw, V02: outliers removed)
#
# OUTPUTS: Four Excel files with "03_" prefix, each containing 6 sheets:
# - Results: Original data from "02_" input files
# - Summary: Formatted analysis summary with model specifications
# - Coefficients_No_FE: Panel A regression results (no fixed effects)
# - Model_Stats_No_FE: Panel A model statistics (R², N, etc.)
# - Coefficients_FE: Panel B regression results (with region+year FE)
# - Model_Stats_FE: Panel B model statistics
# Files generated: 03_Myers_Benford_Col_V01/V02_reg-tot/reg-area.xlsx

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Install and load required packages
packages <- c("readxl", "writexl", "dplyr", "broom", "sandwich", "lmtest")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input path for existing Excel files (from "02_" analysis)
input_path_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col"

# Output path for new Excel files (will be "03_" prefix)
output_path_excel <- input_path_excel

# List of input files to process
input_files <- c(
  "02_Myers_Benford_Col_V01_reg-tot.xlsx",
  "02_Myers_Benford_Col_V01_reg-area.xlsx", 
  "02_Myers_Benford_Col_V02_reg-tot.xlsx",
  "02_Myers_Benford_Col_V02_reg-area.xlsx"
)

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

########################################
# REGRESSION ANALYSIS WITH NEW MODEL SPECIFICATIONS
########################################

run_new_regression_analysis <- function(data, version_name, level = "reg-tot") {
  # Define dependent and independent variables
  myers_var <- "standardized_myers"
  benford_var <- "benford_abs_distance"
  
  # Prepare regression data
  reg_data <- data[complete.cases(data[, c(myers_var, benford_var, "region_number", "ano")]), ]
  
  if (nrow(reg_data) < 10) {
    message("Insufficient data for regression analysis in ", version_name, " ", level)
    return(list(
      coefficients_no_fe = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE),
      coefficients_fe = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE),
      model_stats_no_fe = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE),
      model_stats_fe = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE)
    ))
  }
  
  # Check clustering viability
  cluster_table <- table(reg_data$region_number)
  min_cluster_size <- min(cluster_table)
  n_clusters <- length(cluster_table)
  
  if (n_clusters < 5) {
    message("WARNING: Only ", n_clusters, " clusters (regions) available. Clustered SEs may not be reliable.")
  }
  if (min_cluster_size < 3) {
    message("WARNING: Smallest cluster has only ", min_cluster_size, " observations.")
  }
  
  message("Clustering info for ", version_name, " ", level, ": ", n_clusters, " regions, cluster sizes: ", 
          min(cluster_table), "-", max(cluster_table), " observations")
  
  # Create After Peace Treaty dummy (1 if year >= 2016)
  reg_data$after_peace_treaty <- ifelse(reg_data$ano >= 2016, 1, 0)
  
  # Define regression specifications based on level
  if (level == "reg-area") {
    # For regional-area level: create rural dummy and use full interactions
    reg_data$rural <- 1 - reg_data$urban_area
    
    reg_specs <- list(
      "Model 1: Simple" = paste(benford_var, "~", myers_var),
      "Model 2: + After Peace Treaty" = paste(benford_var, "~", myers_var, "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
      "Model 3: + Rural" = paste(benford_var, "~", myers_var, "+ rural +", myers_var, ": rural"),
      "Model 4: Full Model" = paste(benford_var, "~", myers_var, "+ after_peace_treaty + rural +", 
                                    myers_var, ": after_peace_treaty +", myers_var, ": rural + after_peace_treaty : rural +",
                                    myers_var, ": after_peace_treaty : rural")
    )
  } else {
    # For regional level: use urban_share without interactions (not a dummy)
    reg_specs <- list(
      "Model 1: Simple" = paste(benford_var, "~", myers_var),
      "Model 2: + After Peace Treaty" = paste(benford_var, "~", myers_var, "+ after_peace_treaty +", myers_var, ": after_peace_treaty"),
      "Model 3: + Urban Share" = paste(benford_var, "~", myers_var, "+ urban_share"),
      "Model 4: Full Model" = paste(benford_var, "~", myers_var, "+ after_peace_treaty + urban_share +", myers_var, ": after_peace_treaty")
    )
  }
  
  # Function to run regression with clustered standard errors
  run_clustered_regression <- function(formula_str, data, include_fe = FALSE) {
    tryCatch({
      # Add fixed effects if requested
      if (include_fe && length(unique(data$region_number)) > 1 && length(unique(data$ano)) > 1) {
        # IMPORTANT: Check for collinearity between after_peace_treaty and year FE
        # If model includes after_peace_treaty, use only region FE to avoid perfect collinearity
        if (grepl("after_peace_treaty", formula_str)) {
          # Use only region FE when after_peace_treaty is included
          formula_str <- paste(formula_str, "+ factor(region_number)")
          message("    Using only region FE (not year FE) to avoid collinearity with after_peace_treaty")
        } else {
          # Use both region + year FE when after_peace_treaty is not included
          formula_str <- paste(formula_str, "+ factor(region_number) + factor(ano)")
        }
      }
      
      # Fit the model
      model <- lm(as.formula(formula_str), data = data)
      
      # Check for successful model fitting
      if (any(is.na(coef(model)))) {
        return(list(
          coefficients = data.frame(term = "Collinearity Error", estimate = NA, std.error = NA, 
                                    statistic = NA, p.value = NA, significance = "", stringsAsFactors = FALSE),
          model_stats = data.frame(r.squared = NA, adj.r.squared = NA, nobs = NA, stringsAsFactors = FALSE)
        ))
      }
      
      # Calculate clustered standard errors (clustered by region)
      # Ensure dimensions match by using the actual fitted model data
      model_data <- model$model
      region_cluster <- data$region_number[as.numeric(rownames(model_data))]
      
      clustered_vcov <- vcovCL(model, cluster = region_cluster)
      clustered_se <- sqrt(diag(clustered_vcov))
      
      # Get coefficient estimates
      coefficients <- coef(model)
      
      # Calculate t-statistics and p-values with clustered SEs
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
      message("    Error in regression: ", e$message)
      return(list(
        coefficients = data.frame(term = "Error", estimate = NA, std.error = NA, 
                                  statistic = NA, p.value = NA, significance = "", stringsAsFactors = FALSE),
        model_stats = data.frame(r.squared = NA, adj.r.squared = NA, nobs = NA, stringsAsFactors = FALSE)
      ))
    })
  }
  
  # Run regressions without fixed effects
  results_no_fe <- data.frame()
  model_stats_no_fe <- data.frame()
  
  for (i in seq_along(reg_specs)) {
    model_name <- names(reg_specs)[i]
    formula_str <- reg_specs[[i]]
    
    message("  Running ", model_name, " without FE...")
    result <- run_clustered_regression(formula_str, reg_data, include_fe = FALSE)
    
    if (!is.null(result) && !is.null(result$coefficients) && nrow(result$coefficients) > 0) {
      if (!"Error" %in% result$coefficients$term) {
        result$coefficients$Model <- model_name
        result$coefficients$Fixed_Effects <- "No"
        results_no_fe <- rbind(results_no_fe, result$coefficients)
        
        result$model_stats$Model <- model_name
        result$model_stats$Fixed_Effects <- "No"
        model_stats_no_fe <- rbind(model_stats_no_fe, result$model_stats)
      } else {
        message("    Error in model ", model_name, " without FE")
      }
    }
  }
  
  # Run regressions with fixed effects (if applicable)
  results_fe <- data.frame()
  model_stats_fe <- data.frame()
  
  if (length(unique(reg_data$region_number)) > 1 && length(unique(reg_data$ano)) > 1) {
    for (i in seq_along(reg_specs)) {
      model_name <- names(reg_specs)[i]
      formula_str <- reg_specs[[i]]
      
      message("  Running ", model_name, " with FE...")
      result <- run_clustered_regression(formula_str, reg_data, include_fe = TRUE)
      
      if (!is.null(result) && !is.null(result$coefficients) && nrow(result$coefficients) > 0) {
        if (!"Error" %in% result$coefficients$term) {
          result$coefficients$Model <- model_name
          result$coefficients$Fixed_Effects <- "Yes"
          results_fe <- rbind(results_fe, result$coefficients)
          
          result$model_stats$Model <- model_name
          result$model_stats$Fixed_Effects <- "Yes"
          model_stats_fe <- rbind(model_stats_fe, result$model_stats)
        } else {
          message("    Error in model ", model_name, " with FE")
        }
      }
    }
  } else {
    message("Insufficient variation for fixed effects (need multiple regions AND years)")
  }
  
  return(list(
    coefficients_no_fe = results_no_fe,
    model_stats_no_fe = model_stats_no_fe,
    coefficients_fe = results_fe,
    model_stats_fe = model_stats_fe
  ))
}

########################################
# FORMAT REGRESSION RESULTS FOR DISPLAY
########################################

format_regression_summary <- function(coeffs_no_fe, coeffs_fe, stats_no_fe, stats_fe, version_name, level) {
  
  # Create formatted summary text
  summary_text <- paste0("Colombia Myers-Benford Analysis - NEW SPECIFICATIONS\n")
  summary_text <- paste0(summary_text, "Version: ", version_name, " | Level: ", level, "\n")
  summary_text <- paste0(summary_text, "Date: ", Sys.Date(), "\n")
  summary_text <- paste0(summary_text, paste(rep("=", 80), collapse=""), "\n\n")
  
  summary_text <- paste0(summary_text, "MODEL SPECIFICATIONS:\n")
  summary_text <- paste0(summary_text, "Model 1: benford_abs_distance ~ standardized_myers\n")
  summary_text <- paste0(summary_text, "Model 2: + After Peace Treaty + Myers × After Peace Treaty\n")
  if (level == "reg-area") {
    summary_text <- paste0(summary_text, "Model 3: + Rural + Myers × Rural\n")
    summary_text <- paste0(summary_text, "Model 4: + All controls + Myers × After Peace Treaty × Rural (triple interaction)\n")
  } else {
    summary_text <- paste0(summary_text, "Model 3: + Urban Share (no interactions - continuous variable)\n")
    summary_text <- paste0(summary_text, "Model 4: + Urban Share + Myers × After Peace Treaty\n")
  }
  summary_text <- paste0(summary_text, "\nAfter Peace Treaty = 1 if year >= 2016, 0 otherwise\n")
  summary_text <- paste0(summary_text, "Standard errors clustered by region (department level)\n")
  summary_text <- paste0(summary_text, "Fixed Effects: Region + Year (Region only when after_peace_treaty included to avoid collinearity)\n\n")
  
  # Add key results summary
  if (nrow(coeffs_no_fe) > 0) {
    # Get key Myers coefficients from Model 1
    myers_coef_no_fe <- coeffs_no_fe[coeffs_no_fe$term == "standardized_myers" & 
                                       coeffs_no_fe$Model == "Model 1: Simple" & 
                                       coeffs_no_fe$Fixed_Effects == "No", ]
    
    if (nrow(myers_coef_no_fe) > 0) {
      summary_text <- paste0(summary_text, "KEY RESULT - Myers Index Coefficient (Model 1, No FE): ", 
                             round(myers_coef_no_fe$estimate[1], 4), 
                             myers_coef_no_fe$significance[1], "\n")
    }
  }
  
  if (nrow(coeffs_fe) > 0) {
    # Get key Myers coefficients from Model 1 with FE
    myers_coef_fe <- coeffs_fe[coeffs_fe$term == "standardized_myers" & 
                                 coeffs_fe$Model == "Model 1: Simple" & 
                                 coeffs_fe$Fixed_Effects == "Yes", ]
    
    if (nrow(myers_coef_fe) > 0) {
      summary_text <- paste0(summary_text, "KEY RESULT - Myers Index Coefficient (Model 1, With FE): ", 
                             round(myers_coef_fe$estimate[1], 4), 
                             myers_coef_fe$significance[1], "\n")
    }
  }
  
  summary_text <- paste0(summary_text, "\n*** p<0.01, ** p<0.05, * p<0.10\n")
  summary_text <- paste0(summary_text, paste(rep("=", 80), collapse=""), "\n")
  
  return(summary_text)
}

########################################
# MAIN PROCESSING FUNCTION
########################################

process_regression_files <- function() {
  
  message("Starting regression analysis with new model specifications...")
  
  # Process each input file
  for (input_file in input_files) {
    
    # Check if file exists
    input_file_path <- file.path(input_path_excel, input_file)
    if (!file.exists(input_file_path)) {
      message("File not found: ", input_file)
      next
    }
    
    message("Processing file: ", input_file)
    
    tryCatch({
      # Read the Results sheet from Excel
      data <- read_xlsx(input_file_path, sheet = "Results")
      
      if (nrow(data) == 0) {
        message("  No data in Results sheet")
        next
      }
      
      message("  Data loaded: ", nrow(data), " observations")
      
      # Extract version and level from filename
      # Expected format: "02_Myers_Benford_Col_V01_reg-tot.xlsx"
      file_parts <- strsplit(gsub(".xlsx", "", input_file), "_")[[1]]
      version <- file_parts[length(file_parts)-1]  # e.g., "V01"
      level_part <- file_parts[length(file_parts)]  # e.g., "reg-tot" 
      level <- ifelse(level_part == "reg-tot", "reg-tot", "reg-area")
      
      message("  Detected version: ", version, ", level: ", level)
      
      # Run new regression analysis
      reg_results <- run_new_regression_analysis(data, version, level)
      
      # Check if we have any results
      if (nrow(reg_results$coefficients_no_fe) == 0 && nrow(reg_results$coefficients_fe) == 0) {
        message("  No regression results generated")
        next
      }
      
      # Create formatted summary
      formatted_summary <- format_regression_summary(
        coeffs_no_fe = reg_results$coefficients_no_fe,
        coeffs_fe = reg_results$coefficients_fe,
        stats_no_fe = reg_results$model_stats_no_fe,
        stats_fe = reg_results$model_stats_fe,
        version_name = version,
        level = level
      )
      
      # Print summary to console
      cat("\n", formatted_summary, "\n")
      
      # Create output filename (change "02_" to "03_")
      output_filename <- gsub("02_", "03_", input_file)
      output_file_path <- file.path(output_path_excel, output_filename)
      
      # Create Excel output with all sheets
      excel_data <- list(
        "Results" = data,  # Keep original data
        "Summary" = data.frame(Summary = strsplit(formatted_summary, "\n")[[1]], stringsAsFactors = FALSE),
        "Coefficients_No_FE" = reg_results$coefficients_no_fe,
        "Model_Stats_No_FE" = reg_results$model_stats_no_fe,
        "Coefficients_FE" = reg_results$coefficients_fe,
        "Model_Stats_FE" = reg_results$model_stats_fe
      )
      
      write_xlsx(excel_data, output_file_path)
      message("  Results saved: ", output_filename)
      
      # Print summary statistics
      message("  Summary:")
      message("    Models without FE: ", length(unique(reg_results$coefficients_no_fe$Model)))
      message("    Models with FE: ", length(unique(reg_results$coefficients_fe$Model)))
      
      # Print key coefficient if available
      if (nrow(reg_results$coefficients_no_fe) > 0) {
        key_coef <- reg_results$coefficients_no_fe[reg_results$coefficients_no_fe$term == "standardized_myers" & 
                                                     reg_results$coefficients_no_fe$Model == "Model 1: Simple", ]
        if (nrow(key_coef) > 0) {
          message("    Key coefficient (Model 1, no FE): ", round(key_coef$estimate[1], 4), key_coef$significance[1])
        }
      }
      
      if (nrow(reg_results$coefficients_fe) > 0) {
        key_coef_fe <- reg_results$coefficients_fe[reg_results$coefficients_fe$term == "standardized_myers" & 
                                                     reg_results$coefficients_fe$Model == "Model 1: Simple", ]
        if (nrow(key_coef_fe) > 0) {
          message("    Key coefficient (Model 1, with FE): ", round(key_coef_fe$estimate[1], 4), key_coef_fe$significance[1])
        }
      }
      
    }, error = function(e) {
      message("  Error processing ", input_file, ": ", e$message)
    })
  }
  
  message("Regression analysis with new specifications complete!")
}

########################################
# MAIN EXECUTION
########################################

# Run the analysis
process_regression_files()

message("\n", paste(rep("=", 60), collapse=""))
message("ANÁLISIS COMPLETADO CON NUEVAS ESPECIFICACIONES")
message(paste(rep("=", 60), collapse=""))
message("\n1. NUEVOS MODELOS IMPLEMENTADOS:")
message("   Model 1: Relación bivariada simple")
message("   Model 2: + After Peace Treaty + Myers × After Peace Treaty")
message("   Model 3: + Rural/Urban Share + interacciones apropiadas")
message("   Model 4: Modelo completo con interacción triple (reg-area)")

message("\n2. ESPECIFICACIONES TÉCNICAS:")
message("   - Errores estándar clusterizados a nivel de departamento")
message("   - FE de tiempo y departamento para Panel B")
message("   - After Peace Treaty = 1 si año >= 2016")
message("   - Para reg-tot: urban_share sin interacciones (variable continua)")
message("   - Para reg-area: rural dummy con interacciones completas")

message("\n3. MANEJO DE MULTICOLINEALIDAD:")
message("   - FE a nivel de región (no región-área) para evitar multicolinealidad")
message("   - Variables rurales/urbanas compatibles con FE de región")

message("\n4. ARCHIVOS GENERADOS:")
message("   - Archivos Excel con prefijo '03_' en:")
message("   ", output_path_excel)
message("   - Incluyen: Datos + Resumen + Coeficientes Panel A y B + Estadísticas")

message("\n5. DIFERENCIAS CLAVE:")
message("   - V01: Datos crudos con ceros incluidos")
message("   - V02: Outliers individuales removidos, ceros excluidos")
message("   - reg-tot: 24 departamentos, urban_share continua")
message("   - reg-area: 48 unidades región-área, rural dummy")

########################################
# NOTAS TÉCNICAS IMPORTANTES
########################################
#
# ESPECIFICACIONES DE MODELOS:
# 
# REGIONAL LEVEL (reg-tot):
# Model 1: benford_abs_distance ~ standardized_myers
# Model 2: benford_abs_distance ~ standardized_myers + after_peace_treaty + standardized_myers:after_peace_treaty
# Model 3: benford_abs_distance ~ standardized_myers + urban_share
# Model 4: benford_abs_distance ~ standardized_myers + after_peace_treaty + urban_share + standardized_myers:after_peace_treaty
#
# REGIONAL-AREA LEVEL (reg-area):
# Model 1: benford_abs_distance ~ standardized_myers
# Model 2: benford_abs_distance ~ standardized_myers + after_peace_treaty + standardized_myers:after_peace_treaty  
# Model 3: benford_abs_distance ~ standardized_myers + rural + standardized_myers:rural
# Model 4: benford_abs_distance ~ standardized_myers + after_peace_treaty + rural + 
#          standardized_myers:after_peace_treaty + standardized_myers:rural + 
#          after_peace_treaty:rural + standardized_myers:after_peace_treaty:rural
#
# FIXED EFFECTS STRATEGY:
# - Panel A: Sin FE, errores clusterizados por región
# - Panel B: 
#   * Models sin after_peace_treaty: factor(region_number) + factor(ano)
#   * Models con after_peace_treaty: factor(region_number) únicamente
#     (para evitar perfect collinearity entre after_peace_treaty y year FE)
#
# VENTAJAS DE ESTE ENFOQUE:
# - Evita perfect collinearity entre after_peace_treaty y year FE
# - Preserva identificación del efecto tratado de paz
# - Mantiene errores clusterizados apropiados
# - Permite estimación robusta de todos los modelos
# - Interpretación económica clara
#