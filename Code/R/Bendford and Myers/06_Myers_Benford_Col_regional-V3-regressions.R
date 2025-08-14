########################################
# Colombia Myers-Benford Regression Analysis 
# Using Original Code Structure with Colombian Controls
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-07-20
# Purpose: Run regressions equivalent to original code structure but using Colombian controls

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Install and load required packages
packages <- c("readxl", "writexl", "dplyr", "data.table", "tidyverse", "broom")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input path for generated Excel files
input_path_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col"

# Output path for regression results
output_path_docs <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Docs/Col/Check"

# Create directory if it doesn't exist
dir.create(output_path_docs, showWarnings = FALSE, recursive = TRUE)

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

# Function to format regression results table (following original code structure)
format_regression_table <- function(results_no_fe, results_fe, stats_no_fe, stats_fe, version_name, level) {
  
  if (nrow(results_no_fe) == 0 && nrow(results_fe) == 0) {
    return("Insufficient data for regression analysis")
  }
  
  # Get unique models
  if (nrow(results_no_fe) > 0) {
    models <- unique(results_no_fe$Model)
  } else {
    models <- unique(results_fe$Model)
  }
  
  # Create formatted table
  table_text <- paste0("Colombia Regression Results - ", version_name, " - ", level, "\n")
  table_text <- paste0(table_text, "Dependent Variable: Benford Absolute Distance\n")
  table_text <- paste0(table_text, "=", paste(rep("=", 80), collapse=""), "\n\n")
  
  # Panel A: Without Fixed Effects
  if (nrow(results_no_fe) > 0) {
    table_text <- paste0(table_text, "Panel A: Without Fixed Effects\n")
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
    # Create header
    header <- sprintf("%-25s", "Variable")
    for (model in models) {
      model_short <- gsub("Model \\d+: ", "", model)
      header <- paste0(header, sprintf("%15s", substr(model_short, 1, 14)))
    }
    table_text <- paste0(table_text, header, "\n")
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
    # Get unique terms (excluding intercept)
    terms <- unique(results_no_fe$term[!grepl("Intercept", results_no_fe$term)])
    
    for (term in terms) {
      # Coefficient row
      coef_row <- sprintf("%-25s", term)
      for (model in models) {
        model_data <- results_no_fe[results_no_fe$Model == model & results_no_fe$term == term, ]
        if (nrow(model_data) > 0) {
          coef <- round(model_data$estimate[1], 4)
          stars <- add_significance_stars(model_data$p.value[1])
          coef_row <- paste0(coef_row, sprintf("%12s%3s", format(coef, nsmall=4), stars))
        } else {
          coef_row <- paste0(coef_row, sprintf("%15s", "-"))
        }
      }
      table_text <- paste0(table_text, coef_row, "\n")
      
      # Standard error row
      se_row <- sprintf("%-25s", "")
      for (model in models) {
        model_data <- results_no_fe[results_no_fe$Model == model & results_no_fe$term == term, ]
        if (nrow(model_data) > 0) {
          se <- round(model_data$std.error[1], 4)
          se_row <- paste0(se_row, sprintf("%15s", paste0("(", format(se, nsmall=4), ")")))
        } else {
          se_row <- paste0(se_row, sprintf("%15s", ""))
        }
      }
      table_text <- paste0(table_text, se_row, "\n")
    }
    
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
    # Add model statistics for Panel A
    if (!is.null(stats_no_fe) && nrow(stats_no_fe) > 0) {
      # Observations row
      obs_row <- sprintf("%-25s", "Observations")
      for (model in models) {
        model_stat <- stats_no_fe[stats_no_fe$Model == model, ]
        if (nrow(model_stat) > 0) {
          obs_row <- paste0(obs_row, sprintf("%15s", model_stat$nobs[1]))
        } else {
          obs_row <- paste0(obs_row, sprintf("%15s", "N/A"))
        }
      }
      table_text <- paste0(table_text, obs_row, "\n")
      
      # R-squared row
      r2_row <- sprintf("%-25s", "R-squared")
      for (model in models) {
        model_stat <- stats_no_fe[stats_no_fe$Model == model, ]
        if (nrow(model_stat) > 0) {
          r2 <- round(model_stat$r.squared[1], 4)
          r2_row <- paste0(r2_row, sprintf("%15s", format(r2, nsmall=4)))
        } else {
          r2_row <- paste0(r2_row, sprintf("%15s", "N/A"))
        }
      }
      table_text <- paste0(table_text, r2_row, "\n")
    }
    
    table_text <- paste0(table_text, "\n")
  }
  
  # Panel B: With Fixed Effects
  if (nrow(results_fe) > 0) {
    table_text <- paste0(table_text, "Panel B: With Region and Year Fixed Effects\n")
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
    # Create header
    header <- sprintf("%-25s", "Variable")
    for (model in models) {
      model_short <- gsub("Model \\d+: ", "", model)
      header <- paste0(header, sprintf("%15s", substr(model_short, 1, 14)))
    }
    table_text <- paste0(table_text, header, "\n")
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
    # Get unique terms (excluding intercept and fixed effects)
    terms <- unique(results_fe$term[!grepl("Intercept|factor\\(", results_fe$term)])
    
    for (term in terms) {
      # Coefficient row
      coef_row <- sprintf("%-25s", term)
      for (model in models) {
        model_data <- results_fe[results_fe$Model == model & results_fe$term == term, ]
        if (nrow(model_data) > 0) {
          coef <- round(model_data$estimate[1], 4)
          stars <- add_significance_stars(model_data$p.value[1])
          coef_row <- paste0(coef_row, sprintf("%12s%3s", format(coef, nsmall=4), stars))
        } else {
          coef_row <- paste0(coef_row, sprintf("%15s", "-"))
        }
      }
      table_text <- paste0(table_text, coef_row, "\n")
      
      # Standard error row
      se_row <- sprintf("%-25s", "")
      for (model in models) {
        model_data <- results_fe[results_fe$Model == model & results_fe$term == term, ]
        if (nrow(model_data) > 0) {
          se <- round(model_data$std.error[1], 4)
          se_row <- paste0(se_row, sprintf("%15s", paste0("(", format(se, nsmall=4), ")")))
        } else {
          se_row <- paste0(se_row, sprintf("%15s", ""))
        }
      }
      table_text <- paste0(table_text, se_row, "\n")
    }
    
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    table_text <- paste0(table_text, sprintf("%-25s", "Region FE"), sprintf("%15s", "Yes"), "\n")
    table_text <- paste0(table_text, sprintf("%-25s", "Year FE"), sprintf("%15s", "Yes"), "\n")
    
    # Add model statistics for Panel B
    if (!is.null(stats_fe) && nrow(stats_fe) > 0) {
      # Observations row
      obs_row <- sprintf("%-25s", "Observations")
      for (model in models) {
        model_stat <- stats_fe[stats_fe$Model == model, ]
        if (nrow(model_stat) > 0) {
          obs_row <- paste0(obs_row, sprintf("%15s", model_stat$nobs[1]))
        } else {
          obs_row <- paste0(obs_row, sprintf("%15s", "N/A"))
        }
      }
      table_text <- paste0(table_text, obs_row, "\n")
      
      # R-squared row
      r2_row <- sprintf("%-25s", "R-squared")
      for (model in models) {
        model_stat <- stats_fe[stats_fe$Model == model, ]
        if (nrow(model_stat) > 0) {
          r2 <- round(model_stat$r.squared[1], 4)
          r2_row <- paste0(r2_row, sprintf("%15s", format(r2, nsmall=4)))
        } else {
          r2_row <- paste0(r2_row, sprintf("%15s", "N/A"))
        }
      }
      table_text <- paste0(table_text, r2_row, "\n")
    }
  }
  
  # Add notes
  table_text <- paste0(table_text, "\n")
  table_text <- paste0(table_text, "Notes: Standard errors in parentheses\n")
  table_text <- paste0(table_text, "*** p<0.01, ** p<0.05, * p<0.10\n")
  
  return(table_text)
}

########################################
# REGRESSION ANALYSIS FUNCTION
########################################

run_regression_analysis_original_structure <- function(data, version_num, level = "reg-tot") {
  # Determine variables to use based on version
  if (version_num %in% c(2, 3)) {
    myers_var <- "residual_standardized_myers"
    benford_var <- "residual_benford_abs_distance"
  } else {
    myers_var <- "standardized_myers"
    benford_var <- "benford_abs_distance"
  }
  
  # Prepare regression data
  reg_data <- data[complete.cases(data[, c(myers_var, benford_var)]), ]
  
  if (nrow(reg_data) < 5) {
    return(list(
      results_no_fe = data.frame(),
      results_fe = data.frame(),
      stats_no_fe = data.frame(),
      stats_fe = data.frame()
    ))
  }
  
  # Create PT dummy (1 if year >= 2016)
  reg_data$PT <- ifelse(reg_data$ano >= 2016, 1, 0)
  
  # Define regression specifications adapted to Colombian controls
  if (level == "reg-area") {
    # For region-area level
    reg_data$rural <- 1 - reg_data$urban_area
    
    reg_specs <- list(
      "Model 1: Simple" = paste(benford_var, "~", myers_var),
      "Model 2: + PT" = paste(benford_var, "~", myers_var, "+ PT"),
      "Model 3: + PT + Rural" = paste(benford_var, "~", myers_var, "+ PT + rural"),
      "Model 4: + PT*Rural" = paste(benford_var, "~", myers_var, "+ PT + rural + PT:rural")
    )
  } else {
    # For regional level  
    reg_specs <- list(
      "Model 1: Simple" = paste(benford_var, "~", myers_var),
      "Model 2: + PT" = paste(benford_var, "~", myers_var, "+ PT"),
      "Model 3: + Urban" = paste(benford_var, "~", myers_var, "+ PT + urban_share"),
      "Model 4: + PT*Urban" = paste(benford_var, "~", myers_var, "+ PT + urban_share + PT:urban_share")
    )
  }
  
  # Run regressions WITHOUT fixed effects
  results_no_fe <- data.frame()
  model_stats_no_fe <- data.frame()
  
  for (i in seq_along(reg_specs)) {
    model_name <- names(reg_specs)[i]
    formula_str <- reg_specs[[i]]
    
    tryCatch({
      model <- lm(as.formula(formula_str), data = reg_data)
      
      # Get coefficient information
      model_summary <- tidy(model)
      model_summary$Model <- model_name
      results_no_fe <- rbind(results_no_fe, model_summary)
      
      # Get model statistics
      model_glance <- glance(model)
      model_glance$Model <- model_name
      model_stats_no_fe <- rbind(model_stats_no_fe, model_glance)
      
    }, error = function(e) {
      message("Error in regression ", model_name, ": ", e$message)
    })
  }
  
  # Run regressions WITH fixed effects (if applicable)
  results_fe <- data.frame()
  model_stats_fe <- data.frame()
  
  # Check if we have multiple regions and years for fixed effects
  n_regions <- length(unique(reg_data$region_number))
  n_years <- length(unique(reg_data$ano))
  
  if (n_regions > 1 && n_years > 1) {
    for (i in seq_along(reg_specs)) {
      model_name <- names(reg_specs)[i]
      
      # Add fixed effects to the formula
      if (level == "reg-area") {
        # For region-area level: use region-area fixed effects
        reg_data$region_area_id <- paste(reg_data$region_number, reg_data$urban_area, sep = "_")
        formula_str <- paste(reg_specs[[i]], "+ factor(region_area_id) + factor(ano)")
      } else {
        # For regional level: use region fixed effects
        formula_str <- paste(reg_specs[[i]], "+ factor(region_number) + factor(ano)")
      }
      
      tryCatch({
        model <- lm(as.formula(formula_str), data = reg_data)
        
        # Get coefficient information
        model_summary <- tidy(model)
        model_summary$Model <- model_name
        results_fe <- rbind(results_fe, model_summary)
        
        # Get model statistics
        model_glance <- glance(model)
        model_glance$Model <- model_name
        model_stats_fe <- rbind(model_stats_fe, model_glance)
        
      }, error = function(e) {
        message("Error in regression with FE ", model_name, ": ", e$message)
      })
    }
  } else {
    message("Insufficient variation for fixed effects: ", n_regions, " regions, ", n_years, " years")
  }
  
  return(list(
    results_no_fe = results_no_fe,
    results_fe = results_fe,
    stats_no_fe = model_stats_no_fe,
    stats_fe = model_stats_fe
  ))
}

########################################
# MAIN PROCESSING FUNCTION
########################################

process_colombia_regression_analysis <- function() {
  
  message("Starting Colombia regression analysis using original code structure...")
  
  # Define all versions and levels to process
  versions <- c("V00", "V01", "V02", "V03")
  levels <- c("reg-tot", "reg-area")
  
  for (level in levels) {
    message("Processing level: ", level)
    
    level_suffix <- paste0("_", level)
    
    for (version in versions) {
      message("  Processing version: ", version)
      
      # Construct Excel file path
      excel_filename <- paste0("01_Myers_Benford_Col_", version, level_suffix, ".xlsx")
      excel_path <- file.path(input_path_excel, excel_filename)
      
      # Check if file exists
      if (!file.exists(excel_path)) {
        message("    File not found: ", excel_filename)
        next
      }
      
      tryCatch({
        # Read the Results sheet from Excel
        message("    Loading data from Excel...")
        data <- read_xlsx(excel_path, sheet = "Results")
        
        if (nrow(data) == 0) {
          message("    No data in Results sheet")
          next
        }
        
        message("    Data loaded: ", nrow(data), " observations")
        
        # Determine version number for variable selection
        version_num <- as.numeric(substr(version, 2, 3))
        
        # Run regression analysis following original structure
        reg_results <- run_regression_analysis_original_structure(data, version_num, level)
        
        # Check if we have any results
        if (nrow(reg_results$results_no_fe) == 0 && nrow(reg_results$results_fe) == 0) {
          message("    No regression results generated")
          next
        }
        
        # Format results table
        formatted_table <- format_regression_table(
          results_no_fe = reg_results$results_no_fe,
          results_fe = reg_results$results_fe,
          stats_no_fe = reg_results$stats_no_fe,
          stats_fe = reg_results$stats_fe,
          version_name = version,
          level = level
        )
        
        # Save results
        output_filename <- paste0("Regression_Check_", version, level_suffix, ".txt")
        output_path <- file.path(output_path_docs, output_filename)
        
        writeLines(formatted_table, output_path)
        message("    Results saved: ", output_filename)
        
        # Print summary to console
        message("    Summary:")
        message("      Models without FE: ", length(unique(reg_results$results_no_fe$Model)))
        message("      Models with FE: ", length(unique(reg_results$results_fe$Model)))
        
        # Print key coefficient if available
        if (nrow(reg_results$results_no_fe) > 0) {
          key_coef <- reg_results$results_no_fe[grepl("standardized_myers|residual_standardized_myers", 
                                                      reg_results$results_no_fe$term) & 
                                                  reg_results$results_no_fe$Model == "Model 1: Simple", ]
          if (nrow(key_coef) > 0) {
            message("      Key coefficient (Model 1, no FE): ", round(key_coef$estimate[1], 4))
          }
        }
        
        if (nrow(reg_results$results_fe) > 0) {
          key_coef_fe <- reg_results$results_fe[grepl("standardized_myers|residual_standardized_myers", 
                                                      reg_results$results_fe$term) & 
                                                  reg_results$results_fe$Model == "Model 1: Simple", ]
          if (nrow(key_coef_fe) > 0) {
            message("      Key coefficient (Model 1, with FE): ", round(key_coef_fe$estimate[1], 4))
          }
        }
        
      }, error = function(e) {
        message("    Error processing ", version, " ", level, ": ", e$message)
      })
    }
  }
  
  message("Colombia regression analysis complete!")
}

########################################
# MAIN EXECUTION
########################################

# Run the analysis
process_colombia_regression_analysis()

message("\n", paste(rep("=", 60), collapse=""))
message("IMPORTANT NOTES:")
message(paste(rep("=", 60), collapse=""))
message("\n1. REGRESSION STRUCTURE:")
message("   - Panel A: Without Fixed Effects (pooled OLS)")
message("   - Panel B: With Region and Year Fixed Effects")
message("   - Following original code structure with Colombian controls")

message("\n2. VERSION DIFFERENCES:")
message("   - V00/V01: Uses original variables (standardized_myers, benford_abs_distance)")
message("   - V02/V03: Uses residualized variables (residual_standardized_myers, residual_benford_abs_distance)")

message("\n3. CONTROLS ADAPTED:")
message("   - Regional level: PT dummy + urban_share + PT×urban_share interaction")
message("   - Regional-area level: PT dummy + rural dummy + PT×rural interaction")
message("   - PT = 1 if year >= 2016, 0 otherwise")

message("\n4. FIXED EFFECTS:")
message("   - Regional level: factor(region_number) + factor(ano)")
message("   - Regional-area level: factor(region_area_id) + factor(ano)")
message("   - Only applied when multiple regions AND years available")

message("\n5. KEY DIFFERENCE FROM COLOMBIAN CODE:")
message("   - This follows ORIGINAL structure: explicit FE in regression formulas")
message("   - Colombian code: pre-residualization approach")
message("   - Results should be similar for V02/V03 Panel B vs Colombian V02/V03")

message("\n6. FILES SAVED TO:")
message("   ", output_path_docs)

########################################
# ANALYSIS NOTES
########################################
#
# This code replicates the ORIGINAL regression structure but using the 
# data generated by the Colombian code. Key differences:
#
# ORIGINAL APPROACH (this code):
# - Panel A: Y = α + β*Myers + γ*Controls + ε
# - Panel B: Y = α + β*Myers + γ*Controls + δ*Region_FE + η*Year_FE + ε
#
# CONTROLS:
# - Regional level: PT + urban_share + PT×urban_share interaction (4 models)
# - Regional-area level: PT + rural + PT×rural interaction (4 models)
#
# COLOMBIAN APPROACH (previous code):
# - V00/V01: Y = α + β*Myers + γ*Controls + ε  
# - V02/V03: Y_residual = α + β*Myers_residual + γ*Controls + ε
#            where residuals are from: Var ~ Region_FE + Year_FE
#
# The Panel B results from this code should be equivalent to the 
# Colombian V02/V03 results, but the presentation differs:
# - This code shows explicit FE coefficients (though not reported)
# - Colombian code shows results on pre-residualized variables
#
# Both approaches control for the same unobserved heterogeneity,
# just at different stages of the analysis.