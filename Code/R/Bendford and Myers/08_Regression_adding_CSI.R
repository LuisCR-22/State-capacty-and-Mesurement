########################################
# Colombia Myers-Benford-CSI Regression Analysis 
########################################
# Author: Luis Castellanos Rodriguez  
# Modified: 2025-07-20
# Purpose: Run regressions with CSI variables using Colombian data

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Install and load required packages
packages <- c("haven", "writexl", "dplyr", "data.table", "tidyverse", "broom")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input path for .dta files
input_path_dta <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Merge"

# Base output path for regression results
base_output_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Docs/Col"

# CSI folder names
csi_folders <- c("CSI pre ave", "CSI pre max", "CSI post ave", "CSI post max")

# Create directories
for (folder in csi_folders) {
  dir.create(file.path(base_output_path, folder), showWarnings = FALSE, recursive = TRUE)
}

########################################
# CSI VARIABLE MAPPING
########################################

# Map folder names to actual CSI variable names
csi_var_mapping <- list(
  "CSI pre ave" = "csi_pre1500_avg_no0",
  "CSI pre max" = "csi_pre1500_max_no0", 
  "CSI post ave" = "csi_post1500_avg_no0",
  "CSI post max" = "csi_post1500_max_no0"
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

# Function to format regression results table with CSI specifications
format_csi_regression_table <- function(results_no_fe, results_fe, stats_no_fe, stats_fe, 
                                        version_name, level, csi_name) {
  
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
  table_text <- paste0("Colombia CSI Regression Results - ", version_name, " - ", level, "\n")
  table_text <- paste0(table_text, "CSI Variable: ", csi_name, "\n")
  table_text <- paste0(table_text, "Dependent Variable: Benford Absolute Distance\n")
  table_text <- paste0(table_text, "=", paste(rep("=", 120), collapse=""), "\n\n")
  
  # Panel A: Without Fixed Effects
  if (nrow(results_no_fe) > 0) {
    table_text <- paste0(table_text, "Panel A: Without Fixed Effects\n")
    table_text <- paste0(table_text, paste(rep("-", 120), collapse=""), "\n")
    
    # Create header for 8 models
    header <- sprintf("%-20s", "Variable")
    for (model in models) {
      model_short <- gsub("Model \\d+: ", "", model)
      header <- paste0(header, sprintf("%12s", substr(model_short, 1, 11)))
    }
    table_text <- paste0(table_text, header, "\n")
    table_text <- paste0(table_text, paste(rep("-", 120), collapse=""), "\n")
    
    # Get unique terms (excluding intercept)
    terms <- unique(results_no_fe$term[!grepl("Intercept", results_no_fe$term)])
    
    for (term in terms) {
      # Coefficient row
      coef_row <- sprintf("%-20s", term)
      for (model in models) {
        model_data <- results_no_fe[results_no_fe$Model == model & results_no_fe$term == term, ]
        if (nrow(model_data) > 0) {
          coef <- round(model_data$estimate[1], 4)
          stars <- add_significance_stars(model_data$p.value[1])
          coef_row <- paste0(coef_row, sprintf("%9s%3s", format(coef, nsmall=4), stars))
        } else {
          coef_row <- paste0(coef_row, sprintf("%12s", "-"))
        }
      }
      table_text <- paste0(table_text, coef_row, "\n")
      
      # Standard error row
      se_row <- sprintf("%-20s", "")
      for (model in models) {
        model_data <- results_no_fe[results_no_fe$Model == model & results_no_fe$term == term, ]
        if (nrow(model_data) > 0) {
          se <- round(model_data$std.error[1], 4)
          se_row <- paste0(se_row, sprintf("%12s", paste0("(", format(se, nsmall=4), ")")))
        } else {
          se_row <- paste0(se_row, sprintf("%12s", ""))
        }
      }
      table_text <- paste0(table_text, se_row, "\n")
    }
    
    table_text <- paste0(table_text, paste(rep("-", 120), collapse=""), "\n")
    
    # Add model statistics for Panel A
    if (!is.null(stats_no_fe) && nrow(stats_no_fe) > 0) {
      # Observations row
      obs_row <- sprintf("%-20s", "Observations")
      for (model in models) {
        model_stat <- stats_no_fe[stats_no_fe$Model == model, ]
        if (nrow(model_stat) > 0) {
          obs_row <- paste0(obs_row, sprintf("%12s", model_stat$nobs[1]))
        } else {
          obs_row <- paste0(obs_row, sprintf("%12s", "N/A"))
        }
      }
      table_text <- paste0(table_text, obs_row, "\n")
      
      # R-squared row
      r2_row <- sprintf("%-20s", "R-squared")
      for (model in models) {
        model_stat <- stats_no_fe[stats_no_fe$Model == model, ]
        if (nrow(model_stat) > 0) {
          r2 <- round(model_stat$r.squared[1], 4)
          r2_row <- paste0(r2_row, sprintf("%12s", format(r2, nsmall=4)))
        } else {
          r2_row <- paste0(r2_row, sprintf("%12s", "N/A"))
        }
      }
      table_text <- paste0(table_text, r2_row, "\n")
    }
    
    table_text <- paste0(table_text, "\n")
  }
  
  # Panel B: With Fixed Effects
  if (nrow(results_fe) > 0) {
    table_text <- paste0(table_text, "Panel B: With Region and Year Fixed Effects\n")
    table_text <- paste0(table_text, paste(rep("-", 120), collapse=""), "\n")
    
    # Create header
    header <- sprintf("%-20s", "Variable")
    for (model in models) {
      model_short <- gsub("Model \\d+: ", "", model)
      header <- paste0(header, sprintf("%12s", substr(model_short, 1, 11)))
    }
    table_text <- paste0(table_text, header, "\n")
    table_text <- paste0(table_text, paste(rep("-", 120), collapse=""), "\n")
    
    # Get unique terms (excluding intercept and fixed effects)
    terms <- unique(results_fe$term[!grepl("Intercept|factor\\(", results_fe$term)])
    
    for (term in terms) {
      # Coefficient row
      coef_row <- sprintf("%-20s", term)
      for (model in models) {
        model_data <- results_fe[results_fe$Model == model & results_fe$term == term, ]
        if (nrow(model_data) > 0) {
          coef <- round(model_data$estimate[1], 4)
          stars <- add_significance_stars(model_data$p.value[1])
          coef_row <- paste0(coef_row, sprintf("%9s%3s", format(coef, nsmall=4), stars))
        } else {
          coef_row <- paste0(coef_row, sprintf("%12s", "-"))
        }
      }
      table_text <- paste0(table_text, coef_row, "\n")
      
      # Standard error row
      se_row <- sprintf("%-20s", "")
      for (model in models) {
        model_data <- results_fe[results_fe$Model == model & results_fe$term == term, ]
        if (nrow(model_data) > 0) {
          se <- round(model_data$std.error[1], 4)
          se_row <- paste0(se_row, sprintf("%12s", paste0("(", format(se, nsmall=4), ")")))
        } else {
          se_row <- paste0(se_row, sprintf("%12s", ""))
        }
      }
      table_text <- paste0(table_text, se_row, "\n")
    }
    
    table_text <- paste0(table_text, paste(rep("-", 120), collapse=""), "\n")
    table_text <- paste0(table_text, sprintf("%-20s", "Region FE"), sprintf("%12s", "Yes"), "\n")
    table_text <- paste0(table_text, sprintf("%-20s", "Year FE"), sprintf("%12s", "Yes"), "\n")
    
    # Add model statistics for Panel B
    if (!is.null(stats_fe) && nrow(stats_fe) > 0) {
      # Observations row
      obs_row <- sprintf("%-20s", "Observations")
      for (model in models) {
        model_stat <- stats_fe[stats_fe$Model == model, ]
        if (nrow(model_stat) > 0) {
          obs_row <- paste0(obs_row, sprintf("%12s", model_stat$nobs[1]))
        } else {
          obs_row <- paste0(obs_row, sprintf("%12s", "N/A"))
        }
      }
      table_text <- paste0(table_text, obs_row, "\n")
      
      # R-squared row
      r2_row <- sprintf("%-20s", "R-squared")
      for (model in models) {
        model_stat <- stats_fe[stats_fe$Model == model, ]
        if (nrow(model_stat) > 0) {
          r2 <- round(model_stat$r.squared[1], 4)
          r2_row <- paste0(r2_row, sprintf("%12s", format(r2, nsmall=4)))
        } else {
          r2_row <- paste0(r2_row, sprintf("%12s", "N/A"))
        }
      }
      table_text <- paste0(table_text, r2_row, "\n")
    }
  }
  
  # Add notes
  table_text <- paste0(table_text, "\n")
  table_text <- paste0(table_text, "Notes: Standard errors in parentheses\n")
  table_text <- paste0(table_text, "*** p<0.01, ** p<0.05, * p<0.10\n")
  table_text <- paste0(table_text, "Models 1-4: Base specifications\n")
  table_text <- paste0(table_text, "Models 5-8: Same specifications + Urban control\n")
  table_text <- paste0(table_text, "PT = 1 if year >= 2016, 0 otherwise\n")
  if (level == "reg-tot") {
    table_text <- paste0(table_text, "Urban control: urban_share (regional level)\n")
  } else {
    table_text <- paste0(table_text, "Urban control: urban_area (regional-area level)\n")
  }
  
  return(table_text)
}

########################################
# CSI REGRESSION ANALYSIS FUNCTION
########################################

run_csi_regression_analysis <- function(data, version_num, level = "reg-tot", csi_var) {
  # Determine variables to use based on version
  if (version_num %in% c(2, 3)) {
    myers_var <- "resid_myers_std"
    benford_var <- "resid_benf_abs_dist"
  } else {
    myers_var <- "myers_std"
    benford_var <- "benf_abs_dist_no_zero"
  }
  
  # Prepare regression data - include CSI variable
  required_vars <- c(myers_var, benford_var, csi_var, "ano", "region_number")
  
  # Add urban variable based on level
  if (level == "reg-tot") {
    urban_var <- "urban_share"
  } else {
    urban_var <- "urban_area"
  }
  required_vars <- c(required_vars, urban_var)
  
  # Check which variables exist
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    message("    Missing variables: ", paste(missing_vars, collapse = ", "))
    return(list(
      results_no_fe = data.frame(),
      results_fe = data.frame(),
      stats_no_fe = data.frame(),
      stats_fe = data.frame()
    ))
  }
  
  reg_data <- data[complete.cases(data[, required_vars]), ]
  
  if (nrow(reg_data) < 10) {
    message("    Insufficient observations: ", nrow(reg_data))
    return(list(
      results_no_fe = data.frame(),
      results_fe = data.frame(),
      stats_no_fe = data.frame(),
      stats_fe = data.frame()
    ))
  }
  
  # Create PT dummy (1 if year >= 2016)
  reg_data$PT <- ifelse(reg_data$ano >= 2016, 1, 0)
  
  # Define 8 regression specifications
  reg_specs <- list(
    # Base specifications (1-4)
    "Model 1: Myers" = paste(benford_var, "~", myers_var),
    "Model 2: + PT×Myers" = paste(benford_var, "~", myers_var, "+ PT + PT:", myers_var),
    "Model 3: + CSI×Myers" = paste(benford_var, "~", myers_var, "+ PT + PT:", myers_var, "+", csi_var, "+", csi_var, ":", myers_var),
    "Model 4: + CSI×Myers×PT" = paste(benford_var, "~", myers_var, "+ PT + PT:", myers_var, "+", csi_var, "+", csi_var, ":", myers_var, "+", csi_var, ":", myers_var, ": PT"),
    
    # With urban controls (5-8)
    "Model 5: Myers + Urban" = paste(benford_var, "~", myers_var, "+", urban_var),
    "Model 6: + PT×Myers + Urban" = paste(benford_var, "~", myers_var, "+ PT + PT:", myers_var, "+", urban_var),
    "Model 7: + CSI×Myers + Urban" = paste(benford_var, "~", myers_var, "+ PT + PT:", myers_var, "+", csi_var, "+", csi_var, ":", myers_var, "+", urban_var),
    "Model 8: + CSI×Myers×PT + Urban" = paste(benford_var, "~", myers_var, "+ PT + PT:", myers_var, "+", csi_var, "+", csi_var, ":", myers_var, "+", csi_var, ":", myers_var, ": PT +", urban_var)
  )
  
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
      message("      Error in regression ", model_name, ": ", e$message)
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
        message("      Error in FE regression ", model_name, ": ", e$message)
      })
    }
  } else {
    message("    Insufficient variation for fixed effects: ", n_regions, " regions, ", n_years, " years")
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

process_csi_regression_analysis <- function() {
  
  message("Starting Colombia CSI regression analysis...")
  
  # Define versions and levels to process
  versions <- c("V00", "V01", "V02", "V03")
  levels <- c("reg-tot", "reg-area")
  
  # Define .dta file names
  dta_files <- list(
    "reg-tot" = "01_col_merge_sedlac_CSI_dept.dta",
    "reg-area" = "01_col_merge_sedlac_CSI_dept_area.dta"
  )
  
  # Process each CSI variant
  for (csi_folder in csi_folders) {
    message("\nProcessing CSI variant: ", csi_folder)
    csi_var <- csi_var_mapping[[csi_folder]]
    
    # Create output folder path
    output_folder <- file.path(base_output_path, csi_folder)
    
    for (level in levels) {
      message("  Processing level: ", level)
      
      # Load the appropriate .dta file
      dta_path <- file.path(input_path_dta, dta_files[[level]])
      
      if (!file.exists(dta_path)) {
        message("    File not found: ", dta_files[[level]])
        next
      }
      
      tryCatch({
        # Load data
        message("    Loading data from .dta file...")
        data <- read_dta(dta_path)
        
        if (nrow(data) == 0) {
          message("    No data in file")
          next
        }
        
        # Check if CSI variable exists
        if (!csi_var %in% names(data)) {
          message("    CSI variable not found: ", csi_var)
          next
        }
        
        message("    Data loaded: ", nrow(data), " observations")
        
        # Process each version
        for (version in versions) {
          message("    Processing version: ", version)
          
          # Determine version number for variable selection
          version_num <- as.numeric(substr(version, 2, 3))
          
          # Run CSI regression analysis
          reg_results <- run_csi_regression_analysis(data, version_num, level, csi_var)
          
          # Check if we have any results
          if (nrow(reg_results$results_no_fe) == 0 && nrow(reg_results$results_fe) == 0) {
            message("      No regression results generated")
            next
          }
          
          # Format results table
          formatted_table <- format_csi_regression_table(
            results_no_fe = reg_results$results_no_fe,
            results_fe = reg_results$results_fe,
            stats_no_fe = reg_results$stats_no_fe,
            stats_fe = reg_results$stats_fe,
            version_name = version,
            level = level,
            csi_name = csi_var
          )
          
          # Save results
          output_filename <- paste0("02_Regression_Check_", version, "_", level, "_CSI.txt")
          output_path <- file.path(output_folder, output_filename)
          
          writeLines(formatted_table, output_path)
          message("      Results saved: ", output_filename)
          
          # Print summary to console
          message("      Summary:")
          message("        Models without FE: ", length(unique(reg_results$results_no_fe$Model)))
          message("        Models with FE: ", length(unique(reg_results$results_fe$Model)))
          
          # Print key coefficient if available
          if (nrow(reg_results$results_no_fe) > 0) {
            key_coef <- reg_results$results_no_fe[grepl("myers_std|resid_myers_std", 
                                                        reg_results$results_no_fe$term) & 
                                                    reg_results$results_no_fe$Model == "Model 1: Myers", ]
            if (nrow(key_coef) > 0) {
              message("        Key Myers coefficient (Model 1, no FE): ", round(key_coef$estimate[1], 4))
            }
          }
          
          # Print CSI coefficient if available  
          if (nrow(reg_results$results_no_fe) > 0) {
            csi_coef <- reg_results$results_no_fe[reg_results$results_no_fe$term == csi_var & 
                                                    reg_results$results_no_fe$Model == "Model 3: + CSI×Myers", ]
            if (nrow(csi_coef) > 0) {
              message("        CSI coefficient (Model 3, no FE): ", round(csi_coef$estimate[1], 4))
            }
          }
        }
        
      }, error = function(e) {
        message("    Error processing level ", level, ": ", e$message)
      })
    }
  }
  
  message("\nColombia CSI regression analysis complete!")
}

########################################
# MAIN EXECUTION
########################################

# Run the analysis
process_csi_regression_analysis()

message("\n", paste(rep("=", 80), collapse=""))
message("CSI REGRESSION ANALYSIS SUMMARY")
message(paste(rep("=", 80), collapse=""))

message("\n📊 REGRESSION SPECIFICATIONS:")
message("   Model 1: Benford ~ Myers")
message("   Model 2: Benford ~ Myers + PT + Myers×PT")
message("   Model 3: Benford ~ Myers + PT + Myers×PT + CSI + Myers×CSI")
message("   Model 4: Benford ~ Myers + PT + Myers×PT + CSI + Myers×CSI + Myers×CSI×PT")
message("   Model 5: Model 1 + Urban")
message("   Model 6: Model 2 + Urban") 
message("   Model 7: Model 3 + Urban")
message("   Model 8: Model 4 + Urban")

message("\n🌾 CSI VARIANTS ANALYZED:")
message("   • CSI pre ave: Pre-1500CE Average Calories")
message("   • CSI pre max: Pre-1500CE Maximum Calories")
message("   • CSI post ave: Post-1500CE Average Calories")
message("   • CSI post max: Post-1500CE Maximum Calories")

message("\n📁 RESULTS SAVED TO:")
for (folder in csi_folders) {
  message("   ", file.path(base_output_path, folder))
}

message("\n🔢 VERSIONS PROCESSED:")
message("   V00/V01: Original variables (myers_std, benf_abs_dist_no_zero)")
message("   V02/V03: Residualized variables (resid_myers_std, resid_benf_abs_dist)")

message("\n🏛️ PANELS:")
message("   Panel A: Without Fixed Effects (pooled OLS)")
message("   Panel B: With Region and Year Fixed Effects")

message("\n🏘️ URBAN CONTROLS:")
message("   reg-tot level: urban_share")
message("   reg-area level: urban_area")

message("\n⏰ PT VARIABLE:")
message("   PT = 1 if year >= 2016, 0 otherwise")

message("\n🎯 KEY INTERACTIONS:")
message("   • Myers × PT (all models 2+)")
message("   • Myers × CSI (all models 3+)")
message("   • Myers × CSI × PT (triple interaction in models 4 & 8)")

message(paste(rep("=", 80), collapse=""))

# Display any warnings
if(length(warnings()) > 0) {
  message("\n⚠️  Warnings encountered:")
  print(warnings())
}