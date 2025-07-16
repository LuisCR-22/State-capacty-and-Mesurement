########################################
# Benford Wage Analysis - Enhanced Version 2.0
########################################
# Author: Luis Castellanos Rodriguez
# Modified: 2025-07-16
# Enhanced with V00-V03 versions, regression analysis, and improved efficiency

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Test mode: Set to TRUE to process only ARM and BOL files
TEST_MODE <- TRUE  # Change to FALSE to process all countries

# Install and load required packages
packages <- c("haven", "writexl", "dplyr", "data.table", "ggplot2", "tidyverse", 
              "ggrepel", "gridExtra", "RColorBrewer", "broom")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Updated input path
input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Shared/FY2025/Technology and Inequality project/Raw data/GLD harmonization"

# Updated output paths
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_path_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"
output_path_docs <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Docs"
output_path_plots_main <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/PNG/Myers vs. Bendford/01 Myers vs Benford individual level"
output_path_plots_checks <- file.path(output_path_plots_main, "Checks")

# Create directories if they don't exist
dir.create(output_path_excel, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_docs, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots_main, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots_checks, showWarnings = FALSE, recursive = TRUE)

########################################
# STATISTICAL CONSTANTS AND FUNCTIONS
########################################

# Define the Benford expected probabilities for digits 1 to 9
benford_expected <- log10(1 + 1/(1:9))

# Initialize global result data frame
results <- data.frame(countrycode = character(),
                      year = integer(),
                      n_observations = integer(),
                      traditional_myers = numeric(),
                      standardized_myers = numeric(),
                      benford_abs_distance = numeric(),
                      benford_chi_square = numeric(),
                      urban_share = numeric(),
                      male_share = numeric(),
                      literacy_share = numeric(),
                      unemployment_share = numeric(),
                      stringsAsFactors = FALSE)

########################################
# MYERS INDEX CALCULATION (Age 10-79)
########################################

calculate_traditional_myers <- function(age_data) {
  # Ensure ages are numeric
  ages <- as.numeric(age_data)
  
  # Filter to Lee's suggested age range (10-79)
  ages <- ages[ages >= 10 & ages <= 79]
  
  # If insufficient data, return NA
  if(length(ages) < 100) {
    return(list(traditional = NA, standardized = NA))
  }
  
  # Extract terminal digits
  terminal_digits <- ages %% 10
  
  # Calculate the blended count for each terminal digit using Lee's method
  blended_counts <- numeric(10)
  
  # For each terminal digit (0-9), calculate blended population
  for(d in 0:9) {
    sum_count <- 0
    # Create 10 different age bins starting from each terminal digit
    for(start_digit in 0:9) {
      # Count ages in range [10+start_digit, 69+start_digit] ending in digit d
      for(age in seq(10 + start_digit, 79, by = 10)) {
        if(age <= 79) {
          target_age <- age + (d - start_digit)
          if(target_age >= 10 && target_age <= 79 && target_age %% 10 == d) {
            sum_count <- sum_count + sum(ages == target_age)
          }
        }
      }
    }
    blended_counts[d+1] <- sum_count
  }
  
  # Calculate percent distribution
  total_count <- sum(blended_counts)
  if(total_count == 0) return(list(traditional = NA, standardized = NA))
  
  blended_percents <- 100 * blended_counts / total_count
  
  # Calculate absolute deviations from 10%
  deviations <- abs(blended_percents - 10)
  
  # Traditional Myers Index is half the sum of absolute deviations
  traditional_myers <- sum(deviations) / 2
  
  # Standardized Myers (0-1 scale)
  standardized_myers <- traditional_myers / 90
  
  return(list(traditional = traditional_myers, standardized = standardized_myers))
}

########################################
# BENFORD'S LAW ANALYSIS
########################################

calculate_benford_metrics <- function(wage_data) {
  # Extract first digit for Benford analysis
  first_digits <- as.numeric(substr(as.character(abs(wage_data)), 1, 1))
  first_digits <- first_digits[first_digits %in% 1:9]
  
  if(length(first_digits) < 100) {
    return(list(abs_distance = NA, chi_square = NA))
  }
  
  # Calculate observed frequencies
  obs_table <- table(factor(first_digits, levels = 1:9))
  obs_freq <- as.numeric(obs_table) / sum(obs_table)
  
  # Calculate metrics
  benford_abs_distance <- sum(abs(obs_freq - benford_expected))
  benford_chi_square <- sum((obs_freq - benford_expected)^2 / benford_expected)
  
  return(list(abs_distance = benford_abs_distance, chi_square = benford_chi_square))
}

########################################
# CONTROL VARIABLES CALCULATION
########################################

calculate_control_variables <- function(data) {
  # Initialize with NA values
  urban_share <- NA
  male_share <- NA
  literacy_share <- NA
  unemployment_share <- NA
  
  # Urban share
  if("urban" %in% names(data) && sum(!is.na(data$urban)) > 0) {
    urban_share <- mean(data$urban == 1, na.rm = TRUE)
  }
  
  # Male share
  if("male" %in% names(data) && sum(!is.na(data$male)) > 0) {
    male_share <- mean(data$male == 1, na.rm = TRUE)
  }
  
  # Literacy share
  if("literacy" %in% names(data) && sum(!is.na(data$literacy)) > 0) {
    literacy_share <- mean(data$literacy == 1, na.rm = TRUE)
  }
  
  # Unemployment share (unemployed / (employed + unemployed))
  if("lstatus" %in% names(data) && sum(!is.na(data$lstatus)) > 0) {
    employed_unemployed <- data$lstatus %in% c(1, 2)
    if(sum(employed_unemployed, na.rm = TRUE) > 0) {
      unemployment_share <- sum(data$lstatus == 2, na.rm = TRUE) / 
                           sum(employed_unemployed, na.rm = TRUE)
    }
  }
  
  return(list(urban_share = urban_share,
              male_share = male_share,
              literacy_share = literacy_share,
              unemployment_share = unemployment_share))
}

########################################
# OUTLIER REMOVAL FUNCTIONS
########################################

remove_wage_outliers <- function(wage_data) {
  # Remove zeros and negatives
  clean_wages <- wage_data[wage_data > 0]
  
  # Calculate IQR bounds
  q1 <- quantile(clean_wages, 0.25, na.rm = TRUE)
  q3 <- quantile(clean_wages, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  
  # Define bounds (Q1 - 1.5*IQR, Q3 + 1.5*IQR) - standard outlier detection
  lower_bound <- q1 - 1.5 * iqr_val
  upper_bound <- q3 + 1.5 * iqr_val
  
  # Filter outliers
  filtered_wages <- clean_wages[clean_wages >= lower_bound & clean_wages <= upper_bound]
  
  return(filtered_wages)
}

########################################
# RESIDUALIZATION FUNCTION
########################################

residualize_data <- function(data, outcome_var) {
  # Ensure data is a data frame
  data <- as.data.frame(data)
  
  # Check available dimensions
  n_countries <- length(unique(data$countrycode))
  n_years <- length(unique(data$year))
  
  # Create appropriate formula
  if (n_countries > 1 && n_years > 1) {
    formula_str <- paste(outcome_var, "~ factor(countrycode) + factor(year)")
  } else if (n_countries == 1 && n_years > 1) {
    formula_str <- paste(outcome_var, "~ factor(year)")
  } else if (n_countries > 1 && n_years == 1) {
    formula_str <- paste(outcome_var, "~ factor(countrycode)")
  } else {
    # Center data instead
    return(data[[outcome_var]] - mean(data[[outcome_var]], na.rm = TRUE))
  }
  
  # Fit model and extract residuals
  model_formula <- as.formula(formula_str)
  model <- lm(model_formula, data = data)
  
  return(residuals(model))
}

########################################
# PROCESSING VERSIONS
########################################

process_version0 <- function(results) {
  # Version 0: Raw data (no adjustments)
  return(results)
}

process_version1 <- function(results) {
  # Version 1: Remove extreme Myers values only
  results_v1 <- results
  
  # Calculate outlier threshold using IQR method for Myers
  q1 <- quantile(results_v1$standardized_myers, 0.25, na.rm = TRUE)
  q3 <- quantile(results_v1$standardized_myers, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  myers_threshold <- q3 + 1.5 * iqr
  
  message("Myers Index outlier threshold: ", round(myers_threshold, 3))
  
  # Apply filter
  original_count <- nrow(results_v1)
  results_v1 <- results_v1[results_v1$standardized_myers <= myers_threshold, ]
  removed_count <- original_count - nrow(results_v1)
  
  message(removed_count, " observations removed as Myers Index outliers (", 
          round(removed_count/original_count*100, 1), "% of data)")
  
  return(results_v1)
}

process_version2 <- function(results) {
  # Version 2: Residualize variables only
  results_v2 <- results
  
  # Variables to residualize
  vars_to_residualize <- c("standardized_myers", "benford_abs_distance", "benford_chi_square")
  
  for (var in vars_to_residualize) {
    residual_name <- paste0("residual_", var)
    results_v2[[residual_name]] <- residualize_data(results_v2, var)
    message("Residualized variable: ", var)
  }
  
  return(results_v2)
}

process_version3 <- function(results) {
  # Version 3: Both remove outliers and residualize
  results_v3 <- process_version1(results)
  results_v3 <- process_version2(results_v3)
  return(results_v3)
}

########################################
# PLOTTING FUNCTIONS
########################################

create_scatter_plot <- function(data, x_var, y_var, y_label, version_num, 
                                weighted = FALSE, for_checks = FALSE) {
  
  # Get unique countries for color coding
  countries <- unique(data$countrycode)
  n_countries <- length(countries)
  
  # Generate color palette
  if (n_countries <= 8) {
    country_colors <- brewer.pal(max(8, n_countries), "Set1")[1:n_countries]
  } else {
    country_colors <- colorRampPalette(brewer.pal(9, "Set1"))(n_countries)
  }
  names(country_colors) <- countries
  
  # Create labels
  if (grepl("residual_", x_var)) {
    x_label <- "Standardized Myers Index (Residualized)"
  } else {
    x_label <- "Standardized Myers Index (0-1)"
  }
  
  if (grepl("residual_", y_var)) {
    y_label <- paste(y_label, "(Residualized)")
  }
  
  # Create title
  version_names <- c("V00" = "Raw Data", "V01" = "No Extreme Myers", 
                     "V02" = "Residualized", "V03" = "No Extreme + Residualized")
  title <- paste0(version_names[paste0("V0", version_num)], ": Myers Index vs ", y_label)
  
  # Create plot
  x_var_sym <- sym(x_var)
  y_var_sym <- sym(y_var)
  
  p <- ggplot(data, aes(x = !!x_var_sym, y = !!y_var_sym)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "black"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.title = element_text(size = 10, face = "bold"),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(x = x_label, y = y_label, title = title, color = "Country")
  
  # Add points
  if (weighted && "n_observations" %in% names(data)) {
    p <- p + geom_point(aes(color = countrycode, size = n_observations), alpha = 0.8)
    p <- p + labs(size = "N observations")
  } else {
    p <- p + geom_point(aes(color = countrycode), alpha = 0.8)
  }
  
  # Apply color scale
  p <- p + scale_color_manual(values = country_colors)
  
  # Add regression lines
  p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, 
                       linetype = "dashed", color = "blue")
  p <- p + geom_smooth(method = "loess", formula = y ~ x, se = FALSE, 
                       linetype = "dashed", color = "green", span = 0.75)
  
  # Add correlation
  corr_val <- cor(data[[x_var]], data[[y_var]], use = "pairwise.complete.obs")
  if (!is.na(corr_val)) {
    p <- p + annotate("text", 
                      x = max(data[[x_var]], na.rm = TRUE) * 0.95,
                      y = min(data[[y_var]], na.rm = TRUE) * 1.05,
                      label = paste("Correlation:", round(corr_val, 3)),
                      hjust = 1, fontface = "bold")
  }
  
  return(p)
}

########################################
# REGRESSION ANALYSIS AND TABLE FORMATTING
########################################

# Function to add significance stars
add_significance_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value <= 0.01) return("***")
  if (p_value <= 0.05) return("**")
  if (p_value <= 0.10) return("*")
  return("")
}

# Function to format regression table
format_regression_table <- function(reg_results, version_name) {
  if (nrow(reg_results) == 0 || "Model" %in% reg_results && reg_results$Model[1] == "Insufficient data") {
    return("Insufficient data for regression analysis")
  }
  
  # Separate results by fixed effects
  results_no_fe <- reg_results[reg_results$Fixed_Effects == "No", ]
  results_fe <- reg_results[reg_results$Fixed_Effects == "Yes", ]
  
  # Get unique models and terms
  models <- unique(reg_results$Model)
  
  # Create formatted table
  table_text <- paste0("Regression Results for ", version_name, "\n")
  table_text <- paste0(table_text, "Dependent Variable: Benford Absolute Distance\n")
  table_text <- paste0(table_text, "=", paste(rep("=", 80), collapse=""), "\n\n")
  
  # Without Fixed Effects
  if (nrow(results_no_fe) > 0) {
    table_text <- paste0(table_text, "Panel A: Without Fixed Effects\n")
    table_text <- paste0(table_text, paste(rep("-", 50), collapse=""), "\n")
    
    # Create header
    header <- sprintf("%-25s", "Variable")
    for (model in models) {
      header <- paste0(header, sprintf("%15s", gsub("Model \\d+: ", "", model)))
    }
    table_text <- paste0(table_text, header, "\n")
    table_text <- paste0(table_text, paste(rep("-", 50), collapse=""), "\n")
    
    # Get unique terms (excluding intercept and fixed effects)
    terms <- unique(results_no_fe$term[!grepl("Intercept|factor\\(", results_no_fe$term)])
    
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
    
    # Add R-squared and observations
    table_text <- paste0(table_text, paste(rep("-", 50), collapse=""), "\n")
    
    # Observations row
    obs_row <- sprintf("%-25s", "Observations")
    for (model in models) {
      # This is a simplification - in practice you'd extract this from model objects
      obs_row <- paste0(obs_row, sprintf("%15s", "N/A"))
    }
    table_text <- paste0(table_text, obs_row, "\n\n")
  }
  
  # With Fixed Effects
  if (nrow(results_fe) > 0) {
    table_text <- paste0(table_text, "Panel B: With Country and Year Fixed Effects\n")
    table_text <- paste0(table_text, paste(rep("-", 50), collapse=""), "\n")
    
    # Create header
    header <- sprintf("%-25s", "Variable")
    for (model in models) {
      header <- paste0(header, sprintf("%15s", gsub("Model \\d+: ", "", model)))
    }
    table_text <- paste0(table_text, header, "\n")
    table_text <- paste0(table_text, paste(rep("-", 50), collapse=""), "\n")
    
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
    
    table_text <- paste0(table_text, paste(rep("-", 50), collapse=""), "\n")
    table_text <- paste0(table_text, sprintf("%-25s", "Country FE"), sprintf("%15s", "Yes"), "\n")
    table_text <- paste0(table_text, sprintf("%-25s", "Year FE"), sprintf("%15s", "Yes"), "\n")
  }
  
  # Add notes
  table_text <- paste0(table_text, "\n")
  table_text <- paste0(table_text, "Notes: Standard errors in parentheses\n")
  table_text <- paste0(table_text, "*** p<0.01, ** p<0.05, * p<0.10\n")
  
  return(table_text)
}

run_regression_analysis <- function(data, version_num) {
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
  
  if (nrow(reg_data) < 10) {
    return(data.frame(Model = "Insufficient data", stringsAsFactors = FALSE))
  }
  
  # Define regression specifications
  reg_specs <- list(
    "Model 1: Simple" = paste(benford_var, "~", myers_var),
    "Model 2: + Urban" = paste(benford_var, "~", myers_var, "+ urban_share"),
    "Model 3: + Male" = paste(benford_var, "~", myers_var, "+ urban_share + male_share"),
    "Model 4: + Literacy" = paste(benford_var, "~", myers_var, "+ urban_share + male_share + literacy_share"),
    "Model 5: + Unemployment" = paste(benford_var, "~", myers_var, "+ urban_share + male_share + literacy_share + unemployment_share")
  )
  
  # Run regressions without fixed effects
  results_no_fe <- data.frame()
  for (i in seq_along(reg_specs)) {
    model_name <- names(reg_specs)[i]
    formula_str <- reg_specs[[i]]
    
    tryCatch({
      model <- lm(as.formula(formula_str), data = reg_data)
      model_summary <- tidy(model)
      model_summary$Model <- model_name
      model_summary$Fixed_Effects <- "No"
      results_no_fe <- rbind(results_no_fe, model_summary)
    }, error = function(e) {
      message("Error in regression ", model_name, ": ", e$message)
    })
  }
  
  # Run regressions with fixed effects (if applicable)
  results_fe <- data.frame()
  if (length(unique(reg_data$countrycode)) > 1 && length(unique(reg_data$year)) > 1) {
    for (i in seq_along(reg_specs)) {
      model_name <- names(reg_specs)[i]
      formula_str <- paste(reg_specs[[i]], "+ factor(countrycode) + factor(year)")
      
      tryCatch({
        model <- lm(as.formula(formula_str), data = reg_data)
        model_summary <- tidy(model)
        model_summary$Model <- model_name
        model_summary$Fixed_Effects <- "Yes"
        results_fe <- rbind(results_fe, model_summary)
      }, error = function(e) {
        message("Error in regression with FE ", model_name, ": ", e$message)
      })
    }
  }
  
  # Combine results
  all_results <- rbind(results_no_fe, results_fe)
  
  return(all_results)
}

########################################
# MAIN PROCESSING LOOP
########################################

# Get list of files
if (TEST_MODE) {
  message("RUNNING IN TEST MODE - Processing only ARM and BOL files")
  all_files <- list.files(path = input_path, 
                          pattern = "(ARM|BOL).*\\.dta$", 
                          full.names = TRUE, recursive = TRUE)
} else {
  message("Processing ALL countries")
  all_files <- list.files(path = input_path, 
                          pattern = "\\.dta$", 
                          full.names = TRUE, recursive = TRUE)
}

if(length(all_files) == 0) {
  stop("No .dta files found in input folder")
}

message("Found ", length(all_files), " files to process")

# Process each file
for(file in all_files) {
  message("Processing file: ", basename(file))
  
  tryCatch({
    # Load data
    dat <- read_dta(file) %>% as.data.table()
    
    # Check required variables
    required_vars <- c("wage_no_compen", "unitwage", "year", "countrycode", "age")
    if(!all(required_vars %in% names(dat))) {
      message("  Skipping file due to missing required variables.")
      next
    }
    
    # Filter data
    dat <- dat[!is.na(wage_no_compen) & unitwage == 5 & age >= 10 & age <= 79]
    
    if(nrow(dat) == 0) {
      message("  No observations after filtering.")
      next
    }
    
    # Process by year
    years <- unique(dat$year)
    
    for(yr in years) {
      year_data <- dat[year == yr]
      n_obs <- nrow(year_data)
      
      if(n_obs < 100) {
        message("  Skipping year ", yr, " due to insufficient data (n =", n_obs, ")")
        next
      }
      
      # Remove wage outliers
      clean_wages <- remove_wage_outliers(year_data$wage_no_compen)
      if(length(clean_wages) < 100) {
        message("  Skipping year ", yr, " due to insufficient data after outlier removal")
        next
      }
      
      # Calculate Myers Index
      myers_result <- calculate_traditional_myers(year_data$age)
      
      # Calculate Benford metrics
      benford_result <- calculate_benford_metrics(clean_wages)
      
      # Calculate control variables
      controls <- calculate_control_variables(year_data)
      
      # Get country code
      country <- unique(year_data$countrycode)[1]
      
      # Add to results
      new_row <- data.frame(
        countrycode = country,
        year = yr,
        n_observations = length(clean_wages),
        traditional_myers = myers_result$traditional,
        standardized_myers = myers_result$standardized,
        benford_abs_distance = benford_result$abs_distance,
        benford_chi_square = benford_result$chi_square,
        urban_share = controls$urban_share,
        male_share = controls$male_share,
        literacy_share = controls$literacy_share,
        unemployment_share = controls$unemployment_share,
        stringsAsFactors = FALSE
      )
      
      results <- rbind(results, new_row)
      
      message("  Added year ", yr, " for country ", country, " (n=", length(clean_wages), ")")
    }
    
    # Clear data to save memory
    rm(dat, year_data)
    gc()
    
  }, error = function(e) {
    message("  Error processing file: ", e$message)
  })
}

message("Total observations collected: ", nrow(results))

########################################
# PROCESS ALL VERSIONS AND CREATE OUTPUTS
########################################

if (nrow(results) > 0) {
  
  # Create all versions
  results_v00 <- process_version0(results)
  results_v01 <- process_version1(results)
  results_v02 <- process_version2(results)
  results_v03 <- process_version3(results)
  
  all_versions <- list(
    "V00" = list(data = results_v00, version_num = 0, is_residualized = FALSE),
    "V01" = list(data = results_v01, version_num = 1, is_residualized = FALSE),
    "V02" = list(data = results_v02, version_num = 2, is_residualized = TRUE),
    "V03" = list(data = results_v03, version_num = 3, is_residualized = TRUE)
  )
  
  # Process each version
  for (version_name in names(all_versions)) {
    version_info <- all_versions[[version_name]]
    version_data <- version_info$data
    version_num <- version_info$version_num
    is_residualized <- version_info$is_residualized
    
    message("Processing version ", version_name)
    
    # Determine variables to use
    if (is_residualized) {
      x_var <- "residual_standardized_myers"
      y_vars <- list(
        "mae" = list(var = "residual_benford_abs_distance", label = "Absolute Distance from Benford's Law"),
        "chi_square" = list(var = "residual_benford_chi_square", label = "Chi-Square Statistic")
      )
    } else {
      x_var <- "standardized_myers"
      y_vars <- list(
        "mae" = list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
        "chi_square" = list(var = "benford_chi_square", label = "Chi-Square Statistic")
      )
    }
    
    # Create plots
    for (measure_name in names(y_vars)) {
      y_info <- y_vars[[measure_name]]
      
      # Skip if insufficient data
      if (sum(complete.cases(version_data[, c(x_var, y_info$var)])) < 5) {
        message("  Insufficient data for ", measure_name, " plots")
        next
      }
      
      # Create unweighted plot
      p_unweighted <- create_scatter_plot(
        data = version_data,
        x_var = x_var,
        y_var = y_info$var,
        y_label = y_info$label,
        version_num = version_num,
        weighted = FALSE
      )
      
      # Create weighted plot
      p_weighted <- create_scatter_plot(
        data = version_data,
        x_var = x_var,
        y_var = y_info$var,
        y_label = y_info$label,
        version_num = version_num,
        weighted = TRUE
      )
      
      # Save plots
      unweighted_filename <- paste0("01_Myers_Benford_Ind_", version_name, "_unweighted_", measure_name, ".png")
      weighted_filename <- paste0("01_Myers_Benford_Ind_", version_name, "_weighted_", measure_name, ".png")
      
      if (measure_name == "mae") {
        # Save MAE plots in main folder
        ggsave(file.path(output_path_plots_main, unweighted_filename), 
               p_unweighted, width = 10, height = 7, dpi = 300)
        ggsave(file.path(output_path_plots_main, weighted_filename), 
               p_weighted, width = 10, height = 7, dpi = 300)
      } else {
        # Save other measures in checks folder
        ggsave(file.path(output_path_plots_checks, unweighted_filename), 
               p_unweighted, width = 10, height = 7, dpi = 300)
        ggsave(file.path(output_path_plots_checks, weighted_filename), 
               p_weighted, width = 10, height = 7, dpi = 300)
      }
    }
    
    # Run regression analysis
    reg_results <- run_regression_analysis(version_data, version_num)
    
    # Create formatted regression table
    formatted_table <- format_regression_table(reg_results, version_name)
    
    # Save formatted regression table as text file
    table_filename <- paste0("01_Myers_Benford_Ind_", version_name, ".txt")
    writeLines(formatted_table, file.path(output_path_docs, table_filename))
    
    # Create Excel output
    excel_filename <- paste0("01_Myers_Benford_Ind_", version_name, ".xlsx")
    
    excel_data <- list(
      "Results" = version_data,
      "Regressions" = reg_results
    )
    
    write_xlsx(excel_data, file.path(output_path_excel, excel_filename))
    
    message("  Version ", version_name, " complete - Excel saved, regression table saved, plots generated")
  }
  
  message("All analyses completed successfully!")
  
} else {
  stop("No valid results generated. Check input data and file paths.")
}