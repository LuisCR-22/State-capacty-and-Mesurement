########################################
# Benford Wage Analysis - Household Level Enhanced Version 2.1
########################################
# Author: Luis Castellanos Rodriguez
# Modified: 2025-07-17
# Enhanced with household level Benford analysis and proper Myers/Benford sample definitions

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Test mode: Set to TRUE to process only ARM and BOL files
TEST_MODE <- FALSE  # Change to FALSE to process all countries

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
output_path_plots_main <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/PNG/Myers vs. Bendford/02 Myers vs Benford household level"
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
                      n_observations_myers = integer(),
                      n_households_benford_with_zeros = integer(),
                      n_households_benford_no_zeros = integer(),
                      traditional_myers = numeric(),
                      standardized_myers = numeric(),
                      benford_correlation_with_zeros = numeric(),
                      benford_abs_distance_with_zeros = numeric(),
                      benford_chi_square_with_zeros = numeric(),
                      benford_mae_with_zeros = numeric(),
                      benford_correlation_no_zeros = numeric(),
                      benford_abs_distance_no_zeros = numeric(),
                      benford_chi_square_no_zeros = numeric(),
                      benford_mae_no_zeros = numeric(),
                      kuiper_statistic_with_zeros = numeric(),
                      kuiper_pvalue_with_zeros = numeric(),
                      kuiper_statistic_no_zeros = numeric(),
                      kuiper_pvalue_no_zeros = numeric(),
                      ks_statistic_with_zeros = numeric(),
                      ks_pvalue_with_zeros = numeric(),
                      ks_statistic_no_zeros = numeric(),
                      ks_pvalue_no_zeros = numeric(),
                      urban_share = numeric(),
                      male_share = numeric(),
                      literacy_share = numeric(),
                      unemployment_share = numeric(),
                      stringsAsFactors = FALSE)

########################################
# MYERS INDEX CALCULATION (Age 10-79, ALL PEOPLE)
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
# BENFORD'S LAW ANALYSIS (HOUSEHOLD LEVEL - TWO VERSIONS)
########################################

# Enhanced Kuiper Test function
kuiper_test <- function(obs, expected) {
  obs_cdf <- cumsum(obs) / sum(obs)
  expected_cdf <- cumsum(expected) / sum(expected)
  diff <- obs_cdf - expected_cdf
  
  V <- max(diff) - min(diff)  # Kuiper statistic
  n <- sum(obs)
  
  # Improved p-value calculation for Kuiper test
  # Using asymptotic approximation from Stephens (1970)
  lambda <- (sqrt(n) + 0.155 + 0.24/sqrt(n)) * V
  p_value <- 2 * exp(-2 * lambda^2)
  
  return(list(statistic = V, p.value = p_value))
}

calculate_household_benford_metrics <- function(household_wages, include_zeros = TRUE) {
  # Filter household wages based on include_zeros parameter
  if (include_zeros) {
    # Keep all non-negative household wages
    clean_wages <- household_wages[household_wages >= 0]
    # For zeros, assign first digit as NA (will be excluded from Benford analysis)
    first_digits <- ifelse(clean_wages == 0, NA, as.numeric(substr(as.character(abs(clean_wages)), 1, 1)))
  } else {
    # Remove zeros and negatives
    clean_wages <- household_wages[household_wages > 0]
    # Extract first digit
    first_digits <- as.numeric(substr(as.character(abs(clean_wages)), 1, 1))
  }
  
  # Filter to valid first digits (1-9)
  first_digits <- first_digits[first_digits %in% 1:9]
  
  if(length(first_digits) < 100) {
    return(list(correlation = NA, abs_distance = NA, chi_square = NA, mae = NA,
                kuiper_statistic = NA, kuiper_pvalue = NA, 
                ks_statistic = NA, ks_pvalue = NA, n_households = length(clean_wages)))
  }
  
  # Calculate observed frequencies
  obs_table <- table(factor(first_digits, levels = 1:9))
  obs_freq <- as.numeric(obs_table) / sum(obs_table)
  
  # Calculate metrics
  benford_correlation <- cor(obs_freq, benford_expected)
  benford_abs_distance <- sum(abs(obs_freq - benford_expected))
  benford_chi_square <- sum((obs_freq - benford_expected)^2 / benford_expected)
  benford_mae <- mean(abs(obs_freq - benford_expected))
  
  # Statistical tests
  kuiper_result <- kuiper_test(obs_freq, benford_expected)
  ks_result <- ks.test(obs_freq, benford_expected)
  
  return(list(correlation = benford_correlation,
              abs_distance = benford_abs_distance, 
              chi_square = benford_chi_square,
              mae = benford_mae,
              kuiper_statistic = kuiper_result$statistic,
              kuiper_pvalue = kuiper_result$p.value,
              ks_statistic = ks_result$statistic,
              ks_pvalue = ks_result$p.value,
              n_households = length(clean_wages)))
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
    # Check if we have both employed and unemployed people
    employed_count <- sum(data$lstatus == 1, na.rm = TRUE)
    unemployed_count <- sum(data$lstatus == 2, na.rm = TRUE)
    total_labor_force <- employed_count + unemployed_count
    
    if(total_labor_force > 0) {
      unemployment_share <- unemployed_count / total_labor_force
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

remove_household_wage_outliers <- function(household_wages, include_zeros = TRUE) {
  if (include_zeros) {
    # Keep zeros and positive values
    clean_wages <- household_wages[household_wages >= 0]
  } else {
    # Remove zeros and negatives
    clean_wages <- household_wages[household_wages > 0]
  }
  
  # If we're keeping zeros, only apply IQR to positive values
  if (include_zeros && any(clean_wages > 0)) {
    positive_wages <- clean_wages[clean_wages > 0]
    
    # Calculate IQR bounds on positive wages only
    q1 <- quantile(positive_wages, 0.25, na.rm = TRUE)
    q3 <- quantile(positive_wages, 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1
    
    # Define bounds for positive wages
    lower_bound <- q1 - 1.5 * iqr_val
    upper_bound <- q3 + 1.5 * iqr_val
    
    # Keep zeros + positive wages within bounds
    zeros <- clean_wages[clean_wages == 0]
    filtered_positive <- positive_wages[positive_wages >= lower_bound & positive_wages <= upper_bound]
    filtered_wages <- c(zeros, filtered_positive)
    
  } else if (!include_zeros) {
    # Standard IQR on positive wages only
    q1 <- quantile(clean_wages, 0.25, na.rm = TRUE)
    q3 <- quantile(clean_wages, 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1
    
    # Define bounds
    lower_bound <- q1 - 1.5 * iqr_val
    upper_bound <- q3 + 1.5 * iqr_val
    
    # Filter outliers
    filtered_wages <- clean_wages[clean_wages >= lower_bound & clean_wages <= upper_bound]
  } else {
    # All wages are zero, return as is
    filtered_wages <- clean_wages
  }
  
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
  # Use Benford WITH zeros
  results_v0 <- results
  
  # Create the variables we'll use for this version
  results_v0$benford_correlation <- results_v0$benford_correlation_with_zeros
  results_v0$benford_abs_distance <- results_v0$benford_abs_distance_with_zeros
  results_v0$benford_chi_square <- results_v0$benford_chi_square_with_zeros
  results_v0$benford_mae <- results_v0$benford_mae_with_zeros
  results_v0$kuiper_statistic <- results_v0$kuiper_statistic_with_zeros
  results_v0$kuiper_pvalue <- results_v0$kuiper_pvalue_with_zeros
  results_v0$ks_statistic <- results_v0$ks_statistic_with_zeros
  results_v0$ks_pvalue <- results_v0$ks_pvalue_with_zeros
  results_v0$n_households_benford <- results_v0$n_households_benford_with_zeros
  
  return(results_v0)
}

process_version1 <- function(results) {
  # Version 1: Remove extreme Myers values only
  # Use Benford WITHOUT zeros (since we're removing outliers)
  results_v1 <- results
  
  # Use Benford metrics without zeros
  results_v1$benford_correlation <- results_v1$benford_correlation_no_zeros
  results_v1$benford_abs_distance <- results_v1$benford_abs_distance_no_zeros
  results_v1$benford_chi_square <- results_v1$benford_chi_square_no_zeros
  results_v1$benford_mae <- results_v1$benford_mae_no_zeros
  results_v1$kuiper_statistic <- results_v1$kuiper_statistic_no_zeros
  results_v1$kuiper_pvalue <- results_v1$kuiper_pvalue_no_zeros
  results_v1$ks_statistic <- results_v1$ks_statistic_no_zeros
  results_v1$ks_pvalue <- results_v1$ks_pvalue_no_zeros
  results_v1$n_households_benford <- results_v1$n_households_benford_no_zeros
  
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
  # Use Benford WITH zeros (no outlier removal)
  results_v2 <- results
  
  # Use Benford metrics with zeros
  results_v2$benford_correlation <- results_v2$benford_correlation_with_zeros
  results_v2$benford_abs_distance <- results_v2$benford_abs_distance_with_zeros
  results_v2$benford_chi_square <- results_v2$benford_chi_square_with_zeros
  results_v2$benford_mae <- results_v2$benford_mae_with_zeros
  results_v2$kuiper_statistic <- results_v2$kuiper_statistic_with_zeros
  results_v2$kuiper_pvalue <- results_v2$kuiper_pvalue_with_zeros
  results_v2$ks_statistic <- results_v2$ks_statistic_with_zeros
  results_v2$ks_pvalue <- results_v2$ks_pvalue_with_zeros
  results_v2$n_households_benford <- results_v2$n_households_benford_with_zeros
  
  # Variables to residualize
  vars_to_residualize <- c("standardized_myers", "benford_correlation", 
                           "benford_abs_distance", "benford_chi_square", "benford_mae")
  
  for (var in vars_to_residualize) {
    residual_name <- paste0("residual_", var)
    results_v2[[residual_name]] <- residualize_data(results_v2, var)
    message("Residualized variable: ", var)
  }
  
  return(results_v2)
}

process_version3 <- function(results) {
  # Version 3: Both remove outliers and residualize
  # Use Benford WITHOUT zeros (since we're removing outliers)
  results_v3 <- results
  
  # Use Benford metrics without zeros
  results_v3$benford_correlation <- results_v3$benford_correlation_no_zeros
  results_v3$benford_abs_distance <- results_v3$benford_abs_distance_no_zeros
  results_v3$benford_chi_square <- results_v3$benford_chi_square_no_zeros
  results_v3$benford_mae <- results_v3$benford_mae_no_zeros
  results_v3$kuiper_statistic <- results_v3$kuiper_statistic_no_zeros
  results_v3$kuiper_pvalue <- results_v3$kuiper_pvalue_no_zeros
  results_v3$ks_statistic <- results_v3$ks_statistic_no_zeros
  results_v3$ks_pvalue <- results_v3$ks_pvalue_no_zeros
  results_v3$n_households_benford <- results_v3$n_households_benford_no_zeros
  
  # First remove outliers
  q1 <- quantile(results_v3$standardized_myers, 0.25, na.rm = TRUE)
  q3 <- quantile(results_v3$standardized_myers, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  myers_threshold <- q3 + 1.5 * iqr
  
  message("Myers Index outlier threshold: ", round(myers_threshold, 3))
  
  original_count <- nrow(results_v3)
  results_v3 <- results_v3[results_v3$standardized_myers <= myers_threshold, ]
  removed_count <- original_count - nrow(results_v3)
  
  message(removed_count, " observations removed as Myers Index outliers (", 
          round(removed_count/original_count*100, 1), "% of data)")
  
  # Then residualize
  vars_to_residualize <- c("standardized_myers", "benford_correlation", 
                           "benford_abs_distance", "benford_chi_square", "benford_mae")
  
  for (var in vars_to_residualize) {
    residual_name <- paste0("residual_", var)
    results_v3[[residual_name]] <- residualize_data(results_v3, var)
    message("Residualized variable: ", var)
  }
  
  return(results_v3)
}

########################################
# ENHANCED PLOTTING FUNCTIONS (Code 3 Style)
########################################

create_enhanced_scatter_plot <- function(data, x_var, y_var, y_label, version_num, 
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
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(x = x_label, y = y_label, color = "Country")
  
  # Add points
  if (weighted && "n_households_benford" %in% names(data)) {
    p <- p + geom_point(aes(color = countrycode, size = n_households_benford), alpha = 0.8)
    p <- p + labs(size = "N households")
  } else {
    p <- p + geom_point(aes(color = countrycode), alpha = 0.8)
  }
  
  # Apply color scale
  p <- p + scale_color_manual(values = country_colors)
  
  # Extend y-axis limits to accommodate bottom annotations
  y_range <- max(data[[y_var]], na.rm = TRUE) - min(data[[y_var]], na.rm = TRUE)
  y_min_extended <- min(data[[y_var]], na.rm = TRUE) - y_range * 0.2
  p <- p + coord_cartesian(ylim = c(y_min_extended, max(data[[y_var]], na.rm = TRUE) + y_range * 0.05))
  
  # Enhanced: Add regression lines WITH confidence intervals and improved colors
  # Linear regression: black solid line with gray confidence intervals
  p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
                       linetype = "solid", color = "black", fill = "gray70", alpha = 0.4)
  # Loess regression: darker green dashed line with more transparent green intervals
  p <- p + geom_smooth(method = "loess", formula = y ~ x, se = TRUE, 
                       linetype = "dashed", color = "darkgreen", fill = "darkgreen", alpha = 0.10, span = 0.75)
  
  # Add red horizontal reference line at y=0
  p <- p + geom_hline(yintercept = 0, color = "darkred", linetype = "solid", linewidth = 0.5)
  
  # Add correlation and significance test with improved annotation style
  # Perform correlation test
  cor_test <- cor.test(data[[x_var]], data[[y_var]], use = "pairwise.complete.obs")
  corr_val <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create significance annotation matching the example image style
  if (!is.na(corr_val) && !is.na(p_value)) {
    # Add significance stars
    stars <- ""
    if (p_value <= 0.01) stars <- "***"
    else if (p_value <= 0.05) stars <- "**"
    else if (p_value <= 0.10) stars <- "*"
    
    # Position annotations at bottom like in the example image
    x_range <- max(data[[x_var]], na.rm = TRUE) - min(data[[x_var]], na.rm = TRUE)
    y_range <- max(data[[y_var]], na.rm = TRUE) - min(data[[y_var]], na.rm = TRUE)
    x_center <- (max(data[[x_var]], na.rm = TRUE) + min(data[[x_var]], na.rm = TRUE)) / 2
    y_bottom <- min(data[[y_var]], na.rm = TRUE) - y_range * 0.15
    
    # Add concise line description
    p <- p + annotate("text", 
                      x = x_center,
                      y = y_bottom,
                      label = "Black line: Linear regression. Green line: LOWESS smoothing.",
                      hjust = 0.5, fontface = "plain", size = 3.2)
    
    # Add correlation info
    correlation_text <- paste0("Correlation: ", round(corr_val, 3), stars)
    p <- p + annotate("text", 
                      x = max(data[[x_var]], na.rm = TRUE) * 0.95,
                      y = y_bottom + y_range * 0.05,
                      label = correlation_text,
                      hjust = 1, fontface = "bold", size = 3.5)
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

# Function to format regression table (for text output)
format_regression_table <- function(reg_results, version_name) {
  # Handle new structure
  if (is.list(reg_results) && "coefficients" %in% names(reg_results)) {
    coefficients <- reg_results$coefficients
    model_stats <- reg_results$model_stats
  } else {
    # Backward compatibility
    coefficients <- reg_results
    model_stats <- NULL
  }
  
  if (nrow(coefficients) == 0 || "Model" %in% coefficients && coefficients$Model[1] == "Insufficient data") {
    return("Insufficient data for regression analysis")
  }
  
  # Separate results by fixed effects
  results_no_fe <- coefficients[coefficients$Fixed_Effects == "No", ]
  results_fe <- coefficients[coefficients$Fixed_Effects == "Yes", ]
  
  # Get model stats by fixed effects
  if (!is.null(model_stats)) {
    stats_no_fe <- model_stats[model_stats$Fixed_Effects == "No", ]
    stats_fe <- model_stats[model_stats$Fixed_Effects == "Yes", ]
  } else {
    stats_no_fe <- NULL
    stats_fe <- NULL
  }
  
  # Get unique models and terms
  models <- unique(coefficients$Model)
  
  # Create formatted table
  table_text <- paste0("Regression Results for ", version_name, "\n")
  table_text <- paste0(table_text, "Dependent Variable: Benford Absolute Distance (Household Level)\n")
  table_text <- paste0(table_text, "=", paste(rep("=", 80), collapse=""), "\n\n")
  
  # Without Fixed Effects
  if (nrow(results_no_fe) > 0) {
    table_text <- paste0(table_text, "Panel A: Without Fixed Effects\n")
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
    # Create header
    header <- sprintf("%-25s", "Variable")
    for (model in models) {
      header <- paste0(header, sprintf("%15s", gsub("Model \\d+: ", "", model)))
    }
    table_text <- paste0(table_text, header, "\n")
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
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
    
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
    # Add model statistics
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
  
  # With Fixed Effects
  if (nrow(results_fe) > 0) {
    table_text <- paste0(table_text, "Panel B: With Country and Year Fixed Effects\n")
    table_text <- paste0(table_text, paste(rep("-", 80), collapse=""), "\n")
    
    # Create header
    header <- sprintf("%-25s", "Variable")
    for (model in models) {
      header <- paste0(header, sprintf("%15s", gsub("Model \\d+: ", "", model)))
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
    table_text <- paste0(table_text, sprintf("%-25s", "Country FE"), sprintf("%15s", "Yes"), "\n")
    table_text <- paste0(table_text, sprintf("%-25s", "Year FE"), sprintf("%15s", "Yes"), "\n")
    
    # Add model statistics for FE models
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
  table_text <- paste0(table_text, "Benford analysis conducted on household-level aggregated wages\n")
  table_text <- paste0(table_text, "Myers Index calculated on all individuals aged 10-79\n")
  
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
    return(list(
      coefficients = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE),
      model_stats = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE)
    ))
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
  model_stats_no_fe <- data.frame()
  
  for (i in seq_along(reg_specs)) {
    model_name <- names(reg_specs)[i]
    formula_str <- reg_specs[[i]]
    
    tryCatch({
      model <- lm(as.formula(formula_str), data = reg_data)
      
      # Get coefficient information
      model_summary <- tidy(model)
      model_summary$Model <- model_name
      model_summary$Fixed_Effects <- "No"
      results_no_fe <- rbind(results_no_fe, model_summary)
      
      # Get model statistics
      model_glance <- glance(model)
      model_glance$Model <- model_name
      model_glance$Fixed_Effects <- "No"
      model_stats_no_fe <- rbind(model_stats_no_fe, model_glance)
      
    }, error = function(e) {
      message("Error in regression ", model_name, ": ", e$message)
    })
  }
  
  # Run regressions with fixed effects (if applicable)
  results_fe <- data.frame()
  model_stats_fe <- data.frame()
  
  if (length(unique(reg_data$countrycode)) > 1 && length(unique(reg_data$year)) > 1) {
    for (i in seq_along(reg_specs)) {
      model_name <- names(reg_specs)[i]
      formula_str <- paste(reg_specs[[i]], "+ factor(countrycode) + factor(year)")
      
      tryCatch({
        model <- lm(as.formula(formula_str), data = reg_data)
        
        # Get coefficient information
        model_summary <- tidy(model)
        model_summary$Model <- model_name
        model_summary$Fixed_Effects <- "Yes"
        results_fe <- rbind(results_fe, model_summary)
        
        # Get model statistics
        model_glance <- glance(model)
        model_glance$Model <- model_name
        model_glance$Fixed_Effects <- "Yes"
        model_stats_fe <- rbind(model_stats_fe, model_glance)
        
      }, error = function(e) {
        message("Error in regression with FE ", model_name, ": ", e$message)
      })
    }
  }
  
  # Combine results
  all_coefficients <- rbind(results_no_fe, results_fe)
  all_model_stats <- rbind(model_stats_no_fe, model_stats_fe)
  
  return(list(
    coefficients = all_coefficients,
    model_stats = all_model_stats
  ))
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
    required_vars <- c("wage_no_compen", "unitwage", "year", "countrycode", "age", "hhid")
    if(!all(required_vars %in% names(dat))) {
      message("  Skipping file due to missing required variables.")
      next
    }
    
    # Filter data for age range (keep all employment statuses for control variables and Myers)
    dat <- dat[age >= 10 & age <= 79]
    
    if(nrow(dat) == 0) {
      message("  No observations after age filtering.")
      next
    }
    
    # Process by year
    years <- unique(dat$year)
    
    for(yr in years) {
      year_data_full <- dat[year == yr]  # Keep full data for control variables and Myers
      
      # Calculate Myers Index on ALL people in age range (traditional approach)
      myers_result <- calculate_traditional_myers(year_data_full$age)
      n_myers <- sum(year_data_full$age >= 10 & year_data_full$age <= 79, na.rm = TRUE)
      
      # Filter for household wage analysis (monthly wages, non-missing wage data)
      year_data_wages <- year_data_full[!is.na(wage_no_compen) & unitwage == 5]
      
      if(nrow(year_data_wages) < 100) {
        message("  Skipping year ", yr, " due to insufficient wage data (n =", nrow(year_data_wages), ")")
        next
      }
      
      # Aggregate wages at household level
      hh_wages <- year_data_wages[, .(total_hh_wage = sum(wage_no_compen, na.rm = TRUE)), by = .(hhid)]
      
      if(nrow(hh_wages) < 100) {
        message("  Skipping year ", yr, " due to insufficient households (n =", nrow(hh_wages), ")")
        next
      }
      
      # Calculate Benford metrics - TWO VERSIONS
      # Version 1: Include zeros (household wages can be zero)
      hh_wages_with_zeros <- remove_household_wage_outliers(hh_wages$total_hh_wage, include_zeros = TRUE)
      benford_with_zeros <- calculate_household_benford_metrics(hh_wages_with_zeros, include_zeros = TRUE)
      
      # Version 2: Exclude zeros  
      hh_wages_no_zeros <- remove_household_wage_outliers(hh_wages$total_hh_wage, include_zeros = FALSE)
      benford_no_zeros <- calculate_household_benford_metrics(hh_wages_no_zeros, include_zeros = FALSE)
      
      # Calculate control variables on FULL dataset (includes unemployed people)
      controls <- calculate_control_variables(year_data_full)
      
      # Debug: Check unemployment calculation
      if(!is.na(controls$unemployment_share)) {
        employed_count <- sum(year_data_full$lstatus == 1, na.rm = TRUE)
        unemployed_count <- sum(year_data_full$lstatus == 2, na.rm = TRUE)
        message("    Year ", yr, ": Employed=", employed_count, ", Unemployed=", unemployed_count, 
                ", Unemployment rate=", round(controls$unemployment_share*100, 2), "%")
      }
      
      # Get country code
      country <- unique(year_data_wages$countrycode)[1]
      
      # Add to results
      new_row <- data.frame(
        countrycode = country,
        year = yr,
        n_observations_myers = n_myers,
        n_households_benford_with_zeros = benford_with_zeros$n_households,
        n_households_benford_no_zeros = benford_no_zeros$n_households,
        traditional_myers = myers_result$traditional,
        standardized_myers = myers_result$standardized,
        benford_correlation_with_zeros = benford_with_zeros$correlation,
        benford_abs_distance_with_zeros = benford_with_zeros$abs_distance,
        benford_chi_square_with_zeros = benford_with_zeros$chi_square,
        benford_mae_with_zeros = benford_with_zeros$mae,
        benford_correlation_no_zeros = benford_no_zeros$correlation,
        benford_abs_distance_no_zeros = benford_no_zeros$abs_distance,
        benford_chi_square_no_zeros = benford_no_zeros$chi_square,
        benford_mae_no_zeros = benford_no_zeros$mae,
        kuiper_statistic_with_zeros = benford_with_zeros$kuiper_statistic,
        kuiper_pvalue_with_zeros = benford_with_zeros$kuiper_pvalue,
        kuiper_statistic_no_zeros = benford_no_zeros$kuiper_statistic,
        kuiper_pvalue_no_zeros = benford_no_zeros$kuiper_pvalue,
        ks_statistic_with_zeros = benford_with_zeros$ks_statistic,
        ks_pvalue_with_zeros = benford_with_zeros$ks_pvalue,
        ks_statistic_no_zeros = benford_no_zeros$ks_statistic,
        ks_pvalue_no_zeros = benford_no_zeros$ks_pvalue,
        urban_share = controls$urban_share,
        male_share = controls$male_share,
        literacy_share = controls$literacy_share,
        unemployment_share = controls$unemployment_share,
        stringsAsFactors = FALSE
      )
      
      results <- rbind(results, new_row)
      
      message("  Added year ", yr, " for country ", country, 
              " (Myers n=", n_myers, ", HH w/zeros n=", benford_with_zeros$n_households, 
              ", HH no zeros n=", benford_no_zeros$n_households, ")")
    }
    
    # Clear data to save memory
    rm(dat, year_data_full, year_data_wages)
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
        "chi_square" = list(var = "residual_benford_chi_square", label = "Chi-Square Statistic"),
        "correlation" = list(var = "residual_benford_correlation", label = "Correlation with Benford's Law"),
        "benford_mae" = list(var = "residual_benford_mae", label = "Mean Absolute Error")
      )
    } else {
      x_var <- "standardized_myers"
      y_vars <- list(
        "mae" = list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
        "chi_square" = list(var = "benford_chi_square", label = "Chi-Square Statistic"),
        "correlation" = list(var = "benford_correlation", label = "Correlation with Benford's Law"),
        "benford_mae" = list(var = "benford_mae", label = "Mean Absolute Error")
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
      p_unweighted <- create_enhanced_scatter_plot(
        data = version_data,
        x_var = x_var,
        y_var = y_info$var,
        y_label = y_info$label,
        version_num = version_num,
        weighted = FALSE
      )
      
      # Create weighted plot
      p_weighted <- create_enhanced_scatter_plot(
        data = version_data,
        x_var = x_var,
        y_var = y_info$var,
        y_label = y_info$label,
        version_num = version_num,
        weighted = TRUE
      )
      
      # Save plots
      unweighted_filename <- paste0("02_Myers_Benford_HH_", version_name, "_unweighted_", measure_name, ".png")
      weighted_filename <- paste0("02_Myers_Benford_HH_", version_name, "_weighted_", measure_name, ".png")
      
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
    
    # Create formatted regression table (text version)
    formatted_table <- format_regression_table(reg_results, version_name)
    
    # Save formatted regression table as text file
    table_filename_txt <- paste0("02_Myers_Benford_HH_", version_name, ".txt")
    writeLines(formatted_table, file.path(output_path_docs, table_filename_txt))
    
    # Create Excel output
    excel_filename <- paste0("02_Myers_Benford_HH_", version_name, ".xlsx")
    
    # Handle new regression results format for Excel
    if (is.list(reg_results) && "coefficients" %in% names(reg_results)) {
      excel_data <- list(
        "Results" = version_data,
        "Regression_Coefficients" = reg_results$coefficients,
        "Model_Statistics" = reg_results$model_stats
      )
    } else {
      # Backward compatibility
      excel_data <- list(
        "Results" = version_data,
        "Regressions" = reg_results
      )
    }
    
    write_xlsx(excel_data, file.path(output_path_excel, excel_filename))
    
    message("  Version ", version_name, " complete - Excel saved, regression table (.txt) saved, plots generated")
  }
  
  message("All household-level analyses completed successfully!")
  message("Files saved with naming convention: 02_Myers_Benford_HH_V0X.*")
  message("Excel files: ", output_path_excel)
  message("Text files: ", output_path_docs)
  message("PNG files: ", output_path_plots_main)
  
} else {
  stop("No valid results generated. Check input data and file paths.")
}