########################################
# Colombia Myers-Benford Analysis - Regional Level
########################################
# Author: Luis Castellanos Rodriguez
# Modified: 2025-07-20
# Based on "04_Myers_Bendford_regressions_plots-V4"
# Enhanced for Colombia regional analysis using SEDLAC data

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Test mode: Set to TRUE to process only 2018 file
TEST_MODE <- FALSE  # Change to FALSE to process all years (2008-2024)

# Install and load required packages
packages <- c("haven", "writexl", "dplyr", "data.table", "ggplot2", "tidyverse", 
              "ggrepel", "gridExtra", "RColorBrewer", "broom", "stringr")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input path for SEDLAC data
input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/Sedlac"

# Output paths for Colombia
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_path_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel/Col"
output_path_docs <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Docs/Col"
output_path_plots_main <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/PNG/Myers vs. Bendford/01 Myers vs Benford individual level/Col"
output_path_plots_checks <- file.path(output_path_plots_main, "Checks")

# Create directories if they don't exist
dir.create(output_path_excel, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_docs, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots_main, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots_checks, showWarnings = FALSE, recursive = TRUE)

########################################
# REGIONAL MAPPING FUNCTIONS
########################################

# Function to extract region number and create corrected names
create_region_mapping <- function() {
  # Region mapping based on the provided image and table
  region_mapping <- data.frame(
    region_number = c(5, 8, 11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73, 76),
    region_corrected = c("Antioquia", "Atlántico", "Bogotá D.C.", "Bolívar", "Boyacá", "Caldas", 
                         "Caquetá", "Cauca", "Cesar", "Córdoba", "Cundinamarca", "Chocó", 
                         "Huila", "La Guajira", "Magdalena", "Meta", "Nariño", "Norte de Santander", 
                         "Quindío", "Risaralda", "Santander", "Sucre", "Tolima", "Valle"),
    stringsAsFactors = FALSE
  )
  return(region_mapping)
}

# Function to extract region number from region_est2 string
extract_region_info <- function(data) {
  # Extract region number (first digits before the dash or space)
  # More flexible regex to handle different formats
  data$region_number <- as.numeric(str_extract(data$region_est2, "^\\d+"))
  
  # Get region mapping
  region_mapping <- create_region_mapping()
  
  # Merge to get corrected region names
  data <- merge(data, region_mapping, by = "region_number", all.x = TRUE)
  
  # Handle any unmapped regions
  data$region_corrected[is.na(data$region_corrected)] <- "Unknown"
  
  return(data)
}

########################################
# STATISTICAL CONSTANTS AND FUNCTIONS
########################################

# Define the Benford expected probabilities for digits 1 to 9
benford_expected <- log10(1 + 1/(1:9))

########################################
# MYERS INDEX CALCULATION (adapted for Colombian data)
########################################

calculate_traditional_myers <- function(age_data) {
  # Ensure ages are numeric - handle data.table/data.frame input
  if(is.data.table(age_data) || is.data.frame(age_data)) {
    ages <- as.numeric(age_data[[1]])
  } else {
    ages <- as.numeric(age_data)
  }
  
  # Filter to Lee's suggested age range (10-79)
  ages <- ages[ages >= 10 & ages <= 79 & !is.na(ages)]
  
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
# BENFORD'S LAW ANALYSIS (adapted for Colombian data)
########################################

calculate_benford_metrics <- function(income_data, include_zeros = TRUE) {
  # Filter income based on include_zeros parameter
  if (include_zeros) {
    # Keep all non-negative income
    clean_income <- income_data[income_data >= 0]
    # For zeros, assign first digit as NA (will be excluded from Benford analysis)
    first_digits <- ifelse(clean_income == 0, NA, as.numeric(substr(as.character(abs(clean_income)), 1, 1)))
  } else {
    # Remove zeros and negatives
    clean_income <- income_data[income_data > 0]
    # Extract first digit
    first_digits <- as.numeric(substr(as.character(abs(clean_income)), 1, 1))
  }
  
  # Filter to valid first digits (1-9)
  first_digits <- first_digits[first_digits %in% 1:9]
  
  if(length(first_digits) < 100) {
    return(list(abs_distance = NA, chi_square = NA, n_obs = length(clean_income)))
  }
  
  # Calculate observed frequencies
  obs_table <- table(factor(first_digits, levels = 1:9))
  obs_freq <- as.numeric(obs_table) / sum(obs_table)
  
  # Calculate metrics
  benford_abs_distance <- sum(abs(obs_freq - benford_expected))
  benford_chi_square <- sum((obs_freq - benford_expected)^2 / benford_expected)
  
  return(list(abs_distance = benford_abs_distance, 
              chi_square = benford_chi_square, 
              n_obs = length(clean_income)))
}

########################################
# CONTROL VARIABLES CALCULATION (adapted for Colombian data)
########################################

calculate_control_variables <- function(data) {
  # Convert to data.frame if data.table for compatibility
  if(is.data.table(data)) {
    data <- as.data.frame(data)
  }
  
  # Initialize with NA values
  urban_share <- NA
  
  # Urban share
  if("urbano" %in% names(data) && sum(!is.na(data$urbano)) > 0) {
    urban_share <- mean(data$urbano == 1, na.rm = TRUE)
  }
  
  return(list(urban_share = urban_share))
}

########################################
# OUTLIER REMOVAL FUNCTIONS
########################################

remove_income_outliers <- function(income_data, include_zeros = TRUE) {
  # Handle data.table or data.frame input
  if(is.data.table(income_data) || is.data.frame(income_data)) {
    income_data <- as.numeric(income_data[[1]])
  } else {
    income_data <- as.numeric(income_data)
  }
  
  if (include_zeros) {
    # Keep zeros and positive values
    clean_income <- income_data[income_data >= 0 & !is.na(income_data)]
  } else {
    # Remove zeros and negatives
    clean_income <- income_data[income_data > 0 & !is.na(income_data)]
  }
  
  # If we're keeping zeros, only apply IQR to positive values
  if (include_zeros && any(clean_income > 0)) {
    positive_income <- clean_income[clean_income > 0]
    
    # Calculate IQR bounds on positive income only
    q1 <- quantile(positive_income, 0.25, na.rm = TRUE)
    q3 <- quantile(positive_income, 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1
    
    # Define bounds for positive income
    lower_bound <- q1 - 1.5 * iqr_val
    upper_bound <- q3 + 1.5 * iqr_val
    
    # Apply filter: keep zeros and income within bounds
    clean_income <- clean_income[clean_income == 0 | (clean_income >= lower_bound & clean_income <= upper_bound)]
  } else if (!include_zeros && length(clean_income) > 0) {
    # Apply IQR to all positive values
    q1 <- quantile(clean_income, 0.25, na.rm = TRUE)
    q3 <- quantile(clean_income, 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1
    
    lower_bound <- q1 - 1.5 * iqr_val
    upper_bound <- q3 + 1.5 * iqr_val
    
    clean_income <- clean_income[clean_income >= lower_bound & clean_income <= upper_bound]
  }
  
  return(clean_income)
}

########################################
# RESIDUALIZATION FUNCTION
########################################

residualize_data <- function(data, target_var, level = "reg-tot") {
  # Check if we have enough years for meaningful fixed effects
  n_years <- length(unique(data$ano))
  if (n_years < 2) {
    message("Skipping residualization of ", target_var, " - need multiple years for fixed effects (found ", n_years, " year)")
    return(data[[target_var]])
  }
  
  # Residualize against fixed effects based on analysis level
  if (level == "reg-tot") {
    # For regional level: residualize against region FE + year FE
    n_regions <- length(unique(data$region_number))
    if (n_regions < 2) {
      message("Skipping residualization of ", target_var, " - need multiple regions (found ", n_regions, " region)")
      return(data[[target_var]])
    }
    fixed_effects <- c("region_number", "ano")
  } else {
    # For regional-area level: residualize against region-area FE + year FE
    # Create region-area identifier
    data$region_area_id <- paste(data$region_number, data$urban_area, sep = "_")
    n_region_areas <- length(unique(data$region_area_id))
    if (n_region_areas < 2) {
      message("Skipping residualization of ", target_var, " - need multiple region-areas (found ", n_region_areas, " region-area)")
      return(data[[target_var]])
    }
    fixed_effects <- c("region_area_id", "ano")
  }
  
  # Create formula with fixed effects
  formula_str <- paste(target_var, "~ factor(", fixed_effects[1], ") + factor(", fixed_effects[2], ")")
  
  # Fit model and extract residuals
  tryCatch({
    model <- lm(as.formula(formula_str), data = data)
    
    # Check if model estimation was successful
    if (any(is.na(coef(model)))) {
      message("Model estimation failed for ", target_var, " - returning original variable")
      return(data[[target_var]])
    }
    
    residuals_result <- residuals(model)
    message("Residualized ", target_var, " against ", paste(fixed_effects, collapse = " + "), " fixed effects")
    return(residuals_result)
  }, error = function(e) {
    message("Error in residualization of ", target_var, ": ", e$message, " - returning original variable")
    return(data[[target_var]])
  })
}

########################################
# DATA PROCESSING VERSIONS
########################################

process_version0 <- function(results) {
  # Version 0: Raw data (zeros included)
  results_v0 <- results
  
  # Use Benford metrics with zeros
  results_v0$benford_abs_distance <- results_v0$benford_abs_distance_with_zeros
  results_v0$benford_chi_square <- results_v0$benford_chi_square_with_zeros
  results_v0$n_observations_benford <- results_v0$n_observations_benford_with_zeros
  
  return(results_v0)
}

process_version1 <- function(results) {
  # Version 1: Remove extreme Myers values only
  results_v1 <- results
  
  # Use Benford metrics without zeros
  results_v1$benford_abs_distance <- results_v1$benford_abs_distance_no_zeros
  results_v1$benford_chi_square <- results_v1$benford_chi_square_no_zeros
  results_v1$n_observations_benford <- results_v1$n_observations_benford_no_zeros
  
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

process_version2 <- function(results, level = "reg-tot") {
  # Version 2: Residualize against fixed effects (if multiple years available)
  results_v2 <- results
  
  # Use Benford metrics with zeros
  results_v2$benford_abs_distance <- results_v2$benford_abs_distance_with_zeros
  results_v2$benford_chi_square <- results_v2$benford_chi_square_with_zeros
  results_v2$n_observations_benford <- results_v2$n_observations_benford_with_zeros
  
  # Variables to residualize against fixed effects
  vars_to_residualize <- c("standardized_myers", "benford_abs_distance", "benford_chi_square")
  
  for (var in vars_to_residualize) {
    residual_name <- paste0("residual_", var)
    results_v2[[residual_name]] <- residualize_data(results_v2, var, level)
  }
  
  return(results_v2)
}

process_version3 <- function(results, level = "reg-tot") {
  # Version 3: Remove outliers + residualize against fixed effects
  results_v3 <- results
  
  # Use Benford metrics without zeros
  results_v3$benford_abs_distance <- results_v3$benford_abs_distance_no_zeros
  results_v3$benford_chi_square <- results_v3$benford_chi_square_no_zeros
  results_v3$n_observations_benford <- results_v3$n_observations_benford_no_zeros
  
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
  
  # Then residualize against fixed effects
  vars_to_residualize <- c("standardized_myers", "benford_abs_distance", "benford_chi_square")
  
  for (var in vars_to_residualize) {
    residual_name <- paste0("residual_", var)
    results_v3[[residual_name]] <- residualize_data(results_v3, var, level)
  }
  
  return(results_v3)
}

########################################
# PLOTTING FUNCTIONS (Enhanced with Professional Formatting)
########################################

create_scatter_plot <- function(data, x_var, y_var, y_label, version_num, 
                                weighted = FALSE, level = "reg-tot") {
  
  # Get unique regions for color coding
  regions <- unique(data$region_corrected)
  n_regions <- length(regions)
  
  # Generate color palette
  if (n_regions <= 8) {
    region_colors <- brewer.pal(max(8, n_regions), "Set1")[1:n_regions]
  } else {
    region_colors <- colorRampPalette(brewer.pal(9, "Set1"))(n_regions)
  }
  names(region_colors) <- regions
  
  # Create labels
  if (grepl("residual_", x_var)) {
    x_label <- "Standardized Myers Index (Residualized)"
  } else {
    x_label <- "Standardized Myers Index (0-1)"
  }
  
  if (grepl("residual_", y_var)) {
    y_label_final <- paste(y_label, "(Residualized)")
  } else {
    y_label_final <- y_label
  }
  
  # Create simple title (no version info, following enhanced format)
  title <- paste0("Myers Index vs ", gsub(" \\(Residualized\\)", "", y_label_final))
  
  # Create plot with enhanced professional formatting
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
    labs(x = x_label, y = y_label_final, title = title, color = "Region")
  
  # Add points with regional color coding and optional area shapes
  if (level == "reg-area") {
    # For regional-area level, use shapes for urban/rural
    if (weighted && "n_observations_benford" %in% names(data)) {
      p <- p + geom_point(aes(color = region_corrected, shape = factor(urban_area), 
                              size = n_observations_benford), alpha = 0.8)
      p <- p + labs(size = "N observations")
    } else {
      p <- p + geom_point(aes(color = region_corrected, shape = factor(urban_area)), 
                          alpha = 0.8)
    }
    p <- p + scale_shape_manual(name = "Area", values = c("0" = 16, "1" = 17), 
                                labels = c("0" = "Rural", "1" = "Urban"))
  } else {
    # For regional level, simple points
    if (weighted && "n_observations_benford" %in% names(data)) {
      p <- p + geom_point(aes(color = region_corrected, size = n_observations_benford), alpha = 0.8)
      p <- p + labs(size = "N observations")
    } else {
      p <- p + geom_point(aes(color = region_corrected), alpha = 0.8)
    }
  }
  
  # Apply color scale
  p <- p + scale_color_manual(values = region_colors)
  
  # Extend y-axis limits to accommodate bottom annotations
  y_range <- max(data[[y_var]], na.rm = TRUE) - min(data[[y_var]], na.rm = TRUE)
  y_min_extended <- min(data[[y_var]], na.rm = TRUE) - y_range * 0.2
  p <- p + coord_cartesian(ylim = c(y_min_extended, max(data[[y_var]], na.rm = TRUE) + y_range * 0.05))
  
  # Enhanced: Add regression lines WITH confidence intervals (following enhanced format)
  # Linear regression: black solid line with gray confidence intervals (for ALL data)
  p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
                       linetype = "solid", color = "black", fill = "gray70", alpha = 0.4)
  
  # For regional-area level: Add separate regression lines for urban and rural
  if (level == "reg-area") {
    # Urban-only regression line (blue)
    urban_data <- data[data$urban_area == 1, ]
    if (nrow(urban_data) >= 3) {  # Need at least 3 points for regression
      p <- p + geom_smooth(data = urban_data, method = "lm", formula = y ~ x, se = TRUE,
                           linetype = "dashed", color = "blue", fill = "lightblue", alpha = 0.2)
    }
    
    # Rural-only regression line (red)
    rural_data <- data[data$urban_area == 0, ]
    if (nrow(rural_data) >= 3) {  # Need at least 3 points for regression
      p <- p + geom_smooth(data = rural_data, method = "lm", formula = y ~ x, se = TRUE,
                           linetype = "dashed", color = "red", fill = "lightcoral", alpha = 0.2)
    }
  }
  
  # Loess regression: dark green dashed line with MORE TRANSPARENT intervals
  p <- p + geom_smooth(method = "loess", formula = y ~ x, se = TRUE, 
                       linetype = "dashed", color = "darkgreen", fill = "darkgreen", alpha = 0.05, span = 0.75)
  
  # Add red horizontal reference line at y=0 (following enhanced format)
  p <- p + geom_hline(yintercept = 0, color = "darkred", linetype = "solid", linewidth = 0.5)
  
  # Add correlation and significance test with improved annotation style
  cor_test <- cor.test(data[[x_var]], data[[y_var]], use = "pairwise.complete.obs")
  corr_val <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create significance annotation matching the enhanced style
  if (!is.na(corr_val) && !is.na(p_value)) {
    # Add significance stars
    stars <- ""
    if (p_value <= 0.01) stars <- "***"
    else if (p_value <= 0.05) stars <- "**"
    else if (p_value <= 0.10) stars <- "*"
    
    # Position annotations at bottom (following enhanced format)
    x_range <- max(data[[x_var]], na.rm = TRUE) - min(data[[x_var]], na.rm = TRUE)
    y_range <- max(data[[y_var]], na.rm = TRUE) - min(data[[y_var]], na.rm = TRUE)
    x_center <- (max(data[[x_var]], na.rm = TRUE) + min(data[[x_var]], na.rm = TRUE)) / 2
    y_bottom <- min(data[[y_var]], na.rm = TRUE) - y_range * 0.15
    
    # Add concise line description (following enhanced format)
    if (level == "reg-area") {
      line_description <- "Black: Overall linear regression. Blue: Urban areas only. Red: Rural areas only. Green: LOWESS smoothing."
    } else {
      line_description <- "Black line: Linear regression. Green line: LOWESS smoothing."
    }
    
    p <- p + annotate("text", 
                      x = x_center,
                      y = y_bottom,
                      label = line_description,
                      hjust = 0.5, fontface = "plain", size = 3.2)
    
    # Add correlation info - enhanced for regional-area plots
    if (level == "reg-area") {
      # Overall correlation
      correlation_text <- paste0("Overall Correlation: ", round(corr_val, 3), stars)
      p <- p + annotate("text", 
                        x = max(data[[x_var]], na.rm = TRUE) * 0.95,
                        y = y_bottom + y_range * 0.10,
                        label = correlation_text,
                        hjust = 1, fontface = "bold", size = 3.5)
      
      # Urban correlation
      urban_data <- data[data$urban_area == 1, ]
      if (nrow(urban_data) >= 3) {
        urban_cor <- cor(urban_data[[x_var]], urban_data[[y_var]], use = "pairwise.complete.obs")
        if (!is.na(urban_cor)) {
          urban_text <- paste0("Urban: ", round(urban_cor, 3))
          p <- p + annotate("text", 
                            x = max(data[[x_var]], na.rm = TRUE) * 0.95,
                            y = y_bottom + y_range * 0.05,
                            label = urban_text,
                            hjust = 1, fontface = "plain", size = 3.2, color = "blue")
        }
      }
      
      # Rural correlation
      rural_data <- data[data$urban_area == 0, ]
      if (nrow(rural_data) >= 3) {
        rural_cor <- cor(rural_data[[x_var]], rural_data[[y_var]], use = "pairwise.complete.obs")
        if (!is.na(rural_cor)) {
          rural_text <- paste0("Rural: ", round(rural_cor, 3))
          p <- p + annotate("text", 
                            x = max(data[[x_var]], na.rm = TRUE) * 0.95,
                            y = y_bottom,
                            label = rural_text,
                            hjust = 1, fontface = "plain", size = 3.2, color = "red")
        }
      }
    } else {
      # Regional level - simple correlation
      correlation_text <- paste0("Correlation: ", round(corr_val, 3), stars)
      p <- p + annotate("text", 
                        x = max(data[[x_var]], na.rm = TRUE) * 0.95,
                        y = y_bottom + y_range * 0.05,
                        label = correlation_text,
                        hjust = 1, fontface = "bold", size = 3.5)
    }
  }
  
  return(p)
}

########################################
# REGRESSION ANALYSIS FUNCTIONS
########################################

run_regression_analysis <- function(data, version_num, level = "reg-tot") {
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
      coefficients = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE),
      model_stats = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE)
    ))
  }
  
  # Create PT dummy (1 if year >= 2016)
  reg_data$PT <- ifelse(reg_data$ano >= 2016, 1, 0)
  
  # Define regression specifications based on level
  if (level == "reg-area") {
    # For region-area level, include rural dummy
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
      "Model 2: + PT" = paste(benford_var, "~", myers_var, "+ PT")
    )
  }
  
  # Run regressions
  results_df <- data.frame()
  model_stats_df <- data.frame()
  
  for (i in seq_along(reg_specs)) {
    model_name <- names(reg_specs)[i]
    formula_str <- reg_specs[[i]]
    
    tryCatch({
      model <- lm(as.formula(formula_str), data = reg_data)
      
      # Extract coefficients
      model_tidy <- tidy(model)
      model_tidy$Model <- model_name
      results_df <- rbind(results_df, model_tidy)
      
      # Extract model statistics
      model_stats <- data.frame(
        Model = model_name,
        r.squared = summary(model)$r.squared,
        adj.r.squared = summary(model)$adj.r.squared,
        nobs = nobs(model),
        stringsAsFactors = FALSE
      )
      model_stats_df <- rbind(model_stats_df, model_stats)
      
    }, error = function(e) {
      message("Error in model ", model_name, ": ", e$message)
    })
  }
  
  return(list(coefficients = results_df, model_stats = model_stats_df))
}

########################################
# MAIN DATA PROCESSING FUNCTION
########################################

process_colombia_data <- function() {
  # Determine which years to process
  if (TEST_MODE) {
    years_to_process <- 2018
    message("TEST MODE: Processing only year 2018")
  } else {
    years_to_process <- 2008:2024
    message("FULL MODE: Processing years 2008-2024")
  }
  
  # Initialize results data frames for both levels
  results_reg_tot <- data.frame()
  results_reg_area <- data.frame()
  
  # Process each year
  for (year in years_to_process) {
    message("Processing year ", year)
    
    # Construct file path
    file_path <- file.path(input_path, paste0("SEDLAC_col_", year, "_all.dta"))
    
    # Check if file exists
    if (!file.exists(file_path)) {
      message("  File not found: ", file_path)
      next
    }
    
    tryCatch({
      # Load data
      message("  Loading data...")
      dat <- read_dta(file_path)
      message("    Initial data loaded: ", nrow(dat), " rows, ", ncol(dat), " columns")
      
      # Debug: Check key variables exist
      required_vars <- c("edad", "ila", "ano", "urbano", "region_est2")
      missing_vars <- required_vars[!required_vars %in% names(dat)]
      if(length(missing_vars) > 0) {
        message("    ERROR: Missing required variables: ", paste(missing_vars, collapse = ", "))
        next
      }
      message("    All required variables present")
      
      # Debug: Check data types and ranges
      message("    edad range: ", min(dat$edad, na.rm = TRUE), " to ", max(dat$edad, na.rm = TRUE), 
              " (NAs: ", sum(is.na(dat$edad)), ")")
      message("    ila range: ", min(dat$ila, na.rm = TRUE), " to ", max(dat$ila, na.rm = TRUE), 
              " (NAs: ", sum(is.na(dat$ila)), ")")
      message("    ano values: ", paste(unique(dat$ano), collapse = ", "), 
              " (NAs: ", sum(is.na(dat$ano)), ")")
      message("    urbano values: ", paste(unique(dat$urbano), collapse = ", "), 
              " (NAs: ", sum(is.na(dat$urbano)), ")")
      
      # Basic data cleaning with debugging
      message("    Applying basic filters...")
      
      # Filter step by step with debugging
      original_rows <- nrow(dat)
      
      # Check for missing key variables
      before_na_filter <- nrow(dat)
      dat <- dat[!is.na(dat$edad) & !is.na(dat$ila) & !is.na(dat$ano), ]
      after_na_filter <- nrow(dat)
      message("    After removing NAs in key variables: ", after_na_filter, " rows (removed: ", before_na_filter - after_na_filter, ")")
      
      # Age filter
      before_age_filter <- nrow(dat)
      # Use which() to handle NAs safely
      age_condition <- which(dat$edad >= 10 & dat$edad <= 79)
      dat <- dat[age_condition, ]
      after_age_filter <- nrow(dat)
      message("    After age filter (10-79): ", after_age_filter, " rows (removed: ", before_age_filter - after_age_filter, ")")
      
      # Income filter
      before_income_filter <- nrow(dat)
      # Use which() to handle NAs safely
      income_condition <- which(dat$ila >= 0)
      dat <- dat[income_condition, ]
      after_income_filter <- nrow(dat)
      message("    After income filter (>=0): ", after_income_filter, " rows (removed: ", before_income_filter - after_income_filter, ")")
      
      if (nrow(dat) < 100) {
        message("  Insufficient data after cleaning: ", nrow(dat), " rows")
        next
      }
      
      # Extract region information with debugging
      message("    Extracting region information...")
      message("    Sample region_est2 values: ", paste(head(unique(dat$region_est2), 5), collapse = ", "))
      
      dat <- extract_region_info(dat)
      
      message("    Region extraction complete")
      message("    Unique region numbers: ", paste(sort(unique(dat$region_number[!is.na(dat$region_number)])), collapse = ", "))
      message("    Rows with missing region_number: ", sum(is.na(dat$region_number)))
      
      # Remove rows with missing region information
      before_region_filter <- nrow(dat)
      dat <- dat[!is.na(dat$region_number), ]
      after_region_filter <- nrow(dat)
      message("    After removing missing regions: ", after_region_filter, " rows (removed: ", before_region_filter - after_region_filter, ")")
      
      # Process at regional level (reg-tot)
      message("  Processing regional level data...")
      regions <- unique(dat$region_number[!is.na(dat$region_number)])
      message("    Found ", length(regions), " regions to process: ", paste(regions, collapse = ", "))
      
      for (region in regions) {
        message("    Processing region ", region)
        
        # Use which() to safely handle any potential NAs
        region_indices <- which(dat$region_number == region)
        region_data <- dat[region_indices, ]
        
        message("      Region ", region, " has ", nrow(region_data), " observations")
        
        if (nrow(region_data) < 100) {
          message("      Skipping region ", region, " due to insufficient data")
          next
        }
        
        tryCatch({
          # Calculate Myers Index
          message("      Calculating Myers Index...")
          myers_result <- calculate_traditional_myers(region_data$edad)
          n_myers <- sum(region_data$edad >= 10 & region_data$edad <= 79, na.rm = TRUE)
          message("      Myers Index calculated. Traditional: ", round(myers_result$traditional, 4), 
                  ", Standardized: ", round(myers_result$standardized, 4))
          
          # Calculate Benford metrics - both versions
          message("      Calculating Benford metrics...")
          income_with_zeros <- remove_income_outliers(region_data$ila, include_zeros = TRUE)
          benford_with_zeros <- calculate_benford_metrics(income_with_zeros, include_zeros = TRUE)
          
          income_no_zeros <- remove_income_outliers(region_data$ila, include_zeros = FALSE)
          benford_no_zeros <- calculate_benford_metrics(income_no_zeros, include_zeros = FALSE)
          message("      Benford metrics calculated")
          
          # Calculate control variables
          message("      Calculating control variables...")
          controls <- calculate_control_variables(region_data)
          message("      Control variables calculated")
          
          # Get region info
          region_name <- unique(region_data$region_corrected)[1]
          
          # Add to results
          new_row <- data.frame(
            region_number = region,
            region_corrected = region_name,
            ano = year,
            n_observations_myers = n_myers,
            n_observations_benford_with_zeros = benford_with_zeros$n_obs,
            n_observations_benford_no_zeros = benford_no_zeros$n_obs,
            traditional_myers = myers_result$traditional,
            standardized_myers = myers_result$standardized,
            benford_abs_distance_with_zeros = benford_with_zeros$abs_distance,
            benford_chi_square_with_zeros = benford_with_zeros$chi_square,
            benford_abs_distance_no_zeros = benford_no_zeros$abs_distance,
            benford_chi_square_no_zeros = benford_no_zeros$chi_square,
            urban_share = controls$urban_share,
            stringsAsFactors = FALSE
          )
          
          results_reg_tot <- rbind(results_reg_tot, new_row)
          message("      Successfully added region ", region, " to results")
          
        }, error = function(e) {
          message("      ERROR processing region ", region, ": ", e$message)
        })
      }
      
      # Process at regional-area level (reg-area)
      message("  Processing regional-area level data...")
      for (region in regions) {
        for (urban_val in c(0, 1)) {
          message("    Processing region ", region, ", urban=", urban_val)
          
          # Use which() to safely handle any potential NAs
          region_area_indices <- which(dat$region_number == region & dat$urbano == urban_val)
          region_area_data <- dat[region_area_indices, ]
          
          message("      Region ", region, " urban=", urban_val, " has ", nrow(region_area_data), " observations")
          
          if (nrow(region_area_data) < 100) {
            message("      Skipping region ", region, " urban=", urban_val, " due to insufficient data")
            next
          }
          
          tryCatch({
            # Calculate Myers Index
            message("      Calculating Myers Index...")
            myers_result <- calculate_traditional_myers(region_area_data$edad)
            n_myers <- sum(region_area_data$edad >= 10 & region_area_data$edad <= 79, na.rm = TRUE)
            
            # Calculate Benford metrics - both versions
            message("      Calculating Benford metrics...")
            income_with_zeros <- remove_income_outliers(region_area_data$ila, include_zeros = TRUE)
            benford_with_zeros <- calculate_benford_metrics(income_with_zeros, include_zeros = TRUE)
            
            income_no_zeros <- remove_income_outliers(region_area_data$ila, include_zeros = FALSE)
            benford_no_zeros <- calculate_benford_metrics(income_no_zeros, include_zeros = FALSE)
            
            # Calculate control variables
            message("      Calculating control variables...")
            controls <- calculate_control_variables(region_area_data)
            
            # Get region info
            region_name <- unique(region_area_data$region_corrected)[1]
            
            # Add to results
            new_row <- data.frame(
              region_number = region,
              region_corrected = region_name,
              ano = year,
              urban_area = urban_val,
              n_observations_myers = n_myers,
              n_observations_benford_with_zeros = benford_with_zeros$n_obs,
              n_observations_benford_no_zeros = benford_no_zeros$n_obs,
              traditional_myers = myers_result$traditional,
              standardized_myers = myers_result$standardized,
              benford_abs_distance_with_zeros = benford_with_zeros$abs_distance,
              benford_chi_square_with_zeros = benford_with_zeros$chi_square,
              benford_abs_distance_no_zeros = benford_no_zeros$abs_distance,
              benford_chi_square_no_zeros = benford_no_zeros$chi_square,
              urban_share = controls$urban_share,
              stringsAsFactors = FALSE
            )
            
            results_reg_area <- rbind(results_reg_area, new_row)
            message("      Successfully added region ", region, " urban=", urban_val, " to results")
            
          }, error = function(e) {
            message("      ERROR processing region ", region, " urban=", urban_val, ": ", e$message)
          })
        }
      }
      
      # Clear data to save memory
      rm(dat)
      gc()
      
    }, error = function(e) {
      message("  Error processing year ", year, ": ", e$message)
    })
  }
  
  message("Data processing complete.")
  message("Regional level results: ", nrow(results_reg_tot), " observations")
  message("Regional-area level results: ", nrow(results_reg_area), " observations")
  
  return(list(reg_tot = results_reg_tot, reg_area = results_reg_area))
}

########################################
# MAIN EXECUTION
########################################

message("Starting Colombia Myers-Benford Analysis...")

# Process the data
colombia_results <- process_colombia_data()

message("Processing results:")
message("- Regional level: ", nrow(colombia_results$reg_tot), " observations")
message("- Regional-area level: ", nrow(colombia_results$reg_area), " observations")

# Check if we're in single-year mode and warn about limitations
if (nrow(colombia_results$reg_tot) > 0) {
  n_years_reg <- length(unique(colombia_results$reg_tot$ano))
  if (n_years_reg == 1) {
    message("\nWARNING: Analysis is based on a single year (", unique(colombia_results$reg_tot$ano), ").")
    message("This limits the analysis in several ways:")
    message("- Fixed effects residualization cannot be performed (requires multiple years)")
    message("- V02 and V03 will use original variables instead of residualized ones")
    message("- Results may be less robust than multi-year analysis")
    message("- Consider setting TEST_MODE = FALSE for full multi-year analysis\n")
  }
}

if (nrow(colombia_results$reg_tot) > 0 || nrow(colombia_results$reg_area) > 0) {
  
  # Process both levels
  for (level in c("reg-tot", "reg-area")) {
    
    if (level == "reg-tot") {
      base_results <- colombia_results$reg_tot
      level_suffix <- "_reg-tot"
    } else {
      base_results <- colombia_results$reg_area
      level_suffix <- "_reg-area"
    }
    
    if (nrow(base_results) == 0) {
      message("No data for level: ", level)
      next
    }
    
    message("Processing level: ", level)
    
    # Create all versions with proper level parameter
    results_v00 <- process_version0(base_results)
    results_v01 <- process_version1(base_results)
    results_v02 <- process_version2(base_results, level)
    results_v03 <- process_version3(base_results, level)
    
    # Check if residualization actually occurred (requires multiple years)
    n_years <- length(unique(base_results$ano))
    residualization_successful <- n_years > 1
    
    all_versions <- list(
      "V00" = list(data = results_v00, version_num = 0, is_residualized = FALSE),
      "V01" = list(data = results_v01, version_num = 1, is_residualized = FALSE),
      "V02" = list(data = results_v02, version_num = 2, is_residualized = residualization_successful),
      "V03" = list(data = results_v03, version_num = 3, is_residualized = residualization_successful)
    )
    
    # Process each version
    for (version_name in names(all_versions)) {
      version_info <- all_versions[[version_name]]
      version_data <- version_info$data
      version_num <- version_info$version_num
      is_residualized <- version_info$is_residualized
      
      message("  Processing version ", version_name, " for level ", level)
      
      # Determine variables to use
      if (is_residualized) {
        x_var <- "residual_standardized_myers"
        y_vars <- list(
          # MAE = Mean Absolute Error (Absolute Distance from Benford's Law) - MAIN RESULTS
          "mae" = list(var = "residual_benford_abs_distance", label = "Absolute Distance from Benford's Law"),
          # Chi-square statistic - CHECKS FOLDER
          "chi_square" = list(var = "residual_benford_chi_square", label = "Chi-Square Statistic")
        )
      } else {
        x_var <- "standardized_myers"
        y_vars <- list(
          # MAE = Mean Absolute Error (Absolute Distance from Benford's Law) - MAIN RESULTS  
          "mae" = list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
          # Chi-square statistic - CHECKS FOLDER
          "chi_square" = list(var = "benford_chi_square", label = "Chi-Square Statistic")
        )
      }
      
      # Create plots
      for (measure_name in names(y_vars)) {
        y_info <- y_vars[[measure_name]]
        
        # Skip if insufficient data
        if (sum(complete.cases(version_data[, c(x_var, y_info$var)])) < 5) {
          message("    Insufficient data for ", measure_name, " plots")
          next
        }
        
        # Create unweighted plot
        p_unweighted <- create_scatter_plot(
          data = version_data,
          x_var = x_var,
          y_var = y_info$var,
          y_label = y_info$label,
          version_num = version_num,
          weighted = FALSE,
          level = level
        )
        
        # Create weighted plot
        p_weighted <- create_scatter_plot(
          data = version_data,
          x_var = x_var,
          y_var = y_info$var,
          y_label = y_info$label,
          version_num = version_num,
          weighted = TRUE,
          level = level
        )
        
        # Save plots with enhanced formatting and "_2" suffix
        unweighted_filename <- paste0("01_Myers_Benford_Col_", version_name, level_suffix, "_unweighted_", measure_name, "_2.png")
        weighted_filename <- paste0("01_Myers_Benford_Col_", version_name, level_suffix, "_weighted_", measure_name, "_2.png")
        
        # IMPORTANT: MAE (absolute deviation) plots go to MAIN folder, Chi-square plots go to CHECKS folder
        if (measure_name == "mae") {
          # Save MAE (Absolute Distance from Benford's Law) plots in main folder
          ggsave(file.path(output_path_plots_main, unweighted_filename), 
                 p_unweighted, width = 10, height = 7, dpi = 300)
          ggsave(file.path(output_path_plots_main, weighted_filename), 
                 p_weighted, width = 10, height = 7, dpi = 300)
          message("    Saved MAE plots (main results) for ", version_name, " ", level)
        } else {
          # Save Chi-Square plots in checks folder
          ggsave(file.path(output_path_plots_checks, unweighted_filename), 
                 p_unweighted, width = 10, height = 7, dpi = 300)
          ggsave(file.path(output_path_plots_checks, weighted_filename), 
                 p_weighted, width = 10, height = 7, dpi = 300)
          message("    Saved ", measure_name, " plots (checks folder) for ", version_name, " ", level)
        }
      }
      
      # Run regression analysis
      reg_results <- run_regression_analysis(version_data, version_num, level)
      
      # Create Excel output
      excel_filename <- paste0("01_Myers_Benford_Col_", version_name, level_suffix, ".xlsx")
      
      excel_data <- list(
        "Results" = version_data,
        "Regression_Coefficients" = reg_results$coefficients,
        "Model_Statistics" = reg_results$model_stats
      )
      
      write_xlsx(excel_data, file.path(output_path_excel, excel_filename))
      
      # Save regression results as text
      if (nrow(reg_results$coefficients) > 0 && !"Insufficient data" %in% reg_results$coefficients$Model) {
        txt_filename <- paste0("01_Myers_Benford_Col_", version_name, level_suffix, ".txt")
        
        # Create simple text output for regression results
        reg_text <- paste("Colombia Myers-Benford Analysis - ", version_name, " - ", level, "\n")
        reg_text <- paste0(reg_text, paste(rep("=", 60), collapse=""), "\n\n")
        
        for (model in unique(reg_results$coefficients$Model)) {
          model_data <- reg_results$coefficients[reg_results$coefficients$Model == model, ]
          reg_text <- paste0(reg_text, model, "\n")
          reg_text <- paste0(reg_text, paste(rep("-", 40), collapse=""), "\n")
          
          for (i in 1:nrow(model_data)) {
            stars <- ""
            # Handle NA p-values safely
            p_val <- model_data$p.value[i]
            if (!is.na(p_val)) {
              if (p_val < 0.01) stars <- "***"
              else if (p_val < 0.05) stars <- "**"
              else if (p_val < 0.10) stars <- "*"
            }
            
            # Handle NA standard errors safely
            std_err <- model_data$std.error[i]
            std_err_text <- if(is.na(std_err)) "  (NA)  " else sprintf("(%6.4f)", std_err)
            
            reg_text <- paste0(reg_text, sprintf("%-20s %8.4f%3s %s\n", 
                                                 model_data$term[i], 
                                                 model_data$estimate[i], 
                                                 stars,
                                                 std_err_text))
          }
          
          # Add R-squared
          model_stats <- reg_results$model_stats[reg_results$model_stats$Model == model, ]
          if (nrow(model_stats) > 0) {
            r_sq <- model_stats$r.squared[1]
            n_obs <- model_stats$nobs[1]
            
            reg_text <- paste0(reg_text, sprintf("R-squared:           %8.4f\n", 
                                                 ifelse(is.na(r_sq), NA, r_sq)))
            reg_text <- paste0(reg_text, sprintf("Observations:        %8.0f\n", 
                                                 ifelse(is.na(n_obs), 0, n_obs)))
          }
          reg_text <- paste0(reg_text, "\n")
        }
        
        reg_text <- paste0(reg_text, "Notes: Standard errors in parentheses\n")
        reg_text <- paste0(reg_text, "*** p<0.01, ** p<0.05, * p<0.10\n")
        
        # Add warning if using single year
        n_years <- length(unique(version_data$ano))
        if (n_years == 1) {
          reg_text <- paste0(reg_text, "\nWARNING: Analysis based on single year (", unique(version_data$ano), "). ")
          reg_text <- paste0(reg_text, "Results may be less reliable. Consider using multiple years for robust analysis.\n")
        }
        
        writeLines(reg_text, file.path(output_path_docs, txt_filename))
      }
      
      message("    Version ", version_name, " complete for level ", level)
    }
  }
  
  message("All analyses completed successfully!")
  message("\nEnhanced plot formatting applied:")
  message("✅ Professional formatting with red reference line at y=0")
  message("✅ Black solid regression line with gray confidence intervals (all data)")
  message("✅ REGIONAL-AREA PLOTS: Blue dashed line (urban only) + Red dashed line (rural only)")
  message("✅ Dark green dashed LOWESS line with highly transparent confidence intervals") 
  message("✅ Correlation values with significance stars (**/***)")
  message("✅ Separate urban/rural correlations displayed for regional-area plots")
  message("✅ Urban/rural shape differentiation (triangles vs circles)")
  message("✅ Concise titles and explanatory annotations")
  message("\nFile organization:")
  message("📁 MAIN FOLDER: Myers vs. Absolute Distance from Benford's Law (MAE)")
  message("📁 CHECKS FOLDER: Myers vs. Chi-Square Statistic")
  message("📁 All files saved with '_2' suffix for enhanced versions")
  
} else {
  message("No valid results generated.")
  message("Debug information:")
  message("- Regional level observations: ", nrow(colombia_results$reg_tot))
  message("- Regional-area level observations: ", nrow(colombia_results$reg_area))
  message("Check the debug messages above to identify the issue.")
  message("Common issues:")
  message("1. File path incorrect or file doesn't exist")
  message("2. Variable names don't match expected names (edad, ila, ano, urbano, region_est2)")
  message("3. Region extraction failed due to unexpected region_est2 format")
  message("4. Insufficient data after filtering (need >100 observations per region/region-area)")
  stop("No valid results generated. Check input data and file paths.")
}

message("Colombia Myers-Benford Analysis finished.")

# Show any warnings that occurred
if(length(warnings()) > 0) {
  message("\nWarnings encountered during execution:")
  print(warnings())
}

########################################
# IMPORTANT NOTES FOR FIRST RUN
########################################
# 
# Before running this code on your full dataset, please verify:
# 
# 1. Variable names in SEDLAC data:
#    - Check that "edad", "ila", "ano", "urbano" variables exist
#    - Verify their coding and data types
#    - Only "urbano" is used as control variable (1=urban, 0=rural)
# 
# 2. Data structure:
#    - Confirm "region_est2" format matches expected pattern
#    - Verify "ila" contains labor income as expected
#    - Check "urbano" coding (1=urban, 0=rural)
# 
# 3. Test mode:
#    - Run with TEST_MODE = TRUE first
#    - Check outputs for 2018 data
#    - Verify region mapping works correctly
# 
# 4. File paths:
#    - Confirm SEDLAC files exist in specified input path
#    - Check file naming convention: SEDLAC_col_YYYY_all.dta
# 
# 5. Memory management:
#    - Monitor memory usage for large datasets
#    - Consider processing in smaller batches if needed
#
# 6. Analysis versions:
#    - V00: Raw data with zeros included
#    - V01: Raw data with zeros excluded + outlier removal
#    - V02: Fixed effects residualized (region + year FE) with zeros included
#          (Note: Requires multiple years - reverts to raw data if single year)
#    - V03: Fixed effects residualized (region + year FE) with zeros excluded + outlier removal
#          (Note: Requires multiple years - reverts to raw data if single year)
#    - Residualization removes systematic regional and temporal patterns
#
# 7. Regression specifications:
#    - Regional level: Only Myers + PT dummy
#    - Regional-area level: Myers + PT dummy + Rural dummy + PT×Rural interaction
#    - PT dummy = 1 if year >= 2016, 0 otherwise
#
# 8. Single-year limitations:
#    - Fixed effects residualization not possible with single year
#    - V02 and V03 will use original variables instead of residualized ones
#    - For robust analysis, use multiple years (set TEST_MODE = FALSE)
#
# 9. Performance optimizations:
#    - Uses data.table for faster operations on large datasets
#    - Batch processing instead of individual region loops
#    - Efficient filtering and grouping operations
#    - Aggressive memory management with immediate cleanup
#
# 10. Enhanced plot formatting:
#     - Professional styling with red reference line at y=0
#     - Black solid regression line with gray confidence intervals (all data)
#     - For regional-area plots: Blue dashed line (urban only) + Red dashed line (rural only)
#     - Dark green dashed LOWESS line with highly transparent confidence intervals
#     - Correlation values with significance testing and stars
#     - Separate urban/rural correlations shown for regional-area plots
#     - Urban/rural shape differentiation for regional-area plots
#     - MAE plots (absolute distance) saved in main folder
#     - Chi-square plots saved in checks folder
#     - Files saved with "_2" suffix for enhanced versions
#