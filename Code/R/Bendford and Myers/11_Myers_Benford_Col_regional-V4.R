########################################
# Colombia Myers-Benford Analysis - Enhanced V2.0
########################################
# Author: Luis Castellanos Rodriguez
# Modified: 2025-08-16
# Enhanced version with:
# - Clustered standard errors by region
# - Individual-level outlier removal
# - Streamlined to 2 versions (V01: Raw, V02: Cleaned)
# - Panel A (no FE) + Panel B (with FE) structure

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
              "ggrepel", "gridExtra", "RColorBrewer", "broom", "stringr", 
              "sandwich", "lmtest")
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
  data$region_number <- as.numeric(str_extract(data$region_est2, "^\\d+"))
  region_mapping <- create_region_mapping()
  data <- merge(data, region_mapping, by = "region_number", all.x = TRUE)
  data$region_corrected[is.na(data$region_corrected)] <- "Unknown"
  return(data)
}

########################################
# STATISTICAL CONSTANTS AND FUNCTIONS
########################################

# Define the Benford expected probabilities for digits 1 to 9
benford_expected <- log10(1 + 1/(1:9))

########################################
# MYERS INDEX CALCULATION
########################################

calculate_traditional_myers <- function(age_data) {
  if(is.data.table(age_data) || is.data.frame(age_data)) {
    ages <- as.numeric(age_data[[1]])
  } else {
    ages <- as.numeric(age_data)
  }
  
  ages <- ages[ages >= 10 & ages <= 79 & !is.na(ages)]
  
  if(length(ages) < 100) {
    return(list(traditional = NA, standardized = NA))
  }
  
  terminal_digits <- ages %% 10
  blended_counts <- numeric(10)
  
  for(d in 0:9) {
    sum_count <- 0
    for(start_digit in 0:9) {
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
  
  total_count <- sum(blended_counts)
  if(total_count == 0) return(list(traditional = NA, standardized = NA))
  
  blended_percents <- 100 * blended_counts / total_count
  deviations <- abs(blended_percents - 10)
  traditional_myers <- sum(deviations) / 2
  standardized_myers <- traditional_myers / 90
  
  return(list(traditional = traditional_myers, standardized = standardized_myers))
}

########################################
# BENFORD'S LAW ANALYSIS
########################################

calculate_benford_metrics <- function(income_data, include_zeros = TRUE) {
  if (include_zeros) {
    clean_income <- income_data[income_data >= 0]
    first_digits <- ifelse(clean_income == 0, NA, as.numeric(substr(as.character(abs(clean_income)), 1, 1)))
  } else {
    clean_income <- income_data[income_data > 0]
    first_digits <- as.numeric(substr(as.character(abs(clean_income)), 1, 1))
  }
  
  first_digits <- first_digits[first_digits %in% 1:9]
  
  if(length(first_digits) < 100) {
    return(list(abs_distance = NA, chi_square = NA, n_obs = length(clean_income)))
  }
  
  obs_table <- table(factor(first_digits, levels = 1:9))
  obs_freq <- as.numeric(obs_table) / sum(obs_table)
  
  benford_abs_distance <- sum(abs(obs_freq - benford_expected))
  benford_chi_square <- sum((obs_freq - benford_expected)^2 / benford_expected)
  
  return(list(abs_distance = benford_abs_distance, 
              chi_square = benford_chi_square, 
              n_obs = length(clean_income)))
}

########################################
# INDIVIDUAL-LEVEL OUTLIER REMOVAL FUNCTIONS
########################################

# Remove individual income outliers within region-year using IQR method
remove_individual_income_outliers <- function(income_data, include_zeros = TRUE) {
  if(is.data.table(income_data) || is.data.frame(income_data)) {
    income_data <- as.numeric(income_data[[1]])
  } else {
    income_data <- as.numeric(income_data)
  }
  
  if (include_zeros) {
    clean_income <- income_data[income_data >= 0 & !is.na(income_data)]
  } else {
    clean_income <- income_data[income_data > 0 & !is.na(income_data)]
  }
  
  if(length(clean_income) < 10) {
    return(clean_income)  # Not enough data for outlier detection
  }
  
  if (include_zeros && any(clean_income > 0)) {
    positive_income <- clean_income[clean_income > 0]
    
    # Calculate IQR bounds on positive income only
    q1 <- quantile(positive_income, 0.25, na.rm = TRUE)
    q3 <- quantile(positive_income, 0.75, na.rm = TRUE)
    iqr_val <- q3 - q1
    
    lower_bound <- q1 - 1.5 * iqr_val
    upper_bound <- q3 + 1.5 * iqr_val
    
    # Keep zeros and income within bounds
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

# Remove individual age outliers within region-year using IQR method
remove_individual_age_outliers <- function(age_data) {
  ages <- age_data[age_data >= 10 & age_data <= 79 & !is.na(age_data)]
  
  if(length(ages) < 10) {
    return(ages)  # Not enough data for outlier detection
  }
  
  # Calculate IQR bounds
  q1 <- quantile(ages, 0.25, na.rm = TRUE)
  q3 <- quantile(ages, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1
  
  # Define bounds (but keep within 10-79 range)
  lower_bound <- max(10, q1 - 1.5 * iqr_val)
  upper_bound <- min(79, q3 + 1.5 * iqr_val)
  
  # Filter outliers
  filtered_ages <- ages[ages >= lower_bound & ages <= upper_bound]
  
  return(filtered_ages)
}

########################################
# CONTROL VARIABLES CALCULATION
########################################

calculate_control_variables <- function(data) {
  if(is.data.table(data)) {
    data <- as.data.frame(data)
  }
  
  urban_share <- NA
  
  if("urbano" %in% names(data) && sum(!is.na(data$urbano)) > 0) {
    urban_share <- mean(data$urbano == 1, na.rm = TRUE)
  }
  
  return(list(urban_share = urban_share))
}

########################################
# REGRESSION ANALYSIS WITH CLUSTERED STANDARD ERRORS
########################################

run_regression_analysis_clustered <- function(data, version_name, level = "reg-tot") {
  myers_var <- "standardized_myers"
  benford_var <- "benford_abs_distance"
  
  # Prepare regression data
  reg_data <- data[complete.cases(data[, c(myers_var, benford_var, "region_number", "ano")]), ]
  
  if (nrow(reg_data) < 10) {
    message("Insufficient data for regression analysis in ", version_name)
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
    message("WARNING: Smallest cluster has only ", min_cluster_size, " observations. Consider standard SEs.")
  }
  
  message("Clustering info: ", n_clusters, " regions, cluster sizes: ", 
          min(cluster_table), "-", max(cluster_table), " observations")
  
  # Create PT dummy (1 if year >= 2016)
  reg_data$PT <- ifelse(reg_data$ano >= 2016, 1, 0)
  
  # Define regression specifications based on level
  if (level == "reg-area") {
    reg_data$rural <- 1 - reg_data$urban_area
    
    reg_specs <- list(
      "Model 1: Simple" = paste(benford_var, "~", myers_var),
      "Model 2: + PT" = paste(benford_var, "~", myers_var, "+ PT"),
      "Model 3: + PT + Rural" = paste(benford_var, "~", myers_var, "+ PT + rural"),
      "Model 4: + PT*Rural" = paste(benford_var, "~", myers_var, "+ PT + rural + PT:rural")
    )
  } else {
    reg_specs <- list(
      "Model 1: Simple" = paste(benford_var, "~", myers_var),
      "Model 2: + PT" = paste(benford_var, "~", myers_var, "+ PT")
    )
  }
  
  # Function to run regression with clustered standard errors
  run_clustered_regression <- function(formula_str, data, include_fe = FALSE) {
    tryCatch({
      # Add fixed effects if requested
      if (include_fe && length(unique(data$region_number)) > 1 && length(unique(data$ano)) > 1) {
        if (level == "reg-area") {
          data$region_area_id <- paste(data$region_number, data$urban_area, sep = "_")
          formula_str <- paste(formula_str, "+ factor(region_area_id) + factor(ano)")
        } else {
          formula_str <- paste(formula_str, "+ factor(region_number) + factor(ano)")
        }
      }
      
      # Fit the model
      model <- lm(as.formula(formula_str), data = data)
      
      # Calculate clustered standard errors (clustered by region)
      clustered_vcov <- vcovCL(model, cluster = data$region_number)
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
      message("Error in regression: ", e$message)
      return(list(
        coefficients = data.frame(term = "Error", estimate = NA, std.error = NA, 
                                  statistic = NA, p.value = NA, stringsAsFactors = FALSE),
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
        message("Error in model ", model_name, " without FE")
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
          message("Error in model ", model_name, " with FE")
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
# PLOTTING FUNCTIONS
########################################

create_scatter_plot <- function(data, x_var, y_var, y_label, version_name, 
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
  x_label <- "Standardized Myers Index (0-1)"
  y_label_final <- y_label
  
  # Create simple title
  title <- paste0("Myers Index vs ", y_label_final, " (", version_name, ")")
  
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
  
  # Add regression lines WITH confidence intervals
  # Linear regression: black solid line with gray confidence intervals (for ALL data)
  p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
                       linetype = "solid", color = "black", fill = "gray70", alpha = 0.4)
  
  # For regional-area level: Add separate regression lines for urban and rural
  if (level == "reg-area") {
    # Urban-only regression line (blue)
    urban_data <- data[data$urban_area == 1, ]
    if (nrow(urban_data) >= 3) {
      p <- p + geom_smooth(data = urban_data, method = "lm", formula = y ~ x, se = TRUE,
                           linetype = "dashed", color = "blue", fill = "lightblue", alpha = 0.2)
    }
    
    # Rural-only regression line (red)
    rural_data <- data[data$urban_area == 0, ]
    if (nrow(rural_data) >= 3) {
      p <- p + geom_smooth(data = rural_data, method = "lm", formula = y ~ x, se = TRUE,
                           linetype = "dashed", color = "red", fill = "lightcoral", alpha = 0.2)
    }
  }
  
  # Loess regression: dark green dashed line with transparent intervals
  p <- p + geom_smooth(method = "loess", formula = y ~ x, se = TRUE, 
                       linetype = "dashed", color = "darkgreen", fill = "darkgreen", alpha = 0.05, span = 0.75)
  
  # Add red horizontal reference line at y=0
  p <- p + geom_hline(yintercept = 0, color = "darkred", linetype = "solid", linewidth = 0.5)
  
  # Add correlation and significance test
  cor_test <- cor.test(data[[x_var]], data[[y_var]], use = "pairwise.complete.obs")
  corr_val <- cor_test$estimate
  p_value <- cor_test$p.value
  
  if (!is.na(corr_val) && !is.na(p_value)) {
    # Add significance stars
    stars <- ""
    if (p_value <= 0.01) stars <- "***"
    else if (p_value <= 0.05) stars <- "**"
    else if (p_value <= 0.10) stars <- "*"
    
    # Position annotations at bottom
    x_range <- max(data[[x_var]], na.rm = TRUE) - min(data[[x_var]], na.rm = TRUE)
    y_range <- max(data[[y_var]], na.rm = TRUE) - min(data[[y_var]], na.rm = TRUE)
    x_center <- (max(data[[x_var]], na.rm = TRUE) + min(data[[x_var]], na.rm = TRUE)) / 2
    y_bottom <- min(data[[y_var]], na.rm = TRUE) - y_range * 0.15
    
    # Add line description
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
    
    # Add correlation info
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
      
      # Check key variables exist
      required_vars <- c("edad", "ila", "ano", "urbano", "region_est2")
      missing_vars <- required_vars[!required_vars %in% names(dat)]
      if(length(missing_vars) > 0) {
        message("    ERROR: Missing required variables: ", paste(missing_vars, collapse = ", "))
        next
      }
      
      # Basic data cleaning
      message("    Applying basic filters...")
      original_rows <- nrow(dat)
      
      # Remove NAs in key variables
      dat <- dat[!is.na(dat$edad) & !is.na(dat$ila) & !is.na(dat$ano), ]
      message("    After removing NAs: ", nrow(dat), " rows")
      
      # Age filter
      dat <- dat[dat$edad >= 10 & dat$edad <= 79, ]
      message("    After age filter (10-79): ", nrow(dat), " rows")
      
      # Income filter
      dat <- dat[dat$ila >= 0, ]
      message("    After income filter (>=0): ", nrow(dat), " rows")
      
      if (nrow(dat) < 100) {
        message("  Insufficient data after cleaning: ", nrow(dat), " rows")
        next
      }
      
      # Extract region information
      message("    Extracting region information...")
      dat <- extract_region_info(dat)
      
      # Remove rows with missing region information
      dat <- dat[!is.na(dat$region_number), ]
      message("    After removing missing regions: ", nrow(dat), " rows")
      
      # Process at regional level (reg-tot)
      message("  Processing regional level data...")
      regions <- unique(dat$region_number[!is.na(dat$region_number)])
      message("    Found ", length(regions), " regions to process")
      
      for (region in regions) {
        region_data <- dat[dat$region_number == region, ]
        
        if (nrow(region_data) < 100) {
          message("      Skipping region ", region, " due to insufficient data")
          next
        }
        
        tryCatch({
          # V01: Raw data processing
          # Myers Index on all ages
          myers_result_v1 <- calculate_traditional_myers(region_data$edad)
          n_myers_v1 <- sum(region_data$edad >= 10 & region_data$edad <= 79, na.rm = TRUE)
          
          # Benford metrics on raw income data (with zeros)
          benford_v1 <- calculate_benford_metrics(region_data$ila, include_zeros = TRUE)
          
          # V02: Individual outlier removal processing
          # Apply individual outlier removal within this region-year
          filtered_ages_v2 <- remove_individual_age_outliers(region_data$edad)
          myers_result_v2 <- calculate_traditional_myers(filtered_ages_v2)
          n_myers_v2 <- length(filtered_ages_v2)
          
          # Apply individual outlier removal to income (excluding zeros)
          filtered_income_v2 <- remove_individual_income_outliers(region_data$ila, include_zeros = FALSE)
          benford_v2 <- calculate_benford_metrics(filtered_income_v2, include_zeros = FALSE)
          
          # Calculate control variables
          controls <- calculate_control_variables(region_data)
          region_name <- unique(region_data$region_corrected)[1]
          
          # Add V01 results
          new_row_v1 <- data.frame(
            region_number = region,
            region_corrected = region_name,
            ano = year,
            version = "V01",
            n_observations_myers = n_myers_v1,
            n_observations_benford = benford_v1$n_obs,
            traditional_myers = myers_result_v1$traditional,
            standardized_myers = myers_result_v1$standardized,
            benford_abs_distance = benford_v1$abs_distance,
            benford_chi_square = benford_v1$chi_square,
            urban_share = controls$urban_share,
            stringsAsFactors = FALSE
          )
          
          # Add V02 results
          new_row_v2 <- data.frame(
            region_number = region,
            region_corrected = region_name,
            ano = year,
            version = "V02",
            n_observations_myers = n_myers_v2,
            n_observations_benford = benford_v2$n_obs,
            traditional_myers = myers_result_v2$traditional,
            standardized_myers = myers_result_v2$standardized,
            benford_abs_distance = benford_v2$abs_distance,
            benford_chi_square = benford_v2$chi_square,
            urban_share = controls$urban_share,
            stringsAsFactors = FALSE
          )
          
          results_reg_tot <- rbind(results_reg_tot, new_row_v1, new_row_v2)
          
        }, error = function(e) {
          message("      ERROR processing region ", region, ": ", e$message)
        })
      }
      
      # Process at regional-area level (reg-area)
      message("  Processing regional-area level data...")
      for (region in regions) {
        for (urban_val in c(0, 1)) {
          region_area_data <- dat[dat$region_number == region & dat$urbano == urban_val, ]
          
          if (nrow(region_area_data) < 100) {
            message("      Skipping region ", region, " urban=", urban_val, " due to insufficient data")
            next
          }
          
          tryCatch({
            # V01: Raw data processing
            myers_result_v1 <- calculate_traditional_myers(region_area_data$edad)
            n_myers_v1 <- sum(region_area_data$edad >= 10 & region_area_data$edad <= 79, na.rm = TRUE)
            benford_v1 <- calculate_benford_metrics(region_area_data$ila, include_zeros = TRUE)
            
            # V02: Individual outlier removal processing
            filtered_ages_v2 <- remove_individual_age_outliers(region_area_data$edad)
            myers_result_v2 <- calculate_traditional_myers(filtered_ages_v2)
            n_myers_v2 <- length(filtered_ages_v2)
            
            filtered_income_v2 <- remove_individual_income_outliers(region_area_data$ila, include_zeros = FALSE)
            benford_v2 <- calculate_benford_metrics(filtered_income_v2, include_zeros = FALSE)
            
            controls <- calculate_control_variables(region_area_data)
            region_name <- unique(region_area_data$region_corrected)[1]
            
            # Add V01 results
            new_row_v1 <- data.frame(
              region_number = region,
              region_corrected = region_name,
              ano = year,
              urban_area = urban_val,
              version = "V01",
              n_observations_myers = n_myers_v1,
              n_observations_benford = benford_v1$n_obs,
              traditional_myers = myers_result_v1$traditional,
              standardized_myers = myers_result_v1$standardized,
              benford_abs_distance = benford_v1$abs_distance,
              benford_chi_square = benford_v1$chi_square,
              urban_share = controls$urban_share,
              stringsAsFactors = FALSE
            )
            
            # Add V02 results
            new_row_v2 <- data.frame(
              region_number = region,
              region_corrected = region_name,
              ano = year,
              urban_area = urban_val,
              version = "V02",
              n_observations_myers = n_myers_v2,
              n_observations_benford = benford_v2$n_obs,
              traditional_myers = myers_result_v2$traditional,
              standardized_myers = myers_result_v2$standardized,
              benford_abs_distance = benford_v2$abs_distance,
              benford_chi_square = benford_v2$chi_square,
              urban_share = controls$urban_share,
              stringsAsFactors = FALSE
            )
            
            results_reg_area <- rbind(results_reg_area, new_row_v1, new_row_v2)
            
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

message("Starting Colombia Myers-Benford Analysis Enhanced V2.0...")

# Process the data
colombia_results <- process_colombia_data()

message("Processing results:")
message("- Regional level: ", nrow(colombia_results$reg_tot), " observations")
message("- Regional-area level: ", nrow(colombia_results$reg_area), " observations")

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
    
    # Process each version separately
    for (version in c("V01", "V02")) {
      version_data <- base_results[base_results$version == version, ]
      
      if (nrow(version_data) == 0) {
        message("No data for version: ", version)
        next
      }
      
      message("  Processing version ", version, " for level ", level)
      
      # Create plots
      y_vars <- list(
        "mae" = list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
        "chi_square" = list(var = "benford_chi_square", label = "Chi-Square Statistic")
      )
      
      for (measure_name in names(y_vars)) {
        y_info <- y_vars[[measure_name]]
        
        # Skip if insufficient data
        if (sum(complete.cases(version_data[, c("standardized_myers", y_info$var)])) < 5) {
          message("    Insufficient data for ", measure_name, " plots")
          next
        }
        
        # Create unweighted plot
        p_unweighted <- create_scatter_plot(
          data = version_data,
          x_var = "standardized_myers",
          y_var = y_info$var,
          y_label = y_info$label,
          version_name = version,
          weighted = FALSE,
          level = level
        )
        
        # Create weighted plot
        p_weighted <- create_scatter_plot(
          data = version_data,
          x_var = "standardized_myers",
          y_var = y_info$var,
          y_label = y_info$label,
          version_name = version,
          weighted = TRUE,
          level = level
        )
        
        # Save plots
        unweighted_filename <- paste0("02_Myers_Benford_Col_", version, level_suffix, "_unweighted_", measure_name, ".png")
        weighted_filename <- paste0("02_Myers_Benford_Col_", version, level_suffix, "_weighted_", measure_name, ".png")
        
        if (measure_name == "mae") {
          ggsave(file.path(output_path_plots_main, unweighted_filename), 
                 p_unweighted, width = 10, height = 7, dpi = 300)
          ggsave(file.path(output_path_plots_main, weighted_filename), 
                 p_weighted, width = 10, height = 7, dpi = 300)
          message("    Saved MAE plots for ", version, " ", level)
        } else {
          ggsave(file.path(output_path_plots_checks, unweighted_filename), 
                 p_unweighted, width = 10, height = 7, dpi = 300)
          ggsave(file.path(output_path_plots_checks, weighted_filename), 
                 p_weighted, width = 10, height = 7, dpi = 300)
          message("    Saved chi-square plots for ", version, " ", level)
        }
      }
      
      # Run regression analysis with clustered standard errors
      reg_results <- run_regression_analysis_clustered(version_data, version, level)
      
      # Create Excel output
      excel_filename <- paste0("02_Myers_Benford_Col_", version, level_suffix, ".xlsx")
      
      excel_data <- list(
        "Results" = version_data,
        "Coefficients_No_FE" = reg_results$coefficients_no_fe,
        "Model_Stats_No_FE" = reg_results$model_stats_no_fe,
        "Coefficients_FE" = reg_results$coefficients_fe,
        "Model_Stats_FE" = reg_results$model_stats_fe
      )
      
      write_xlsx(excel_data, file.path(output_path_excel, excel_filename))
      message("    Excel file saved: ", excel_filename)
      
      message("    Version ", version, " complete for level ", level)
    }
  }
  
  message("All analyses completed successfully!")
  message("\nEnhancements implemented:")
  message("✅ Clustered standard errors by region (department level)")
  message("✅ Individual-level outlier removal within region-year combinations")
  message("✅ Streamlined to 2 versions: V01 (Raw) + V02 (Cleaned)")
  message("✅ Panel A (no FE) + Panel B (with FE) regression structure")
  message("✅ Updated file naming with '02_' prefix")
  message("✅ Professional plotting with multiple regression lines")
  
} else {
  message("No valid results generated.")
  stop("No valid results generated. Check input data and file paths.")
}

message("Colombia Myers-Benford Analysis Enhanced V2.0 finished.")

########################################
# ANALYSIS SUMMARY
########################################
#
# ENHANCED VERSION 2.0 FEATURES:
# 
# 1. CLUSTERED STANDARD ERRORS:
#    - All regressions use region-level clustering
#    - Accounts for within-region correlation across years
#    - More conservative inference than classical SEs
# 
# 2. INDIVIDUAL-LEVEL OUTLIER REMOVAL:
#    - Applied within each region-year combination
#    - IQR method for both ages and income
#    - Preserves regional heterogeneity while removing measurement errors
# 
# 3. STREAMLINED VERSIONS:
#    - V01: Raw data, zeros included in Benford (as NA for first digit)
#    - V02: Individual outliers removed, zeros excluded from Benford
#    - No residualization (use FE in regressions instead)
# 
# 4. REGRESSION STRUCTURE:
#    - Panel A: No fixed effects + clustered SE
#    - Panel B: With fixed effects + clustered SE
#    - Same model specifications as original
# 
# 5. TECHNICAL CHOICES:
#    - Individual outlier removal: within region-year (preserves heterogeneity)
#    - Zero handling: keep individuals for Myers, exclude from Benford in V02
#    - Clustering: by region_number (24 Colombian departments)
#    - File naming: "02_" prefix for enhanced version
# 
# 6. OUTPUT STRUCTURE:
#    Excel files: Results + Panel A coefficients/stats + Panel B coefficients/stats
#    PNG plots: MAE plots (main folder) + Chi-square plots (checks folder)
#    Both weighted and unweighted versions of each plot
#