########################################
# Benford Multiple-Digit Analysis with Myers Index
########################################
# Author: Luis Castellanos
# Last modification: 2025-04-27

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

# *** TEST MODE FLAG ***
# Set to TRUE to process only ARM and BOL files for testing
# Set to FALSE to process all countries
test_mode <- FALSE  # <-- CHANGE THIS TO FALSE FOR FULL RUN

# Install and load required packages
packages <- c("haven", "writexl", "dplyr", "data.table", "ggplot2", "tidyverse", 
              "ggrepel", "gridExtra", "RColorBrewer")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

# 1. Define file paths for input and outputs
input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/GLD database"

# Output paths
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_path_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Out of Repo/Outputs/Excel"
output_path_plots_base <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Out of Repo/Outputs/PNG/Myers vs. Bendford"
output_path_plots <- file.path(output_path_plots_base, "2025-04-27")
output_path_v1 <- file.path(output_path_plots, "V1")
output_path_v2 <- file.path(output_path_plots, "V2")

# Create directories if they don't exist
dir.create(output_path_excel, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_v1, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_v2, showWarnings = FALSE, recursive = TRUE)

##############################################################
# PART I: IMPLEMENTATION OF DIGITANALYSIS PACKAGE FUNCTIONS
##############################################################

# Function to extract digits from numbers
extract_digits <- function(x, num_digits = 4) {
  # Convert to character
  str_x <- as.character(abs(x))
  
  # Extract first num_digits digits or pad with NA if too short
  result <- matrix(NA, nrow = length(x), ncol = num_digits)
  
  for (i in 1:length(x)) {
    if (!is.na(x[i]) && x[i] != 0) {
      digits <- as.numeric(strsplit(str_x[i], "")[[1]])
      if (length(digits) >= num_digits) {
        result[i, 1:num_digits] <- digits[1:num_digits]
      } else {
        result[i, 1:length(digits)] <- digits
      }
    }
  }
  
  return(result)
}

# Function to generate Benford's Law expected frequencies for first n digits
generate_benford_expected <- function(num_digits = 4) {
  if (num_digits == 1) {
    # For first digit only (1-9)
    return(log10(1 + 1/(1:9)))
  } else if (num_digits == 2) {
    # For first two digits (10-99)
    probs <- numeric(90)
    for (d in 10:99) {
      probs[d-9] <- log10(1 + 1/d)
    }
    return(probs)
  } else {
    # For more digits, we need to approximate
    # This is simplified - in production we'd use a more sophisticated method
    # For demo purposes, we'll use this approximation
    num_combinations <- 10^(num_digits) - 10^(num_digits-1)
    probs <- numeric(num_combinations)
    start_num <- 10^(num_digits-1)
    
    for (i in 1:num_combinations) {
      d <- start_num + i - 1
      probs[i] <- log10(1 + 1/d)
    }
    
    # Normalize
    probs <- probs / sum(probs)
    return(probs)
  }
}

# Function to calculate Chi-Square test for Benford's Law
chi_square_test <- function(observed, expected) {
  # Ensure both vectors are the same length
  if (length(observed) != length(expected)) {
    stop("Observed and expected vectors must have the same length")
  }
  
  # Calculate Chi-Square statistic
  chi_sq <- sum((observed - expected)^2 / expected)
  
  # Calculate degrees of freedom
  df <- length(observed) - 1
  
  # Calculate p-value
  p_value <- 1 - pchisq(chi_sq, df)
  
  return(list(statistic = chi_sq, p.value = p_value, df = df))
}

# Function to calculate Mean Absolute Deviation from Benford's Law
mad_benford <- function(observed, expected) {
  return(mean(abs(observed - expected)))
}

# Function to calculate Absolute Distance from Benford's Law
abs_distance_benford <- function(observed, expected) {
  return(sum(abs(observed - expected)))
}

# Function to calculate Correlation with Benford's Law
correlation_benford <- function(observed, expected) {
  return(cor(observed, expected))
}

# Function to get frequencies of first n digits
get_digit_frequencies <- function(numbers, num_digits = 4) {
  # Extract digits
  digits_matrix <- extract_digits(numbers, num_digits)
  
  if (num_digits == 1) {
    # For first digit only
    first_digits <- digits_matrix[,1]
    first_digits <- first_digits[!is.na(first_digits)]
    first_digits <- first_digits[first_digits >= 1 & first_digits <= 9]
    
    # Count occurrences
    obs_counts <- table(factor(first_digits, levels = 1:9))
    obs_freq <- as.numeric(obs_counts) / sum(obs_counts)
    
    return(list(
      counts = obs_counts,
      frequencies = obs_freq,
      n = length(first_digits)
    ))
  } else {
    # For multiple digits - we encode the digits as a number
    # e.g., first 2 digits: 10-99, first 3 digits: 100-999, etc.
    valid_rows <- which(!is.na(digits_matrix[,1]))
    first_n_digits <- numeric(length(valid_rows))
    
    for (i in 1:length(valid_rows)) {
      row <- valid_rows[i]
      digits <- digits_matrix[row, 1:min(num_digits, ncol(digits_matrix))]
      digits <- digits[!is.na(digits)]
      
      if (length(digits) >= num_digits) {
        # Combine first n digits into a number
        first_n_digits[i] <- as.numeric(paste(digits[1:num_digits], collapse = ""))
      } else {
        first_n_digits[i] <- NA
      }
    }
    
    # Remove NAs
    first_n_digits <- first_n_digits[!is.na(first_n_digits)]
    
    # Check if we have valid range for the specified number of digits
    min_val <- 10^(num_digits-1)
    max_val <- 10^num_digits - 1
    first_n_digits <- first_n_digits[first_n_digits >= min_val & first_n_digits <= max_val]
    
    # Create factor levels for the range
    levels <- min_val:max_val
    
    # Count occurrences
    obs_counts <- table(factor(first_n_digits, levels = levels))
    obs_freq <- as.numeric(obs_counts) / sum(obs_counts)
    
    return(list(
      counts = obs_counts,
      frequencies = obs_freq,
      n = length(first_n_digits)
    ))
  }
}

# Function to test Benford's Law for first n digits
test_benford_law <- function(numbers, num_digits = 4) {
  # Get observed frequencies
  observed <- get_digit_frequencies(numbers, num_digits)
  
  # Get expected frequencies under Benford's Law
  expected_freq <- generate_benford_expected(num_digits)
  
  # Ensure observed and expected vectors match in length
  # This might be needed if some digits don't appear in the data
  if (length(observed$frequencies) < length(expected_freq)) {
    expected_freq <- expected_freq[1:length(observed$frequencies)]
    # Renormalize
    expected_freq <- expected_freq / sum(expected_freq)
  } else if (length(observed$frequencies) > length(expected_freq)) {
    observed$frequencies <- observed$frequencies[1:length(expected_freq)]
    observed$counts <- observed$counts[1:length(expected_freq)]
    observed$n <- sum(observed$counts)
  }
  
  # Calculate test statistics
  chi_sq_result <- chi_square_test(observed$frequencies, expected_freq)
  mad <- mad_benford(observed$frequencies, expected_freq)
  abs_dist <- abs_distance_benford(observed$frequencies, expected_freq)
  corr <- correlation_benford(observed$frequencies, expected_freq)
  
  return(list(
    observed = observed$frequencies,
    expected = expected_freq,
    n = observed$n,
    chi_square = chi_sq_result$statistic,
    chi_square_pvalue = chi_sq_result$p.value,
    mad = mad,
    abs_distance = abs_dist,
    correlation = corr
  ))
}

##############################################################
# PART II: DATA PROCESSING AND ANALYSIS
##############################################################

# Define the PROPER Traditional Myers Blended Index function
calculate_traditional_myers <- function(age_data) {
  # Ensure ages are numeric
  ages <- as.numeric(age_data)
  
  # Filter to relevant age range (15-90)
  ages <- ages[ages >= 15 & ages <= 90]
  
  # If insufficient data, return NA
  if(length(ages) < 100) {
    return(list(traditional = NA, standardized = NA))
  }
  
  # Extract terminal digits
  terminal_digits <- ages %% 10
  
  # Count original frequencies for each terminal digit
  digit_counts <- numeric(10)
  for(i in 0:9) {
    digit_counts[i+1] <- sum(terminal_digits == i)
  }
  
  # Calculate the blended count for each terminal digit
  blended_counts <- numeric(10)
  for(d in 0:9) {
    sum_count <- 0
    for(j in 0:9) {
      # This uses different starting points for each terminal digit
      # to create a "blended" count
      age_start <- 10 * j + d
      if(age_start >= 15 && age_start <= 90) {
        sum_count <- sum_count + sum(ages == age_start)
      }
    }
    blended_counts[d+1] <- sum_count
  }
  
  # Calculate percent distribution
  total_count <- sum(blended_counts)
  blended_percents <- 100 * blended_counts / total_count
  
  # Calculate absolute deviations from 10%
  deviations <- abs(blended_percents - 10)
  
  # Traditional Myers Index is half the sum of absolute deviations
  traditional_myers <- sum(deviations) / 2
  
  # Standardized Myers (0-1 scale)
  # The maximum possible value is 90 (when all ages end in a single digit)
  standardized_myers <- traditional_myers / 90
  
  return(list(traditional = traditional_myers, standardized = standardized_myers))
}

# Initialize global result data frame
results <- data.frame(countrycode = character(),
                      year = integer(),
                      n_observations = integer(),
                      traditional_myers = numeric(),
                      standardized_myers = numeric(),
                      benford_correlation = numeric(),
                      benford_abs_distance = numeric(),
                      benford_chi_square = numeric(),
                      benford_mad = numeric(),
                      benford_chi_square_pvalue = numeric(),
                      stringsAsFactors = FALSE)

# Get list of ALL .dta files in the input folder
all_files <- list.files(path = input_path, 
                        pattern = "\\.dta$",
                        full.names = TRUE,
                        recursive = TRUE)

if(length(all_files) == 0) {
  stop("No .dta files found in input folder")
}

# If in test mode, filter to only ARM and BOL files
if(test_mode) {
  message("*** RUNNING IN TEST MODE - Only processing ARM and BOL files ***")
  all_files <- all_files[grepl("^ARM|^BOL", basename(all_files))]
  if(length(all_files) == 0) {
    stop("No ARM or BOL files found in input folder")
  }
  message("Found ", length(all_files), " ARM/BOL files for testing")
} else {
  message("Running in FULL MODE - Processing all country files")
}

# Extract country codes from the filenames
countries <- unique(sapply(basename(all_files), function(x) {
  # Extract country code from filename (assuming format: COUNTRYCODE_*.dta)
  parts <- strsplit(x, "_")[[1]]
  if(length(parts) > 0) {
    return(parts[1])
  } else {
    return(NA)
  }
}))

# Remove any NA values
countries <- countries[!is.na(countries)]

message("Processing ", length(countries), " countries: ", paste(countries, collapse = ", "))

# Process files for each country
for(country in countries) {
  message("Processing country: ", country)
  
  # Get list of files for this country
  country_files <- list.files(path = input_path, 
                              pattern = paste0("^", country, "_.*\\.dta$"), 
                              full.names = TRUE,
                              recursive = TRUE)
  
  if(length(country_files) == 0) {
    message("No files found for country ", country, ", skipping.")
    next
  }
  
  # Process each file separately to save memory
  for(file in country_files) {
    message("  Processing file: ", file)
    
    tryCatch({
      # Load the data
      dat <- read_dta(file) %>% as.data.table()
      
      # Check for required variables
      required_vars <- c("wage_no_compen", "unitwage", "year", "countrycode", "age")
      if(!all(required_vars %in% names(dat))) {
        message("    Skipping file due to missing required variables.")
        next
      }
      
      # Filter for unit wage = 5 (monthly) and age between 15-90
      dat <- dat[!is.na(wage_no_compen) & unitwage == 5 & age >= 15 & age <= 90]
      
      if(nrow(dat) == 0) {
        message("    No observations remain after filtering in this file.")
        next
      }
      
      # Handle zeros and remove outliers using IQR method
      dat <- dat[wage_no_compen > 0]
      
      # Calculate IQR for wage
      q1 <- quantile(dat$wage_no_compen, 0.25)
      q3 <- quantile(dat$wage_no_compen, 0.75)
      iqr_val <- q3 - q1
      
      # Define upper bound (lower bound is already handled by wage > 0)
      upper_bound <- q3 + 3 * iqr_val
      
      # Filter out extreme outliers
      dat <- dat[wage_no_compen <= upper_bound]
      
      # Process by year
      years <- unique(dat$year)
      
      for(yr in years) {
        year_data <- dat[year == yr]
        n_obs <- nrow(year_data)
        
        if(n_obs < 100) {
          message(paste("    Skipping year", yr, "due to insufficient data (n =", n_obs, ")"))
          next
        }
        
        # Calculate Myers Index
        myers_result <- calculate_traditional_myers(year_data$age)
        
        # Test Benford's Law for first 4 digits on individual wages
        benford_result <- test_benford_law(year_data$wage_no_compen, num_digits = 4)
        
        # Add to results
        results <- rbind(results,
                         data.frame(countrycode = country,
                                    year = yr,
                                    n_observations = n_obs,
                                    traditional_myers = myers_result$traditional,
                                    standardized_myers = myers_result$standardized,
                                    benford_correlation = benford_result$correlation,
                                    benford_abs_distance = benford_result$abs_distance,
                                    benford_chi_square = benford_result$chi_square,
                                    benford_mad = benford_result$mad,
                                    benford_chi_square_pvalue = benford_result$chi_square_pvalue,
                                    stringsAsFactors = FALSE))
      }
      
      # Clear data to free memory
      rm(dat)
      gc()
      
    }, error = function(e) {
      message("    Error processing file: ", e$message)
    })
  }
}

##############################################################
# PART III: DATA PROCESSING AND OUTPUT CREATION
##############################################################

# Function for residualization
residualize_data <- function(data, outcome_var) {
  # Ensure data is a data frame
  data <- as.data.frame(data)
  
  # Check how many unique countries and years we have
  n_countries <- length(unique(data$countrycode))
  n_years <- length(unique(data$year))
  
  # Create appropriate formula based on available data
  if (n_countries > 1 && n_years > 1) {
    # Both country and year fixed effects
    formula_str <- paste(outcome_var, "~ factor(countrycode) + factor(year)")
  } else if (n_countries == 1 && n_years > 1) {
    # Only year fixed effects (single country)
    formula_str <- paste(outcome_var, "~ factor(year)")
  } else if (n_countries > 1 && n_years == 1) {
    # Only country fixed effects (single year)
    formula_str <- paste(outcome_var, "~ factor(countrycode)")
  } else {
    # No fixed effects possible (single country, single year)
    # Return centered data instead (subtract mean)
    message("Cannot create fixed effects with only one country and one year. Centering data instead.")
    return(data[[outcome_var]] - mean(data[[outcome_var]], na.rm = TRUE))
  }
  
  # Fit the model with the appropriate formula
  model_formula <- as.formula(formula_str)
  model <- lm(model_formula, data = data)
  
  # Extract residuals
  residuals <- residuals(model)
  
  return(residuals)
}

# Process Version 1 - Remove extreme Myers values
process_version1 <- function(results) {
  # Copy data
  results_v1 <- results
  
  # Calculate outlier threshold using IQR method
  q1 <- quantile(results_v1$standardized_myers, 0.25, na.rm = TRUE)
  q3 <- quantile(results_v1$standardized_myers, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  myers_threshold <- q3 + 1.5 * iqr
  
  # Print threshold for reference
  message("Myers Index outlier threshold: ", round(myers_threshold, 3))
  
  # Apply filter: keep only observations below the threshold
  original_count <- nrow(results_v1)
  results_v1 <- results_v1[results_v1$standardized_myers <= myers_threshold, ]
  removed_count <- original_count - nrow(results_v1)
  
  message(removed_count, " observations removed as Myers Index outliers (", 
          round(removed_count/original_count*100, 1), "% of data)")
  
  return(results_v1)
}

# Process Version 2 - Both remove outliers and residualize
process_version2 <- function(results) {
  # First remove outliers
  results_v2 <- process_version1(results)
  
  # Then residualize
  # List of variables to residualize
  vars_to_residualize <- c("standardized_myers", "benford_correlation", 
                           "benford_abs_distance", "benford_chi_square", "benford_mad")
  
  # Residualize each variable
  for (var in vars_to_residualize) {
    residual_name <- paste0("residual_", var)
    results_v2[[residual_name]] <- residualize_data(results_v2, var)
    message("Residualized variable: ", var)
  }
  
  return(results_v2)
}

# Create improved scatter plot function
create_improved_scatter_plot <- function(data, x_var, y_var, y_label, 
                                         correlation_value = NULL, weighted = FALSE) {
  
  # Get unique countries for color coding
  countries <- unique(data$countrycode)
  n_countries <- length(countries)
  
  # Generate a color palette with enough distinct colors
  if (n_countries <= 8) {
    country_colors <- brewer.pal(max(8, n_countries), "Set1")[1:n_countries]
  } else {
    country_colors <- colorRampPalette(brewer.pal(9, "Set1"))(n_countries)
  }
  
  # Create named vector of colors
  names(country_colors) <- countries
  
  # Create x and y labels based on variable names
  if (grepl("residual_", x_var)) {
    x_label <- "Standardized Myers Index (Residualized)"
  } else {
    x_label <- "Standardized Myers Index (0-1)"
  }
  
  if (grepl("residual_", y_var)) {
    y_label <- paste(y_label, "(Residualized)")
  }
  
  # Create base plot - using modern tidy evaluation instead of aes_string
  x_var_sym <- sym(x_var)
  y_var_sym <- sym(y_var)
  
  p <- ggplot(data, aes(x = !!x_var_sym, y = !!y_var_sym)) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = "black"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95"),
      
      # Enhanced legend styling
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      legend.margin = margin(6, 6, 6, 6),
      legend.box.margin = margin(0, 0, 0, 6),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.8, "cm"),
      
      plot.margin = margin(15, 15, 15, 15),
      
      # Remove title
      plot.title = element_blank()
    ) +
    labs(
      x = x_label,
      y = y_label,
      color = "Country"
    )
  
  # Add points (weighted or not)
  if (weighted) {
    p <- p + geom_point(aes(color = countrycode, size = n_observations), alpha = 0.8)
  } else {
    p <- p + geom_point(aes(color = countrycode), alpha = 0.8)
  }
  
  # Apply color scale
  p <- p + scale_color_manual(values = country_colors, 
                              labels = countries,
                              breaks = countries)
  
  # Add horizontal red line at y=0
  p <- p + geom_hline(yintercept = 0, color = "red")
  
  # Add regression lines WITH 95% CONFIDENCE BANDS
  if (weighted) {
    # Linear regression with gray confidence bands
    p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
                         linetype = "solid", color = "black", fill = "gray", alpha = 0.3,
                         aes(weight = n_observations))
    
    # LOWESS smoothing with more muted colors
    p <- p + geom_smooth(method = "loess", formula = y ~ x, se = TRUE, 
                         linetype = "dashed", color = "darkgreen", fill = "#E0F0E0", alpha = 0.3,
                         span = 0.75, aes(weight = n_observations))
  } else {
    # Linear regression with gray confidence bands
    p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
                         linetype = "solid", color = "black", fill = "gray", alpha = 0.3)
    
    # LOWESS smoothing with more muted colors
    p <- p + geom_smooth(method = "loess", formula = y ~ x, se = TRUE, 
                         linetype = "dashed", color = "darkgreen", fill = "#E0F0E0", alpha = 0.3,
                         span = 0.75)
  }
  
  # Add correlation value
  if (!is.null(correlation_value) && !is.na(correlation_value)) {
    p <- p + annotate(
      "text", 
      x = max(data[[x_var]], na.rm = TRUE) - 0.05 * diff(range(data[[x_var]], na.rm = TRUE)),
      y = min(data[[y_var]], na.rm = TRUE) + 0.05 * diff(range(data[[y_var]], na.rm = TRUE)),
      label = paste("Correlation:", round(correlation_value, 3)),
      hjust = 1,
      fontface = "bold"
    )
  }
  
  # Add legend for weighted plots
  if (weighted) {
    p <- p + labs(size = "Number of observations") +
      guides(
        color = guide_legend(order = 1, override.aes = list(size = 3), title.position = "top"),
        size = guide_legend(order = 2, title.position = "top")
      )
  } else {
    p <- p + guides(color = guide_legend(title.position = "top"))
  }
  
  # Add regression line legend with simpler terms
  p <- p + annotate(
    "text",
    x = max(data[[x_var]], na.rm = TRUE) - 0.05 * diff(range(data[[x_var]], na.rm = TRUE)),
    y = min(data[[y_var]], na.rm = TRUE) + 0.10 * diff(range(data[[y_var]], na.rm = TRUE)),
    label = "Solid line: Linear regression, Dashed line: LOWESS (both with 95% CI)",
    hjust = 1,
    size = 3
  )
  
  # Handle case with many countries
  if (n_countries > 15) {
    p <- p + guides(color = guide_legend(ncol = 2))
  }
  
  return(p)
}

# Function to create correlation tables
create_correlation_tables <- function(data, is_residualized = TRUE) {
  # Define variables to use
  if (is_residualized) {
    myers_var <- "residual_standardized_myers"
    metrics <- list(
      list(var = "residual_benford_abs_distance", label = "Absolute Distance from Benford's Law"),
      list(var = "residual_benford_chi_square", label = "Chi-Square Statistic"),
      list(var = "residual_benford_mad", label = "Mean Absolute Deviation"),
      list(var = "residual_benford_correlation", label = "Correlation with Benford's Law")
    )
    vars_for_corr_matrix <- c("residual_standardized_myers", "residual_benford_correlation", 
                              "residual_benford_abs_distance", "residual_benford_chi_square", 
                              "residual_benford_mad")
  } else {
    myers_var <- "standardized_myers"
    metrics <- list(
      list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
      list(var = "benford_chi_square", label = "Chi-Square Statistic"),
      list(var = "benford_mad", label = "Mean Absolute Deviation"),
      list(var = "benford_correlation", label = "Correlation with Benford's Law")
    )
    vars_for_corr_matrix <- c("standardized_myers", "benford_correlation", 
                              "benford_abs_distance", "benford_chi_square", 
                              "benford_mad")
  }
  
  # Create main correlation matrix 
  correlations_measures <- cor(data[, vars_for_corr_matrix], 
                               use = "pairwise.complete.obs")
  
  # Create correlations by country
  country_correlations <- data.frame(
    countrycode = character(),
    stringsAsFactors = FALSE
  )
  
  # Add column names based on metrics
  for (metric in metrics) {
    short_name <- gsub("residual_", "", metric$var)
    short_name <- gsub("benford_", "", short_name)
    col_name <- paste0("corr_myers_", short_name)
    country_correlations[[col_name]] <- numeric()
  }
  
  # Calculate correlations for each country
  for(country in unique(data$countrycode)) {
    country_data <- data[data$countrycode == country, ]
    
    # Need at least 3 observations for correlation
    if(nrow(country_data) >= 3) {
      country_row <- data.frame(countrycode = country)
      
      for (metric in metrics) {
        short_name <- gsub("residual_", "", metric$var)
        short_name <- gsub("benford_", "", short_name)
        col_name <- paste0("corr_myers_", short_name)
        country_row[[col_name]] <- cor(country_data[[myers_var]], 
                                       country_data[[metric$var]], 
                                       use = "pairwise.complete.obs")
      }
      
      country_correlations <- rbind(country_correlations, country_row)
    }
  }
  
  return(list(
    correlations_matrix = correlations_measures,
    country_correlations = country_correlations
  ))
}

# Function to create explanation tables
create_explanations <- function(is_residualized = TRUE) {
  explanations <- data.frame(
    sheet = character(),
    column = character(),
    description = character(),
    stringsAsFactors = FALSE
  )
  
  # Add explanations for base columns
  base_columns <- c(
    "countrycode", "Country code",
    "year", "Year of the survey",
    "n_observations", "Number of individual observations used in calculations",
    "traditional_myers", "Traditional Myers Blended Index (0-90 scale) - measures digit preference in age reporting",
    "standardized_myers", "Standardized Myers Index (0-1 scale) - normalized version of the Myers Index",
    "benford_correlation", "Correlation between observed first 4 digits frequencies and Benford's Law expected frequencies",
    "benford_abs_distance", "Sum of absolute differences between observed and expected Benford frequencies for first 4 digits",
    "benford_chi_square", "Chi-square statistic for goodness of fit to Benford's Law for first 4 digits",
    "benford_mad", "Mean Absolute Deviation between observed and expected Benford frequencies for first 4 digits",
    "benford_chi_square_pvalue", "P-value for the Chi-square test against Benford's Law for first 4 digits"
  )
  
  # Add explanations for residualized columns if needed
  if (is_residualized) {
    residual_columns <- c(
      "residual_standardized_myers", "Standardized Myers Index after removing country and year fixed effects",
      "residual_benford_correlation", "Benford correlation after removing country and year fixed effects",
      "residual_benford_abs_distance", "Benford absolute distance after removing country and year fixed effects",
      "residual_benford_chi_square", "Benford chi-square after removing country and year fixed effects",
      "residual_benford_mad", "Benford MAD after removing country and year fixed effects"
    )
    
    base_columns <- c(base_columns, residual_columns)
  }
  
  for(i in seq(1, length(base_columns), by = 2)) {
    explanations <- rbind(explanations,
                          data.frame(
                            sheet = "Country-Year Results",
                            column = base_columns[i],
                            description = base_columns[i+1],
                            stringsAsFactors = FALSE
                          ))
  }
  
  # Add explanations for correlation matrix
  explanations <- rbind(explanations,
                        data.frame(
                          sheet = "Correlations Matrix",
                          column = "All columns",
                          description = "Correlation matrix between all indicators at country-year level",
                          stringsAsFactors = FALSE
                        ))
  
  # Add explanations for country correlations
  country_corr_columns <- c(
    "countrycode", "Country code"
  )
  
  # Add explanation for correlation columns
  if (is_residualized) {
    metrics <- c("abs_distance", "chi_square", "mad", "correlation")
    for (metric in metrics) {
      country_corr_columns <- c(
        country_corr_columns,
        paste0("corr_myers_", metric),
        paste0("Correlation between residualized Myers Index and residualized ", 
               gsub("_", " ", metric))
      )
    }
  } else {
    country_corr_columns <- c(
      country_corr_columns,
      "corr_myers_abs_distance", "Correlation between standardized Myers Index and absolute distance from Benford's Law",
      "corr_myers_chi_square", "Correlation between standardized Myers Index and chi-square statistic",
      "corr_myers_mad", "Correlation between standardized Myers Index and Mean Absolute Deviation",
      "corr_myers_correlation", "Correlation between standardized Myers Index and Benford correlation"
    )
  }
  
  for(i in seq(1, length(country_corr_columns), by = 2)) {
    explanations <- rbind(explanations,
                          data.frame(
                            sheet = "Country Correlations",
                            column = country_corr_columns[i],
                            description = country_corr_columns[i+1],
                            stringsAsFactors = FALSE
                          ))
  }
  
  return(explanations)
}

# Function to create plots for Version 1 (no residualization)
create_v1_plots <- function(data) {
  # For V1, we use non-residualized variables
  x_var <- "standardized_myers"
  metrics <- list(
    list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
    list(var = "benford_chi_square", label = "Chi-Square Statistic"),
    list(var = "benford_mad", label = "Mean Absolute Deviation"),
    list(var = "benford_correlation", label = "Correlation with Benford's Law")
  )
  
  # Create plots for each metric
  for (metric in metrics) {
    # Calculate correlation
    corr_val <- cor(data[[x_var]], data[[metric$var]], use = "pairwise.complete.obs")
    
    # Determine short filename for metric
    short_metric <- gsub("benford_", "", metric$var)
    
    # Create unweighted plot
    p_unweighted <- create_improved_scatter_plot(
      data = data,
      x_var = x_var,
      y_var = metric$var,
      y_label = metric$label,
      correlation_value = corr_val,
      weighted = FALSE
    )
    
    # Save plot with simplified naming convention
    plot_filename <- paste0("myers-", short_metric, "-4digits.png")
    
    ggsave(
      filename = file.path(output_path_v1, plot_filename),
      plot = p_unweighted,
      width = 10,
      height = 7,
      dpi = 300
    )
    
    # Create weighted plot
    p_weighted <- create_improved_scatter_plot(
      data = data,
      x_var = x_var,
      y_var = metric$var,
      y_label = paste(metric$label, "(Weighted)"),
      correlation_value = corr_val,
      weighted = TRUE
    )
    
    # Save weighted plot with simplified naming convention
    weighted_plot_filename <- paste0("myers-", short_metric, "-4digits-weighted.png")
    
    ggsave(
      filename = file.path(output_path_v1, weighted_plot_filename),
      plot = p_weighted,
      width = 10,
      height = 7,
      dpi = 300
    )
  }
}

# Function to create plots for Version 2 (residualized)
create_v2_plots <- function(data) {
  # For V2, we use residualized variables
  x_var <- "residual_standardized_myers"
  metrics <- list(
    list(var = "residual_benford_abs_distance", label = "Absolute Distance from Benford's Law"),
    list(var = "residual_benford_chi_square", label = "Chi-Square Statistic"),
    list(var = "residual_benford_mad", label = "Mean Absolute Deviation"),
    list(var = "residual_benford_correlation", label = "Correlation with Benford's Law")
  )
  
  # Create plots for each metric
  for (metric in metrics) {
    # Calculate correlation
    corr_val <- cor(data[[x_var]], data[[metric$var]], use = "pairwise.complete.obs")
    
    # Determine short filename for metric
    short_metric <- gsub("residual_", "", metric$var)
    short_metric <- gsub("benford_", "", short_metric)
    
    # Create unweighted plot
    p_unweighted <- create_improved_scatter_plot(
      data = data,
      x_var = x_var,
      y_var = metric$var,
      y_label = metric$label,
      correlation_value = corr_val,
      weighted = FALSE
    )
    
    # Save plot with simplified naming convention
    plot_filename <- paste0("myers-", short_metric, "-4digits.png")
    
    ggsave(
      filename = file.path(output_path_v2, plot_filename),
      plot = p_unweighted,
      width = 10,
      height = 7,
      dpi = 300
    )
    
    # Create weighted plot
    p_weighted <- create_improved_scatter_plot(
      data = data,
      x_var = x_var,
      y_var = metric$var,
      y_label = paste(metric$label, "(Weighted)"),
      correlation_value = corr_val,
      weighted = TRUE
    )
    
    # Save weighted plot with simplified naming convention
    weighted_plot_filename <- paste0("myers-", short_metric, "-4digits-weighted.png")
    
    ggsave(
      filename = file.path(output_path_v2, weighted_plot_filename),
      plot = p_weighted,
      width = 10,
      height = 7,
      dpi = 300
    )
  }
}

# Function to create a footnotes Excel file for the plots
create_plot_footnotes <- function() {
  footnotes <- data.frame(
    plot_name = character(),
    description = character(),
    x_axis = character(),
    y_axis = character(),
    stringsAsFactors = FALSE
  )
  
  # Add footnotes for each plot (V1 - No residualization)
  metrics <- c(
    "abs_distance" = "Absolute Distance from Benford's Law (first 4 digits)",
    "chi_square" = "Chi-Square Statistic (first 4 digits)", 
    "mad" = "Mean Absolute Deviation (first 4 digits)",
    "correlation" = "Correlation with Benford's Law (first 4 digits)"
  )
  
  for (metric_key in names(metrics)) {
    metric_label <- metrics[metric_key]
    
    # V1 - Unweighted plot
    plot_name <- paste0("V1/myers-", metric_key, "-4digits.png")
    footnotes <- rbind(footnotes,
                       data.frame(
                         plot_name = plot_name,
                         description = paste0("Relationship between standardized Myers Index and ", metric_label, 
                                              " based on individual wages. Each point represents a country-year observation. Unlike traditional Benford's Law analysis which focuses only on the first digit, this analysis considers the distribution of the first 4 digits, providing a more comprehensive assessment of data quality."),
                         x_axis = "Standardized Myers Index (0-1) - measures age heaping in survey responses",
                         y_axis = paste0(metric_label, " - calculated using individual wages"),
                         stringsAsFactors = FALSE
                       ))
    
    # V1 - Weighted plot
    weighted_plot_name <- paste0("V1/myers-", metric_key, "-4digits-weighted.png")
    footnotes <- rbind(footnotes,
                       data.frame(
                         plot_name = weighted_plot_name,
                         description = paste0("Relationship between standardized Myers Index and ", metric_label, 
                                              " based on individual wages. Each point represents a country-year observation, with point size proportional to the number of observations. The multi-digit Benford's Law analysis provides greater statistical power by examining the full distribution of the first 4 digits rather than only the first digit."),
                         x_axis = "Standardized Myers Index (0-1) - measures age heaping in survey responses",
                         y_axis = paste0(metric_label, " - calculated using individual wages"),
                         stringsAsFactors = FALSE
                       ))
    
    # V2 - Unweighted plot
    plot_name <- paste0("V2/myers-", metric_key, "-4digits.png")
    footnotes <- rbind(footnotes,
                       data.frame(
                         plot_name = plot_name,
                         description = paste0("Relationship between residualized standardized Myers Index and residualized ", metric_label, 
                                              " based on individual wages. Each point represents a country-year observation. The residualization process removes country and year fixed effects, isolating the relationship between age heaping and Benford's Law compliance beyond systematic country or time-period effects."),
                         x_axis = "Standardized Myers Index (Residualized) - measures age heaping after removing country and year fixed effects",
                         y_axis = paste0(metric_label, " (Residualized) - calculated using individual wages after removing country and year fixed effects"),
                         stringsAsFactors = FALSE
                       ))
    
    # V2 - Weighted plot
    weighted_plot_name <- paste0("V2/myers-", metric_key, "-4digits-weighted.png")
    footnotes <- rbind(footnotes,
                       data.frame(
                         plot_name = weighted_plot_name,
                         description = paste0("Relationship between residualized standardized Myers Index and residualized ", metric_label, 
                                              " based on individual wages. Each point represents a country-year observation, with point size proportional to the number of observations. The four-digit Benford analysis combined with residualization provides a robust methodology for identifying potential data quality issues while controlling for systematic patterns."),
                         x_axis = "Standardized Myers Index (Residualized) - measures age heaping after removing country and year fixed effects",
                         y_axis = paste0(metric_label, " (Residualized) - calculated using individual wages after removing country and year fixed effects"),
                         stringsAsFactors = FALSE
                       ))
  }
  
  return(footnotes)
}

# Main processing workflow
if (nrow(results) > 0) {
  if(test_mode) {
    message("*** TEST MODE: Processed ", nrow(results), " country-year observations from ARM and BOL ***")
    message("To run the full analysis, set test_mode = FALSE at the beginning of the script")
  } else {
    message("FULL MODE: Processed ", nrow(results), " country-year observations")
  }
  
  # Process V1 (remove outliers)
  message("Creating Version 1: removing extreme Myers values...")
  results_v1 <- process_version1(results)
  
  # Process V2 (both remove outliers and residualize)
  message("Creating Version 2: removing extreme Myers values and residualizing variables...")
  results_v2 <- process_version2(results)
  
  # Create correlation tables
  corr_tables_v1 <- create_correlation_tables(results_v1, is_residualized = FALSE)
  corr_tables_v2 <- create_correlation_tables(results_v2, is_residualized = TRUE)
  
  # Create explanations
  explanations_v1 <- create_explanations(is_residualized = FALSE)
  explanations_v2 <- create_explanations(is_residualized = TRUE)
  
  # Create plots
  create_v1_plots(results_v1)
  create_v2_plots(results_v2)
  
  # Create and save plot footnotes
  plot_footnotes <- create_plot_footnotes()
  footnotes_filename <- paste0("plot_footnotes.xlsx")
  write_xlsx(list("Plot Footnotes" = plot_footnotes), file.path(output_path_plots, footnotes_filename))
  
  # Create Excel filename with specific path and format
  excel_filename <- paste0(current_date, "-Myers-Bendford-multiple digits.xlsx")
  excel_path <- file.path(output_path_excel, excel_filename)
  
  # Prepare data for Excel export
  list_of_data <- list(
    "Country-Year Results" = results_v2,
    "Correlations Matrix V1" = as.data.frame(corr_tables_v1$correlations_matrix),
    "Correlations Matrix V2" = as.data.frame(corr_tables_v2$correlations_matrix),
    "Country Correlations V1" = corr_tables_v1$country_correlations,
    "Country Correlations V2" = corr_tables_v2$country_correlations,
    "Column Explanations" = explanations_v2
  )
  
  # Export to Excel
  write_xlsx(list_of_data, excel_path)
  
  mode_suffix <- if(test_mode) " (TEST MODE - ARM/BOL only)" else ""
  message("Analysis complete", mode_suffix, ". Excel results saved to ", excel_path)
  message("V1 plots saved to ", output_path_v1)
  message("V2 plots saved to ", output_path_v2)
  message("Plot footnotes saved to ", file.path(output_path_plots, footnotes_filename))
  
} else {
  message("No results to process. Check input data.")
}
