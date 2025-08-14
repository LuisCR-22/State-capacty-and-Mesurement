########################################
# Benford Wage Analysis - Modified Version V3 Only
########################################
# Author: Luis Castellanos Rodriguez
# Modified: 2025-04-01

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

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

# New output paths as specified
current_date <- format(Sys.Date(), "%Y-%m-%d")
output_path_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Out of Repo/Outputs/Excel"
output_path_plots <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/Out of Repo/Outputs/PNG/Myers vs. Bendford/2025-04-01"

# Create directories if they don't exist
dir.create(output_path_excel, showWarnings = FALSE, recursive = TRUE)
dir.create(output_path_plots, showWarnings = FALSE, recursive = TRUE)

# 2. Define the Benford expected probabilities for digits 1 to 9
benford_expected <- log10(1 + 1/(1:9))

# 3. Initialize global result data frame
results <- data.frame(countrycode = character(),
                      year = integer(),
                      n_observations = integer(),
                      traditional_myers = numeric(),
                      standardized_myers = numeric(),
                      benford_correlation = numeric(),
                      benford_abs_distance = numeric(),
                      benford_chi_square = numeric(),
                      benford_mae = numeric(),
                      kuiper_statistic = numeric(),
                      kuiper_pvalue = numeric(),
                      ks_statistic = numeric(),
                      ks_pvalue = numeric(),
                      stringsAsFactors = FALSE)

# 4. Define the PROPER Traditional Myers Blended Index function
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

# 5. Define improved Kuiper Test function
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

# 6. Get list of ALL .dta files in the input folder
all_files <- list.files(path = input_path, 
                        pattern = "\\.dta$",
                        full.names = TRUE,
                        recursive = TRUE)

if(length(all_files) == 0) {
  stop("No .dta files found in input folder")
}

# 7. Extract country codes from the filenames
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

message("Found ", length(countries), " unique countries: ", paste(countries, collapse = ", "))

# 8. Process files for each country
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
        
        # Extract first digit for Benford analysis
        year_data[, first_digit := as.numeric(substr(as.character(abs(wage_no_compen)), 1, 1))]
        year_data <- year_data[first_digit %in% 1:9]
        
        # Recalculate n after filtering first digits
        n_obs <- nrow(year_data)
        if(n_obs < 100) {
          message(paste("    Skipping year", yr, "due to insufficient data after first digit extraction (n =", n_obs, ")"))
          next
        }
        
        # Calculate Myers Index
        myers_result <- calculate_traditional_myers(year_data$age)
        
        # Calculate Benford metrics
        first_digits <- year_data$first_digit
        obs_table <- table(factor(first_digits, levels = 1:9))
        obs_freq <- as.numeric(obs_table) / sum(obs_table)
        
        # Improved metrics
        benford_correlation <- cor(obs_freq, benford_expected)
        benford_abs_distance <- sum(abs(obs_freq - benford_expected))
        benford_chi_square <- sum((obs_freq - benford_expected)^2 / benford_expected)
        benford_mae <- mean(abs(obs_freq - benford_expected))
        
        # Statistical tests
        kuiper_result <- kuiper_test(obs_freq, benford_expected)
        ks_result <- ks.test(obs_freq, benford_expected)
        
        # Add to results
        results <- rbind(results,
                         data.frame(countrycode = country,
                                    year = yr,
                                    n_observations = n_obs,
                                    traditional_myers = myers_result$traditional,
                                    standardized_myers = myers_result$standardized,
                                    benford_correlation = benford_correlation,
                                    benford_abs_distance = benford_abs_distance,
                                    benford_chi_square = benford_chi_square,
                                    benford_mae = benford_mae,
                                    kuiper_statistic = kuiper_result$statistic,
                                    kuiper_pvalue = kuiper_result$p.value,
                                    ks_statistic = ks_result$statistic,
                                    ks_pvalue = ks_result$p.value,
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

# 9. Create function for residualization
# This function removes country and year fixed effects
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
# We use a more rigorous method than just a 0.7 threshold:
# Calculate threshold as Q3 + 1.5*IQR (standard for outlier detection)
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

# Process Version 2 - Residualize variables
process_version2 <- function(results) {
  # Copy data
  results_v2 <- results
  
  # List of variables to residualize (all Benford metrics and Myers Index)
  vars_to_residualize <- c("standardized_myers", "benford_correlation", 
                           "benford_abs_distance", "benford_chi_square", "benford_mae")
  
  # Residualize each variable
  for (var in vars_to_residualize) {
    residual_name <- paste0("residual_", var)
    results_v2[[residual_name]] <- residualize_data(results_v2, var)
    message("Residualized variable: ", var)
  }
  
  return(results_v2)
}

# Process Version 3 - Both remove outliers and residualize
process_version3 <- function(results) {
  # First remove outliers
  results_v3 <- process_version1(results)
  
  # Then residualize
  results_v3 <- process_version2(results_v3)
  
  return(results_v3)
}

# 10. Create improved scatter plot function with country-based colors and clear labels
# MODIFIED: Added horizontal red line at y=0
# MODIFIED: Changed linear regression line to solid black with gray confidence bands
create_improved_scatter_plot <- function(data, x_var, y_var, y_label, 
                                         correlation_value = NULL, weighted = FALSE,
                                         title_prefix = "") {
  
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
  
  # Create title with appropriate prefix
  if (title_prefix != "") {
    title <- paste0(title_prefix, ": Relationship between Myers Index and ", y_label)
  } else {
    title <- paste0("Relationship between Myers Index and ", y_label)
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
      
      plot.margin = margin(15, 15, 15, 15)
    ) +
    labs(
      x = x_label,
      y = y_label,
      title = title,
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
  
  # Add regression lines WITH 95% CONFIDENCE BANDS (modified from original)
  if (weighted) {
    # Linear regression (black solid line) with gray confidence bands
    p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, 
                         linetype = "solid", color = "black", fill = "gray", alpha = 0.3,
                         aes(weight = n_observations))
    
    # LOWESS smoothing with more muted colors
    p <- p + geom_smooth(method = "loess", formula = y ~ x, se = TRUE, 
                         linetype = "dashed", color = "darkgreen", fill = "#E0F0E0", alpha = 0.3,
                         span = 0.75, aes(weight = n_observations))
  } else {
    # Linear regression (black solid line) with gray confidence bands
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
  
  # Add correlation line legend
  p <- p + annotate(
    "text",
    x = max(data[[x_var]], na.rm = TRUE) - 0.05 * diff(range(data[[x_var]], na.rm = TRUE)),
    y = min(data[[y_var]], na.rm = TRUE) + 0.10 * diff(range(data[[y_var]], na.rm = TRUE)),
    label = "Black line: Linear (with 95% CI), Green line: LOWESS (with 95% CI)",
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
      list(var = "residual_benford_mae", label = "Mean Absolute Error"),
      list(var = "residual_benford_correlation", label = "Correlation with Benford's Law")
    )
    vars_for_corr_matrix <- c("residual_standardized_myers", "residual_benford_correlation", 
                              "residual_benford_abs_distance", "residual_benford_chi_square", 
                              "residual_benford_mae")
  } else {
    myers_var <- "standardized_myers"
    metrics <- list(
      list(var = "benford_abs_distance", label = "Absolute Distance from Benford's Law"),
      list(var = "benford_chi_square", label = "Chi-Square Statistic"),
      list(var = "benford_mae", label = "Mean Absolute Error"),
      list(var = "benford_correlation", label = "Correlation with Benford's Law")
    )
    vars_for_corr_matrix <- c("standardized_myers", "benford_correlation", 
                              "benford_abs_distance", "benford_chi_square", 
                              "benford_mae")
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
    "n_observations", "Number of observations used in calculations",
    "traditional_myers", "Traditional Myers Blended Index (0-90 scale) - measures digit preference in age reporting",
    "standardized_myers", "Standardized Myers Index (0-1 scale) - normalized version of the Myers Index",
    "benford_correlation", "Correlation between observed first digit frequencies and Benford's Law expected frequencies",
    "benford_abs_distance", "Sum of absolute differences between observed and expected Benford frequencies",
    "benford_chi_square", "Chi-square statistic for goodness of fit to Benford's Law",
    "benford_mae", "Mean Absolute Error between observed and expected Benford frequencies",
    "kuiper_statistic", "Kuiper test statistic - measures maximum deviation in cumulative distributions",
    "kuiper_pvalue", "P-value for the Kuiper test",
    "ks_statistic", "Kolmogorov-Smirnov test statistic",
    "ks_pvalue", "P-value for the Kolmogorov-Smirnov test"
  )
  
  # Add explanations for residualized columns if needed
  if (is_residualized) {
    residual_columns <- c(
      "residual_standardized_myers", "Standardized Myers Index after removing country and year fixed effects",
      "residual_benford_correlation", "Benford correlation after removing country and year fixed effects",
      "residual_benford_abs_distance", "Benford absolute distance after removing country and year fixed effects",
      "residual_benford_chi_square", "Benford chi-square after removing country and year fixed effects",
      "residual_benford_mae", "Benford MAE after removing country and year fixed effects"
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
    metrics <- c("abs_distance", "chi_square", "mae", "correlation")
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
      "corr_myers_mae", "Correlation between standardized Myers Index and Mean Absolute Error",
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

# Function to create plots for V3 version
create_v3_plots <- function(data) {
  # For V3, we use residualized variables
  x_var <- "residual_standardized_myers"
  metrics <- list(
    list(var = "residual_benford_abs_distance", label = "Absolute Distance from Benford's Law"),
    list(var = "residual_benford_chi_square", label = "Chi-Square Statistic"),
    list(var = "residual_benford_mae", label = "Mean Absolute Error"),
    list(var = "residual_benford_correlation", label = "Correlation with Benford's Law")
  )
  
  # Use V3 title
  title_prefix <- "Residualized Variables, No Extreme Myers Values"
  
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
      weighted = FALSE,
      title_prefix = title_prefix
    )
    
    # Save plot with new naming convention
    plot_filename <- paste0(current_date, "-myers-", short_metric, "-V3.png")
    
    ggsave(
      filename = file.path(output_path_plots, plot_filename),
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
      weighted = TRUE,
      title_prefix = title_prefix
    )
    
    # Save weighted plot with new naming convention
    weighted_plot_filename <- paste0(current_date, "-myers-", short_metric, "-weighted-V3.png")
    
    ggsave(
      filename = file.path(output_path_plots, weighted_plot_filename),
      plot = p_weighted,
      width = 10,
      height = 7,
      dpi = 300
    )
  }
}

# Main processing workflow - ONLY generate V3 (combining both approaches)
if (nrow(results) > 0) {
  # Process V3 (both remove outliers and residualize)
  message("Creating Version 3: removing extreme Myers values and residualizing variables...")
  results_v3 <- process_version3(results)
  
  # Create correlation tables for V3
  corr_tables <- create_correlation_tables(results_v3, is_residualized = TRUE)
  
  # Create explanations for V3
  explanations <- create_explanations(is_residualized = TRUE)
  
  # Create plots for V3
  create_v3_plots(results_v3)
  
  # Create Excel filename with specific path and format
  excel_filename <- paste0(current_date, "-Myers_Bendford-V3.xlsx")
  excel_path <- file.path(output_path_excel, excel_filename)
  
  # Prepare data for Excel export
  list_of_data <- list(
    "Country-Year Results" = results_v3,
    "Correlations Matrix" = as.data.frame(corr_tables$correlations_matrix),
    "Country Correlations" = corr_tables$country_correlations,
    "Column Explanations" = explanations
  )
  
  # Export to Excel
  write_xlsx(list_of_data, excel_path)
  
  message("Version 3 analysis complete. Excel results saved to ", excel_path)
  message("Version 3 plots saved to ", output_path_plots)
  
} else {
  message("No results to process. Check input data.")
}