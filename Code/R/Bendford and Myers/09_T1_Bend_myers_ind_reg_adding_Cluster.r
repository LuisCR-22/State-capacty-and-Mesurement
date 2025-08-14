########################################
# Benford Wage Analysis - Enhanced Version 2.2
########################################
# Author: Luis Castellanos Rodriguez
# Modified: 2025-08-12
# Simplified version focusing on two analysis approaches with clustered standard errors
# V1: Raw data, V2: Extreme values excluded
# Both versions run with and without fixed effects using country-level clustered errors

# 0. Clean up workspace and load libraries
rm(list = ls())
gc()

########################################
# CONFIGURATION PARAMETERS
########################################

# Test mode: Set to TRUE to process only ARM and BOL files
TEST_MODE <- FALSE  # Change to FALSE to process all countries

# Install and load required packages
packages <- c("haven", "writexl", "dplyr", "data.table", "tidyverse", 
              "broom", "sandwich", "lmtest")
installed <- sapply(packages, require, character.only = TRUE)
if(any(!installed)) {
  install.packages(packages[!installed])
  sapply(packages[!installed], require, character.only = TRUE)
}

########################################
# FILE PATHS CONFIGURATION
########################################

# Input path for GLD harmonized data
input_path <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Shared/FY2025/Technology and Inequality project/Raw data/GLD harmonization"

# Output path for Excel files
output_path_excel <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/04_Outputs/Excel"

# Create directories if they don't exist
dir.create(output_path_excel, showWarnings = FALSE, recursive = TRUE)

########################################
# STATISTICAL CONSTANTS AND FUNCTIONS
########################################

# Define the Benford expected probabilities for digits 1 to 9
benford_expected <- log10(1 + 1/(1:9))

# Initialize global result data frame
results <- data.frame(countrycode = character(),
                      year = integer(),
                      n_observations_myers = integer(),
                      n_observations_benford_with_zeros = integer(),
                      n_observations_benford_no_zeros = integer(),
                      traditional_myers = numeric(),
                      standardized_myers = numeric(),
                      benford_abs_distance_with_zeros = numeric(),
                      benford_chi_square_with_zeros = numeric(),
                      benford_abs_distance_no_zeros = numeric(),
                      benford_chi_square_no_zeros = numeric(),
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
# BENFORD'S LAW ANALYSIS (TWO VERSIONS)
########################################

calculate_benford_metrics <- function(wage_data, include_zeros = TRUE) {
  # Filter wages based on include_zeros parameter
  if (include_zeros) {
    # Keep all non-negative wages
    clean_wages <- wage_data[wage_data >= 0]
    # For zeros, assign first digit as NA (will be excluded from Benford analysis)
    first_digits <- ifelse(clean_wages == 0, NA, as.numeric(substr(as.character(abs(clean_wages)), 1, 1)))
  } else {
    # Remove zeros and negatives
    clean_wages <- wage_data[wage_data > 0]
    # Extract first digit
    first_digits <- as.numeric(substr(as.character(abs(clean_wages)), 1, 1))
  }
  
  # Filter to valid first digits (1-9)
  first_digits <- first_digits[first_digits %in% 1:9]
  
  if(length(first_digits) < 100) {
    return(list(abs_distance = NA, chi_square = NA, n_obs = length(clean_wages)))
  }
  
  # Calculate observed frequencies
  obs_table <- table(factor(first_digits, levels = 1:9))
  obs_freq <- as.numeric(obs_table) / sum(obs_table)
  
  # Calculate metrics
  benford_abs_distance <- sum(abs(obs_freq - benford_expected))
  benford_chi_square <- sum((obs_freq - benford_expected)^2 / benford_expected)
  
  return(list(abs_distance = benford_abs_distance, 
              chi_square = benford_chi_square, 
              n_obs = length(clean_wages)))
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

remove_wage_outliers <- function(wage_data, include_zeros = TRUE) {
  if (include_zeros) {
    # Keep zeros and positive values
    clean_wages <- wage_data[wage_data >= 0]
  } else {
    # Remove zeros and negatives
    clean_wages <- wage_data[wage_data > 0]
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
# VERSION PROCESSING FUNCTIONS
########################################

# V1: Raw data (no outlier removal, use Benford with zeros)
process_version1 <- function(results) {
  results_v1 <- results
  
  # Use Benford metrics with zeros (keeping all data)
  results_v1$benford_abs_distance <- results_v1$benford_abs_distance_with_zeros
  results_v1$benford_chi_square <- results_v1$benford_chi_square_with_zeros
  results_v1$n_observations_benford <- results_v1$n_observations_benford_with_zeros
  
  message("Version 1 processed: Raw data with ", nrow(results_v1), " observations")
  return(results_v1)
}

# V2: Remove extreme Myers values (use Benford without zeros)
process_version2 <- function(results) {
  results_v2 <- results
  
  # Use Benford metrics without zeros (consistent with outlier removal approach)
  results_v2$benford_abs_distance <- results_v2$benford_abs_distance_no_zeros
  results_v2$benford_chi_square <- results_v2$benford_chi_square_no_zeros
  results_v2$n_observations_benford <- results_v2$n_observations_benford_no_zeros
  
  # Calculate outlier threshold using IQR method for Myers Index
  q1 <- quantile(results_v2$standardized_myers, 0.25, na.rm = TRUE)
  q3 <- quantile(results_v2$standardized_myers, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  myers_threshold <- q3 + 1.5 * iqr
  
  message("Myers Index outlier threshold: ", round(myers_threshold, 3))
  
  # Apply filter to remove extreme Myers values
  original_count <- nrow(results_v2)
  results_v2 <- results_v2[results_v2$standardized_myers <= myers_threshold, ]
  removed_count <- original_count - nrow(results_v2)
  
  message("Version 2 processed: ", removed_count, " observations removed as Myers Index outliers (", 
          round(removed_count/original_count*100, 1), "% of data). Final N = ", nrow(results_v2))
  
  return(results_v2)
}

########################################
# REGRESSION ANALYSIS WITH CLUSTERED STANDARD ERRORS
########################################

run_regression_analysis_clustered <- function(data, version_name) {
  # Define the dependent and independent variables
  myers_var <- "standardized_myers"
  benford_var <- "benford_abs_distance"
  
  # Prepare regression data with complete cases
  reg_data <- data[complete.cases(data[, c(myers_var, benford_var, "countrycode", "year")]), ]
  
  # Check if we have sufficient data
  if (nrow(reg_data) < 10) {
    message("Insufficient data for regression analysis in ", version_name)
    return(list(
      coefficients_no_fe = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE),
      coefficients_fe = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE),
      model_stats_no_fe = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE),
      model_stats_fe = data.frame(Model = "Insufficient data", stringsAsFactors = FALSE)
    ))
  }
  
  # Define regression specifications
  reg_specs <- list(
    "Model 1: Bivariate" = paste(benford_var, "~", myers_var),
    "Model 2: + Urban" = paste(benford_var, "~", myers_var, "+ urban_share"),
    "Model 3: + Male" = paste(benford_var, "~", myers_var, "+ urban_share + male_share"),
    "Model 4: + Literacy" = paste(benford_var, "~", myers_var, "+ urban_share + male_share + literacy_share"),
    "Model 5: + Unemployment" = paste(benford_var, "~", myers_var, "+ urban_share + male_share + literacy_share + unemployment_share")
  )
  
  # Function to run regression with clustered standard errors
  run_clustered_regression <- function(formula_str, data, include_fe = FALSE) {
    tryCatch({
      # Add fixed effects if requested
      if (include_fe && length(unique(data$countrycode)) > 1 && length(unique(data$year)) > 1) {
        formula_str <- paste(formula_str, "+ factor(countrycode) + factor(year)")
      }
      
      # Fit the model
      model <- lm(as.formula(formula_str), data = data)
      
      # Calculate clustered standard errors (clustered by country)
      clustered_vcov <- vcovCL(model, cluster = data$countrycode)
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
      # Return proper structure even on error
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
    
    # Improved error checking
    if (!is.null(result) && !is.null(result$coefficients) && nrow(result$coefficients) > 0) {
      # Check if this is an error result
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
  
  if (length(unique(reg_data$countrycode)) > 1 && length(unique(reg_data$year)) > 1) {
    for (i in seq_along(reg_specs)) {
      model_name <- names(reg_specs)[i]
      formula_str <- reg_specs[[i]]
      
      result <- run_clustered_regression(formula_str, reg_data, include_fe = TRUE)
      
      # Improved error checking
      if (!is.null(result) && !is.null(result$coefficients) && nrow(result$coefficients) > 0) {
        # Check if this is an error result
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
    message("Insufficient variation for fixed effects (need multiple countries AND years)")
  }
  
  return(list(
    coefficients_no_fe = results_no_fe,
    model_stats_no_fe = model_stats_no_fe,
    coefficients_fe = results_fe,
    model_stats_fe = model_stats_fe
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
  stop("No .dta files found in input folder: ", input_path)
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
    
    # Filter data for age range only (keep all employment statuses for control variables)
    dat <- dat[age >= 10 & age <= 79]
    
    if(nrow(dat) == 0) {
      message("  No observations after age filtering.")
      next
    }
    
    # Process by year
    years <- unique(dat$year)
    
    for(yr in years) {
      year_data_full <- dat[year == yr]  # Keep full data for control variables and Myers
      
      # Filter for wage analysis (monthly wages, non-missing wage data)
      year_data_wages <- year_data_full[!is.na(wage_no_compen) & unitwage == 5]
      
      if(nrow(year_data_wages) < 100) {
        message("  Skipping year ", yr, " due to insufficient wage data (n = ", nrow(year_data_wages), ")")
        next
      }
      
      # Calculate Myers Index on ALL people in age range (traditional approach)
      myers_result <- calculate_traditional_myers(year_data_full$age)
      n_myers <- sum(year_data_full$age >= 10 & year_data_full$age <= 79, na.rm = TRUE)
      
      # Calculate Benford metrics - TWO VERSIONS
      # Version 1: Include zeros
      wages_with_zeros <- remove_wage_outliers(year_data_wages$wage_no_compen, include_zeros = TRUE)
      benford_with_zeros <- calculate_benford_metrics(wages_with_zeros, include_zeros = TRUE)
      
      # Version 2: Exclude zeros  
      wages_no_zeros <- remove_wage_outliers(year_data_wages$wage_no_compen, include_zeros = FALSE)
      benford_no_zeros <- calculate_benford_metrics(wages_no_zeros, include_zeros = FALSE)
      
      # Calculate control variables on FULL dataset (includes unemployed people)
      controls <- calculate_control_variables(year_data_full)
      
      # Get country code
      country <- unique(year_data_wages$countrycode)[1]
      
      # Add to results
      new_row <- data.frame(
        countrycode = country,
        year = yr,
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
        male_share = controls$male_share,
        literacy_share = controls$literacy_share,
        unemployment_share = controls$unemployment_share,
        stringsAsFactors = FALSE
      )
      
      results <- rbind(results, new_row)
      
      message("  Added year ", yr, " for country ", country, 
              " (Myers n=", n_myers, ", Benford w/zeros n=", benford_with_zeros$n_obs, 
              ", Benford no zeros n=", benford_no_zeros$n_obs, ")")
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
# PROCESS VERSIONS AND CREATE OUTPUTS
########################################

if (nrow(results) > 0) {
  
  # Continue with the analysis using your existing results data
  message("Processing Version 1 (V1): Raw data")
  results_v1 <- process_version1(results)
  
  message("Processing Version 2 (V2): Extreme values excluded")
  results_v2 <- process_version2(results)
  
  # Run regression analyses for both versions
  message("Running regression analysis for V1...")
  reg_results_v1 <- run_regression_analysis_clustered(results_v1, "V1")
  
  message("Running regression analysis for V2...")
  reg_results_v2 <- run_regression_analysis_clustered(results_v2, "V2")
  
  # Create Excel outputs for V1
  message("Creating Excel output for V1...")
  excel_data_v1 <- list(
    "V1_Results_Data" = results_v1,
    "V1_Coefficients_No_FE" = reg_results_v1$coefficients_no_fe,
    "V1_Model_Stats_No_FE" = reg_results_v1$model_stats_no_fe,
    "V1_Coefficients_FE" = reg_results_v1$coefficients_fe,
    "V1_Model_Stats_FE" = reg_results_v1$model_stats_fe
  )
  
  write_xlsx(excel_data_v1, file.path(output_path_excel, "03_Myers_Benford_Ind_V01_Cluster.xlsx"))
  message("V1 Excel file saved successfully")
  
  # Create Excel outputs for V2
  message("Creating Excel output for V2...")
  excel_data_v2 <- list(
    "V2_Results_Data" = results_v2,
    "V2_Coefficients_No_FE" = reg_results_v2$coefficients_no_fe,
    "V2_Model_Stats_No_FE" = reg_results_v2$model_stats_no_fe,
    "V2_Coefficients_FE" = reg_results_v2$coefficients_fe,
    "V2_Model_Stats_FE" = reg_results_v2$model_stats_fe
  )
  
  write_xlsx(excel_data_v2, file.path(output_path_excel, "03_Myers_Benford_Ind_V02_Cluster.xlsx"))
  message("V2 Excel file saved successfully")
  
  # Print summary statistics
  message("\n", paste(rep("=", 60), collapse=""))
  message("ANALYSIS COMPLETED SUCCESSFULLY!")
  message(paste(rep("=", 60), collapse=""))
  message("V1 (Raw data): ", nrow(results_v1), " observations")
  message("V2 (Outliers excluded): ", nrow(results_v2), " observations")
  message("Output files saved to: ", output_path_excel)
  message("- 03_Myers_Benford_Ind_V01_Cluster.xlsx")
  message("- 03_Myers_Benford_Ind_V02_Cluster.xlsx")
  message(paste(rep("=", 60), collapse=""))
  
} else {
  stop("No valid results generated. Check input data and file paths.")
}