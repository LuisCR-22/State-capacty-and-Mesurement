/*====================================================================
Project:        Poverty and Inequality Indicators for Colombia
Author:         Created based on user specifications
Creation Date:  2025
====================================================================
PURPOSE: Calculate poverty rates (3 lines) and Gini coefficients
         at year-region-area level for Colombia 2008-2024
         
INDICATORS:
- Poverty rates at $3.00, $4.20, and $8.30 per day (2021 PPP)
- Gini coefficients
- Share of poor by region-area
- Gini decomposition by region-area
*=================================================================*/

clear all
set more off

**# ==============================================================================
**# 0. SETUP
**# ==============================================================================

* Define paths
global output_path "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\03_Data\Merge"

* Create output directory if it doesn't exist
cap mkdir "$output_path"

* Define poverty lines in 2021 PPP terms (daily to monthly conversion)
global pov_line_3_00_monthly = 3.00*(365/12)   // $3.00/day = ~$91.25/month
global pov_line_4_20_monthly = 4.20*(365/12)   // $4.20/day = ~$127.75/month
global pov_line_8_30_monthly = 8.30*(365/12)   // $8.30/day = ~$252.58/month

* Define years to analyze
local years "2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024"

* Create empty dataset to store results
clear
gen ano = .
gen region_number = .
gen urban_area = .
tempfile master_results
save `master_results', replace

noi di ""
noi di "=== COLOMBIA POVERTY AND INEQUALITY INDICATORS 2008-2024 ==="
noi di "=== Processing year-region-area level indicators ==="
noi di ""

**# ==============================================================================
**# 1. LOOP THROUGH EACH YEAR
**# ==============================================================================

foreach yr of local years {
    
    noi di "=== Processing year `yr' ==="
    
    * Try to load data for this year
    capture {
        datalib, country("COL") module(all) year(`yr') clear
    }
    
    * Check if data loaded successfully
    if _rc != 0 {
        noi di "WARNING: Data not available for year `yr', skipping..."
        continue
    }
    
    * Check if required variables exist
    capture confirm variable ipcf_ppp21
    if _rc != 0 {
        noi di "WARNING: Variable ipcf_ppp21 not found for year `yr', skipping..."
        continue
    }
    
    capture confirm variable region_est2
    if _rc != 0 {
        noi di "WARNING: Variable region_est2 not found for year `yr', skipping..."
        continue
    }
    
    capture confirm variable urbano
    if _rc != 0 {
        noi di "WARNING: Variable urbano not found for year `yr', skipping..."
        continue
    }
    
    **# ==============================================================================
    **# 1.1 DATA PREPARATION
    **# ==============================================================================

    * Keep only coherent households
    keep if cohh == 1

    * Keep only observations with non-missing income and region
    keep if !missing(ipcf_ppp21) & !missing(region_est2) & !missing(urbano)

    * Verify pondera exists and keep only positive weights
    capture confirm variable pondera
    if _rc != 0 {
        noi di "ERROR: Variable pondera not found for year `yr', skipping..."
        continue
    }
    keep if pondera > 0 & !missing(pondera)

    * CHECK IF ANY OBSERVATIONS REMAIN AFTER FILTERING
    qui count
    if r(N) == 0 {
        noi di "WARNING: No valid observations after filtering for year `yr', skipping..."
        continue
    }

    * Rename variables for consistency
    rename urbano urban_area
    
    * Extract numeric region code from region_est2 string
    * Format is "5 - Antioquia", "8 - Atlántico", etc.
    * Extract first word (the number) and convert to numeric
    capture confirm string variable region_est2
    if _rc == 0 {
        * region_est2 is a string variable
        gen region_number = real(word(region_est2, 1))
        drop region_est2
    }
    else {
        * region_est2 is already numeric
        rename region_est2 region_number
    }
    
    * Ensure region_number is double type
    recast double region_number, force
    
    * Drop observations with missing region_number (if extraction failed)
    drop if missing(region_number)
    
    * Verify we still have observations
    qui count
    if r(N) == 0 {
        noi di "WARNING: No valid observations after extracting region codes for year `yr', skipping..."
        continue
    }

    * Now proceed with ano creation
    capture confirm variable ano
    if _rc != 0 {
        gen ano = `yr'
    }
    else {
        replace ano = `yr'
    }
    
    **# ==============================================================================
    **# 1.2 CREATE POVERTY STATUS VARIABLES
    **# ==============================================================================
    
    * Create poverty status for each line (1 = poor, 0 = non-poor)
    gen poor_3_00 = (ipcf_ppp21 <= $pov_line_3_00_monthly)
    gen poor_4_20 = (ipcf_ppp21 <= $pov_line_4_20_monthly)
    gen poor_8_30 = (ipcf_ppp21 <= $pov_line_8_30_monthly)
    
    **# ==============================================================================
    **# 1.3 CALCULATE COUNTRY-LEVEL INDICATORS (for shares and decomposition)
    **# ==============================================================================
    
    * Country-level poverty rates (weighted mean, converted to percentage)
    qui mean poor_3_00 [pw=pondera]
    local country_pov_3_00 = _b[poor_3_00] * 100
    
    qui mean poor_4_20 [pw=pondera]
    local country_pov_4_20 = _b[poor_4_20] * 100
    
    qui mean poor_8_30 [pw=pondera]
    local country_pov_8_30 = _b[poor_8_30] * 100
    
    * Country-level total poor population (weighted counts - calculated manually)
    gen temp_poor_3_00 = poor_3_00 * pondera
    gen temp_poor_4_20 = poor_4_20 * pondera
    gen temp_poor_8_30 = poor_8_30 * pondera
    
    qui sum temp_poor_3_00
    local country_poor_count_3_00 = r(sum)
    
    qui sum temp_poor_4_20
    local country_poor_count_4_20 = r(sum)
    
    qui sum temp_poor_8_30
    local country_poor_count_8_30 = r(sum)
    
    drop temp_poor_*
    
    * Country-level Gini coefficient using fastgini with pweights
    capture which fastgini
    if _rc != 0 {
        noi di "Installing fastgini package..."
        ssc install fastgini
    }
    
    qui fastgini ipcf_ppp21 [pw=pondera]
    local country_gini = r(gini)
    
    * Store total population for later use
    qui sum pondera
    local total_pop = r(sum)
    
    noi di "  Country-level poverty rate (8.30 line): " %5.2f `country_pov_8_30' "%"
    noi di "  Country-level Gini: " %5.3f `country_gini'
    
    **# ==============================================================================
    **# 1.4 CALCULATE REGION-AREA LEVEL INDICATORS
    **# ==============================================================================
    
    * Create temporary variables for calculations
    gen weighted_poor_3_00 = poor_3_00 * pondera
    gen weighted_poor_4_20 = poor_4_20 * pondera
    gen weighted_poor_8_30 = poor_8_30 * pondera
    
    * Collapse to region-area level
    preserve
    
    collapse (mean) pov_rate_3_00 = poor_3_00 ///
                    pov_rate_4_20 = poor_4_20 ///
                    pov_rate_8_30 = poor_8_30 ///
             (sum) weighted_poor_3_00 ///
                   weighted_poor_4_20 ///
                   weighted_poor_8_30 ///
                   weighted_pop = pondera ///
             [pw=pondera], ///
             by(ano region_number urban_area)
    
    * Convert poverty rates to percentages
    replace pov_rate_3_00 = pov_rate_3_00 * 100
    replace pov_rate_4_20 = pov_rate_4_20 * 100
    replace pov_rate_8_30 = pov_rate_8_30 * 100
    
    * Calculate shares of poor (proportion of country's poor in each region-area)
    gen share_pov_3_00 = weighted_poor_3_00 / `country_poor_count_3_00'
    gen share_pov_4_20 = weighted_poor_4_20 / `country_poor_count_4_20'
    gen share_pov_8_30 = weighted_poor_8_30 / `country_poor_count_8_30'
    
    * Handle case where there are no poor at a given poverty line
    replace share_pov_3_00 = 0 if `country_poor_count_3_00' == 0
    replace share_pov_4_20 = 0 if `country_poor_count_4_20' == 0
    replace share_pov_8_30 = 0 if `country_poor_count_8_30' == 0
    
    * Add country-level indicators (same for all observations in this year)
    gen country_pov_3_00 = `country_pov_3_00'
    gen country_pov_4_20 = `country_pov_4_20'
    gen country_pov_8_30 = `country_pov_8_30'
    gen country_gini = `country_gini'
    
    * Save temporary region-area results
    tempfile region_area_temp
    save `region_area_temp'
    
    restore
    
**# ==============================================================================
**# 1.5 CALCULATE GINI BY REGION-AREA
**# ==============================================================================

* Get list of unique region-area combinations
preserve
keep ano region_number urban_area
duplicates drop
sort ano region_number urban_area

local n_groups = _N

* Create temporary dataset to store gini values
gen gini = .
gen pop = .
tempfile gini_values
save `gini_values'

* Loop through each region-area to calculate Gini
forvalues i = 1/`n_groups' {
    use `gini_values', clear
    local yr = ano[`i']
    local reg = region_number[`i']
    local urb = urban_area[`i']
    
    * Load full data and filter to this region-area
    restore, preserve
    keep if ano == `yr' & region_number == `reg' & urban_area == `urb'
    
    * Calculate Gini for this region-area using pweights
    qui count
    if r(N) >= 2 {  // Need at least 2 observations for Gini
        capture qui fastgini ipcf_ppp21 [pw=pondera]
        if _rc == 0 {
            local gini_value = r(gini)
            local pop_value = r(sum_w)
        }
        else {
            local gini_value = .
            local pop_value = 0
        }
    }
    else {
        local gini_value = .
        local pop_value = 0
    }
    
    * Store the result
    use `gini_values', clear
    replace gini = `gini_value' in `i'
    replace pop = `pop_value' in `i'
    save `gini_values', replace
}

restore
    
    **# ==============================================================================
    **# 1.6 MERGE GINI VALUES AND CALCULATE DECOMPOSITION
    **# ==============================================================================
    
    * Load region-area results
    use `region_area_temp', clear
    
    * Merge with Gini values
    merge 1:1 ano region_number urban_area using `gini_values', nogen
    
    * Calculate Gini decomposition
    /*
    METHODOLOGY FOR GINI DECOMPOSITION:
    Using a population-weighted contribution approach where each region-area's
    contribution to total inequality is approximated by:
    
    gini_decomposition = (population_share) * (gini_region_area) / (country_gini)
    
    This represents the proportion of total national inequality attributable to
    inequality within that region-area, weighted by its population share.
    
    Interpretation: If a region-area has a high Gini and large population, it 
    contributes more to national inequality. Values sum to approximately 1.0.
    
    Note: This is a simplified within-group decomposition. A full Gini decomposition 
    would include between-group components using the Pyatt/Chen/Fei methodology.
    */
    
    gen gini_decomposition = .
    replace gini_decomposition = (pop / `total_pop') * (gini / `country_gini') if !missing(gini) & `country_gini' > 0
    
    **# ==============================================================================
    **# 1.7 ENSURE CORRECT VARIABLE TYPES AND CLEAN UP
    **# ==============================================================================
    
    * Ensure ano is double
    recast double ano, force
    
    * Ensure region_number is double
    recast double region_number, force
    
    * Ensure urban_area is double
    recast double urban_area, force
    
    * Drop temporary variables
    drop weighted_* pop
    
    * Sort for consistency
    sort ano region_number urban_area
    
    **# ==============================================================================
    **# 1.8 APPEND TO MASTER RESULTS
    **# ==============================================================================
    
    * Count observations for this year
    qui count
    local n_obs_year = r(N)
    
    * Append to master results file
    append using `master_results'
    save `master_results', replace
    
    noi di "  Completed: `n_obs_year' region-area combinations"
    noi di ""
}

**# ==============================================================================
**# 2. FINALIZE AND SAVE RESULTS
**# ==============================================================================

use `master_results', clear

* Drop the initial empty observation if it exists
drop if missing(ano)

* Check if we have any data
qui count
if r(N) == 0 {
    noi di "ERROR: No data was processed. Please check data availability."
    exit
}

* Sort the dataset
sort ano region_number urban_area

* Label variables
label variable ano "Year"
label variable region_number "Region number (numeric code)"
label variable urban_area "Urban area (1=urban, 0=rural)"
label variable pov_rate_3_00 "Poverty rate at $3.00/day (2021 PPP) (%)"
label variable pov_rate_4_20 "Poverty rate at $4.20/day (2021 PPP) (%)"
label variable pov_rate_8_30 "Poverty rate at $8.30/day (2021 PPP) (%)"
label variable gini "Gini coefficient (region-area level)"
label variable share_pov_3_00 "Share of country's poor at $3.00/day in this region-area"
label variable share_pov_4_20 "Share of country's poor at $4.20/day in this region-area"
label variable share_pov_8_30 "Share of country's poor at $8.30/day in this region-area"
label variable gini_decomposition "Contribution to total inequality (population-weighted)"
label variable country_pov_3_00 "Country-level poverty rate at $3.00/day (%)"
label variable country_pov_4_20 "Country-level poverty rate at $4.20/day (%)"
label variable country_pov_8_30 "Country-level poverty rate at $8.30/day (%)"
label variable country_gini "Country-level Gini coefficient"

* Display summary
noi di "=== FINAL DATASET SUMMARY ==="
qui count
noi di "Total observations: " r(N)
qui tab ano
noi di "Years included: " r(r)
qui tab region_number
noi di "Regions included: " r(r)
qui tab urban_area
noi di "Areas included: " r(r)

* Verify shares sum to approximately 1 for each year
noi di ""
noi di "=== VERIFICATION: Sum of poverty shares by year (should be ≈1.0) ==="
preserve
collapse (sum) share_pov_3_00 share_pov_4_20 share_pov_8_30, by(ano)
list ano share_pov_3_00 share_pov_4_20 share_pov_8_30, clean noobs
restore

* Verify gini decomposition sums to approximately 1 for each year
noi di ""
noi di "=== VERIFICATION: Sum of gini decomposition by year (should be ≈1.0) ==="
preserve
collapse (sum) gini_decomposition, by(ano)
list ano gini_decomposition, clean noobs
restore

* Save final dataset
save "$output_path/07_pov_gini_area_depto_datalib_2008-2024.dta", replace

noi di ""
noi di "=== ANALYSIS COMPLETED SUCCESSFULLY ==="
noi di "Dataset saved to: $output_path/07_pov_gini_area_depto_datalib_2008-2024.dta"
noi di ""

* Display sample of results
noi di "=== SAMPLE OF RESULTS (first 20 observations) ==="
list ano region_number urban_area pov_rate_8_30 gini country_pov_8_30 country_gini in 1/20, clean noobs abbreviate(12)