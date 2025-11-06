/*====================================================================
Project:        Merge Myers-Benford Analysis with Poverty Indicators
====================================================================
PURPOSE: Merge the V01 Myers-Benford dataset (master) with poverty 
         indicators dataset (using) at year-region-area level
         
MERGE KEYS: ano, region_number, urban_area

OUTPUT: 07_V01_my_Bend_CSI_NTL_pov_depto_area.dta
*=================================================================*/

clear all
set more off

**# ==============================================================================
**# 0. DEFINE PATHS
**# ==============================================================================

global data_path "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\03_Data\Merge"

* Input files
global master_file "$data_path\05_V01_Myers_Bend_CSI_NTL_depto_area.dta"
global using_file "$data_path\07_pov_gini_area_depto_datalib_2008-2024.dta"

* Output file
global output_file "$data_path\08_V01_my_Bend_CSI_NTL_pov_depto_area.dta"

noi di ""
noi di "=== MERGING MYERS-BENFORD WITH POVERTY INDICATORS ==="
noi di ""

**# ==============================================================================
**# 1. LOAD AND INSPECT MASTER DATASET
**# ==============================================================================

noi di "=== STEP 1: Loading Master Dataset (Myers-Benford) ==="
use "$master_file", clear

* Check if merge keys exist
foreach var in ano region_number urban_area {
    capture confirm variable `var'
    if _rc != 0 {
        noi di "ERROR: Variable `var' not found in master dataset!"
        exit 111
    }
}

* Store master dataset statistics
qui count
local master_obs = r(N)

qui tab ano
local master_years = r(r)

qui tab region_number
local master_regions = r(r)

qui tab urban_area
local master_areas = r(r)

* Get year range
qui sum ano
local master_year_min = r(min)
local master_year_max = r(max)

noi di "Master dataset loaded successfully:"
noi di "  - Total observations: " %9.0fc `master_obs'
noi di "  - Years: " `master_years' " (range: `master_year_min'-`master_year_max')"
noi di "  - Regions: " `master_regions'
noi di "  - Areas: " `master_areas' " (0=rural, 1=urban)"
noi di ""

* Check for duplicates in master on merge keys
duplicates report ano region_number urban_area
qui duplicates tag ano region_number urban_area, gen(dup_master)
qui count if dup_master > 0
if r(N) > 0 {
    noi di "WARNING: Master dataset has " r(N) " duplicate observations on merge keys!"
    noi di "  First 10 duplicates:"
    list ano region_number urban_area if dup_master > 0 in 1/10, clean noobs
    noi di ""
}
else {
    noi di "✓ No duplicates found in master dataset on merge keys"
    noi di ""
}
drop dup_master

**# ==============================================================================
**# 2. INSPECT USING DATASET (POVERTY DATA)
**# ==============================================================================

noi di "=== STEP 2: Inspecting Using Dataset (Poverty Indicators) ==="

preserve
use "$using_file", clear

* Check if merge keys exist and match names
foreach var in ano region_number urban_area {
    capture confirm variable `var'
    if _rc != 0 {
        noi di "ERROR: Variable `var' not found in using dataset!"
        exit 111
    }
}

* Store using dataset statistics
qui count
local using_obs = r(N)

qui tab ano
local using_years = r(r)

qui tab region_number
local using_regions = r(r)

qui tab urban_area
local using_areas = r(r)

* Get year range
qui sum ano
local using_year_min = r(min)
local using_year_max = r(max)

noi di "Using dataset (poverty) loaded successfully:"
noi di "  - Total observations: " %9.0fc `using_obs'
noi di "  - Years: " `using_years' " (range: `using_year_min'-`using_year_max')"
noi di "  - Regions: " `using_regions'
noi di "  - Areas: " `using_areas' " (0=rural, 1=urban)"
noi di ""

* Check for duplicates in using on merge keys
duplicates report ano region_number urban_area
qui duplicates tag ano region_number urban_area, gen(dup_using)
qui count if dup_using > 0
if r(N) > 0 {
    noi di "WARNING: Using dataset has " r(N) " duplicate observations on merge keys!"
    noi di "  First 10 duplicates:"
    list ano region_number urban_area if dup_using > 0 in 1/10, clean noobs
    noi di ""
}
else {
    noi di "✓ No duplicates found in using dataset on merge keys"
    noi di ""
}

* List poverty variables that will be merged
noi di "Poverty variables to be merged:"
describe pov_rate_3_00 pov_rate_4_20 pov_rate_8_30 ///
         share_pov_3_00 share_pov_4_20 share_pov_8_30 ///
         country_pov_3_00 country_pov_4_20 country_pov_8_30, simple
noi di ""

restore

**# ==============================================================================
**# 3. PERFORM THE MERGE
**# ==============================================================================

noi di "=== STEP 3: Performing the Merge ==="
noi di "Merge type: Left join (keeping all observations from master)"
noi di "Merge keys: ano, region_number, urban_area"
noi di ""

* Perform merge
merge m:1 ano region_number urban_area using "$using_file", ///
    generate(_merge_poverty)

**# ==============================================================================
**# 4. ANALYZE MERGE RESULTS
**# ==============================================================================

noi di "=== STEP 4: Merge Results ==="
noi di ""

* Overall merge statistics
tab _merge_poverty

noi di ""
noi di "Detailed merge statistics:"

* Count by merge result
qui count if _merge_poverty == 1
local not_matched_master = r(N)

qui count if _merge_poverty == 2
local not_matched_using = r(N)

qui count if _merge_poverty == 3
local matched = r(N)

qui count
local total_final = r(N)

noi di "  - Master only (not matched):        " %9.0fc `not_matched_master' " (" %5.2f `not_matched_master'/`master_obs'*100 "%)"
noi di "  - Using only (not matched):         " %9.0fc `not_matched_using'
noi di "  - Matched (poverty data added):     " %9.0fc `matched' " (" %5.2f `matched'/`master_obs'*100 "%)"
noi di "  - Total observations in final data: " %9.0fc `total_final'
noi di ""

* Check if all master observations preserved
if `total_final' == `master_obs' {
    noi di "✓ SUCCESS: All master observations preserved!"
}
else {
    noi di "⚠ WARNING: Final observation count differs from master!"
    noi di "  Expected: " %9.0fc `master_obs'
    noi di "  Got:      " %9.0fc `total_final'
}
noi di ""

**# ==============================================================================
**# 5. ANALYZE NON-MATCHED OBSERVATIONS
**# ==============================================================================

noi di "=== STEP 5: Analyzing Non-Matched Observations ==="
noi di ""

if `not_matched_master' > 0 {
    noi di "Master observations without poverty data breakdown:"
    noi di ""
    
    * By year
    noi di "  By year:"
    tab ano if _merge_poverty == 1, missing
    noi di ""
    
    * By region
    noi di "  By region:"
    tab region_number if _merge_poverty == 1, missing
    noi di ""
    
    * By area
    noi di "  By urban/rural:"
    tab urban_area if _merge_poverty == 1, missing
    noi di ""
    
    * Year range of non-matched
    qui sum ano if _merge_poverty == 1
    noi di "  Year range of non-matched: " r(min) "-" r(max)
    noi di ""
    
    * Sample of non-matched observations
    noi di "  Sample of non-matched observations (first 20):"
    list ano region_number urban_area standardized_myers benford_abs_distance ///
        if _merge_poverty == 1 in 1/20, clean noobs abbreviate(12)
    noi di ""
}
else {
    noi di "✓ All master observations matched with poverty data!"
    noi di ""
}

if `not_matched_using' > 0 {
    noi di "⚠ WARNING: " `not_matched_using' " observations in poverty dataset not matched to master"
    noi di "  (These will NOT be included in the final dataset)"
    noi di ""
}

**# ==============================================================================
**# 6. VERIFY DATA QUALITY
**# ==============================================================================

noi di "=== STEP 6: Data Quality Checks ==="
noi di ""

* Check for missing values in poverty variables (among matched observations)
if `matched' > 0 {
    noi di "Missing values in poverty variables (among matched obs):"
    
    foreach var in pov_rate_3_00 pov_rate_4_20 pov_rate_8_30 ///
                   share_pov_3_00 share_pov_4_20 share_pov_8_30 ///
                   country_pov_3_00 country_pov_4_20 country_pov_8_30 {
        qui count if _merge_poverty == 3 & missing(`var')
        if r(N) > 0 {
            noi di "  - `var': " r(N) " missing (" %5.2f r(N)/`matched'*100 "%)"
        }
    }
    noi di ""
}

* Summary statistics for key variables
noi di "Summary statistics for poverty rates (matched observations only):"
sum pov_rate_3_00 pov_rate_4_20 pov_rate_8_30 if _merge_poverty == 3, separator(0)
noi di ""

**# ==============================================================================
**# 7. FINALIZE AND SAVE
**# ==============================================================================

noi di "=== STEP 7: Finalizing and Saving Dataset ==="
noi di ""

* Label the merge variable
label variable _merge_poverty "Merge indicator: poverty data"
label define merge_poverty_lbl 1 "Master only (no poverty data)" ///
                                2 "Using only (not in master)" ///
                                3 "Matched (poverty data added)"
label values _merge_poverty merge_poverty_lbl

* Sort the dataset
sort ano region_number urban_area

* Add/verify labels for poverty variables (these will overwrite if they exist)
capture label variable pov_rate_3_00 "Poverty rate at $3.00/day (2021 PPP) (%)"
capture label variable pov_rate_4_20 "Poverty rate at $4.20/day (2021 PPP) (%)"
capture label variable pov_rate_8_30 "Poverty rate at $8.30/day (2021 PPP) (%)"
capture label variable share_pov_3_00 "Share of country's poor at $3.00/day in this region-area"
capture label variable share_pov_4_20 "Share of country's poor at $4.20/day in this region-area"
capture label variable share_pov_8_30 "Share of country's poor at $8.30/day in this region-area"
capture label variable country_pov_3_00 "Country-level poverty rate at $3.00/day (%)"
capture label variable country_pov_4_20 "Country-level poverty rate at $4.20/day (%)"
capture label variable country_pov_8_30 "Country-level poverty rate at $8.30/day (%)"

* Save the merged dataset
save "$output_file", replace

noi di "✓ Dataset saved successfully!"
noi di "  File: " "$output_file"
noi di ""

**# ==============================================================================
**# 8. FINAL SUMMARY
**# ==============================================================================

noi di "=== FINAL SUMMARY ==="
noi di ""
noi di "Input datasets:"
noi di "  - Master (Myers-Benford):  " %9.0fc `master_obs' " observations"
noi di "  - Using (Poverty):         " %9.0fc `using_obs' " observations"
noi di ""
noi di "Merge results:"
noi di "  - Total in final dataset:  " %9.0fc `total_final' " observations"
noi di "  - Matched (with poverty):  " %9.0fc `matched' " (" %5.2f `matched'/`total_final'*100 "%)"
noi di "  - Not matched (no poverty):" %9.0fc `not_matched_master' " (" %5.2f `not_matched_master'/`total_final'*100 "%)"
noi di ""
noi di "Variables in final dataset: " c(k)
noi di ""

* List first 10 observations to verify
noi di "Sample of merged data (first 10 observations):"
list ano region_number urban_area standardized_myers benford_abs_distance ///
     pov_rate_8_30 country_pov_8_30 _merge_poverty in 1/10, ///
     clean noobs abbreviate(12)
noi di ""

noi di "=== MERGE COMPLETED SUCCESSFULLY ==="
noi di ""