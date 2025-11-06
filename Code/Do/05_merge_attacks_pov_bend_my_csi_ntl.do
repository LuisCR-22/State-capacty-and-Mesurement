/*====================================================================
Project:        Merge Attacks Data with Myers-Benford-Poverty Dataset
====================================================================
PURPOSE: Merge the attacks dataset (using) with the master dataset
         (Myers-Benford-Poverty) at year-region level
         
MERGE KEYS: ano, region_number
NOTE: Attacks data is at department level, so both urban and rural 
      areas will receive the same attack values for each year-department

OUTPUT: 09_V01_my_Bn_CSI_NTL_pov_atack_dep_area.dta
*=================================================================*/

clear all
set more off

**# ==============================================================================
**# 0. DEFINE PATHS
**# ==============================================================================

global data_path "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\03_Data\Merge"

* Input files
global master_file "$data_path\07_V01_my_Bend_CSI_NTL_pov_depto_area.dta"
global using_file "$data_path\08_attacks.dta"

* Output file
global output_file "$data_path\09_V01_my_Bn_CSI_NTL_pov_atack_dep_area.dta"

noi di ""
noi di "=== MERGING ATTACKS DATA WITH MYERS-BENFORD-POVERTY DATASET ==="
noi di ""

**# ==============================================================================
**# 1. LOAD AND INSPECT MASTER DATASET
**# ==============================================================================

noi di "=== STEP 1: Loading Master Dataset (Myers-Benford-Poverty) ==="
use "$master_file", clear

* Check if merge keys exist
foreach var in ano region_number {
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

capture confirm variable urban_area
if _rc == 0 {
    qui tab urban_area
    local master_areas = r(r)
}
else {
    local master_areas = 1
}

* Get year range
qui sum ano
local master_year_min = r(min)
local master_year_max = r(max)

noi di "Master dataset loaded successfully:"
noi di "  - Total observations: " %9.0fc `master_obs'
noi di "  - Years: " `master_years' " (range: `master_year_min'-`master_year_max')"
noi di "  - Regions (departments): " `master_regions'
noi di "  - Areas: " `master_areas' " (0=rural, 1=urban)"
noi di ""

* Check for duplicates in master on merge keys
duplicates report ano region_number
qui duplicates tag ano region_number, gen(dup_master)
qui count if dup_master > 0
if r(N) > 0 {
    noi di "NOTE: Master dataset has " r(N) " observations with same year-region"
    noi di "  (This is expected - urban and rural areas are separate observations)"
    noi di ""
}
drop dup_master

**# ==============================================================================
**# 2. INSPECT USING DATASET (ATTACKS DATA)
**# ==============================================================================

noi di "=== STEP 2: Inspecting Using Dataset (Attacks) ==="

preserve
use "$using_file", clear

* Check if merge keys exist
foreach var in ano region_number {
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

* Get year range
qui sum ano
local using_year_min = r(min)
local using_year_max = r(max)

noi di "Using dataset (attacks) loaded successfully:"
noi di "  - Total observations: " %9.0fc `using_obs'
noi di "  - Years: " `using_years' " (range: `using_year_min'-`using_year_max')"
noi di "  - Regions (departments): " `using_regions'
noi di "  - Level: Year-Department (no urban/rural breakdown)"
noi di ""

* Check for duplicates in using on merge keys
duplicates report ano region_number
qui duplicates tag ano region_number, gen(dup_using)
qui count if dup_using > 0
if r(N) > 0 {
    noi di "WARNING: Using dataset has " r(N) " duplicate observations on merge keys!"
    noi di "  First 10 duplicates:"
    list ano region_number Depto Attacks Victims if dup_using > 0 in 1/10, clean noobs
    noi di ""
}
else {
    noi di "✓ No duplicates found in using dataset on merge keys"
    noi di ""
}

* Summary of attacks variables
noi di "Attack variables summary:"
sum Attacks Victims, separator(0)
noi di ""

* Check for missing values in attack variables
qui count if missing(Attacks)
local miss_attacks = r(N)
qui count if missing(Victims)
local miss_victims = r(N)

if `miss_attacks' > 0 | `miss_victims' > 0 {
    noi di "WARNING: Missing values detected in attacks dataset:"
    noi di "  - Missing Attacks: " `miss_attacks'
    noi di "  - Missing Victims: " `miss_victims'
    noi di ""
}

restore

**# ==============================================================================
**# 3. PERFORM THE MERGE
**# ==============================================================================

noi di "=== STEP 3: Performing the Merge ==="
noi di "Merge type: Many-to-one (m:1)"
noi di "  - Multiple master obs (urban+rural) → one using obs (department total)"
noi di "Merge keys: ano, region_number"
noi di "Note: Both urban and rural areas will receive the same attack values"
noi di ""

* Perform merge (many-to-one)
merge m:1 ano region_number using "$using_file", ///
    generate(_merge_attacks) keepusing(Attacks Victims Depto)

**# ==============================================================================
**# 4. ANALYZE MERGE RESULTS
**# ==============================================================================

noi di "=== STEP 4: Merge Results ==="
noi di ""

* Overall merge statistics
tab _merge_attacks

noi di ""
noi di "Detailed merge statistics:"

* Count by merge result
qui count if _merge_attacks == 1
local not_matched_master = r(N)

qui count if _merge_attacks == 2
local not_matched_using = r(N)

qui count if _merge_attacks == 3
local matched = r(N)

qui count
local total_final = r(N)

noi di "  - Master only (no attacks data):    " %9.0fc `not_matched_master' " (" %5.2f `not_matched_master'/`master_obs'*100 "%)"
noi di "  - Using only (not in master):       " %9.0fc `not_matched_using'
noi di "  - Matched (attacks data added):     " %9.0fc `matched' " (" %5.2f `matched'/`master_obs'*100 "%)"
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
**# 5. HANDLE NON-MATCHED OBSERVATIONS
**# ==============================================================================

noi di "=== STEP 5: Handling Non-Matched Observations ==="
noi di ""

if `not_matched_master' > 0 {
    noi di "Master observations without attacks data: " `not_matched_master'
    noi di "Setting Attacks and Victims to 0 for these observations..."
    noi di ""
    
    * Replace missing with 0 for non-matched master observations
    replace Attacks = 0 if _merge_attacks == 1
    replace Victims = 0 if _merge_attacks == 1
    
    * Show breakdown by year
    noi di "  Breakdown by year (observations with Attacks=0):"
    tab ano if Attacks == 0, missing
    noi di ""
    
    * Show breakdown by region (top 10)
    noi di "  Top 10 regions with most zero-attack observations:"
    preserve
    keep if Attacks == 0
    contract region_number, freq(n_obs)
    gsort -n_obs
    list region_number n_obs in 1/10, clean noobs
    restore
    noi di ""
}
else {
    noi di "✓ All master observations matched with attacks data!"
    noi di ""
}

if `not_matched_using' > 0 {
    noi di "⚠ WARNING: " `not_matched_using' " observations in attacks dataset not matched to master"
    noi di "  (These will be DROPPED from the final dataset)"
    
    * Show which year-regions are only in using
    noi di ""
    noi di "  Year-regions only in attacks dataset (first 10):"
    list ano region_number Depto Attacks Victims if _merge_attacks == 2 in 1/10, clean noobs
    noi di ""
    
    * Drop these observations
    drop if _merge_attacks == 2
    
    qui count
    local total_after_drop = r(N)
    noi di "  Dropped " `not_matched_using' " observations"
    noi di "  Final observation count: " %9.0fc `total_after_drop'
    noi di ""
}

**# ==============================================================================
**# 6. VERIFY DATA QUALITY AND FINAL ADJUSTMENTS
**# ==============================================================================

noi di "=== STEP 6: Data Quality Checks and Final Adjustments ==="
noi di ""

* Ensure no missing values in Attacks and Victims
qui count if missing(Attacks)
local miss_att_final = r(N)
qui count if missing(Victims)
local miss_vic_final = r(N)

if `miss_att_final' > 0 | `miss_vic_final' > 0 {
    noi di "⚠ Remaining missing values detected - replacing with 0:"
    noi di "  - Missing Attacks: " `miss_att_final'
    noi di "  - Missing Victims: " `miss_vic_final'
    
    replace Attacks = 0 if missing(Attacks)
    replace Victims = 0 if missing(Victims)
    noi di "  ✓ Replaced with 0"
    noi di ""
}
else {
    noi di "✓ No missing values in Attacks and Victims"
    noi di ""
}

* Summary statistics for attacks variables
noi di "Summary statistics for attacks variables (full dataset):"
sum Attacks Victims, separator(0)
noi di ""

* Distribution of attacks
noi di "Distribution of Attacks:"
tab Attacks if Attacks <= 10, missing
noi di "  (Note: Only showing Attacks ≤ 10 for readability)"
noi di ""

* Verify that urban and rural have same values for same year-department
capture confirm variable urban_area
if _rc == 0 {
    noi di "Verification: Urban and rural areas have same attack values"
    noi di "  (Checking first 5 year-regions with both urban and rural):"
    
    preserve
    bysort ano region_number: gen n_areas = _N
    keep if n_areas == 2  // Keep only year-regions with both urban and rural
    sort ano region_number urban_area
    list ano region_number urban_area Attacks Victims in 1/10, ///
        sepby(ano region_number) clean noobs
    restore
    noi di ""
}

**# ==============================================================================
**# 7. FINALIZE AND SAVE
**# ==============================================================================

noi di "=== STEP 7: Finalizing and Saving Dataset ==="
noi di ""

* Label the merge variable
label variable _merge_attacks "Merge indicator: attacks data"
label define merge_attacks_lbl 1 "Master only (no attacks, set to 0)" ///
                                3 "Matched (attacks data added)"
label values _merge_attacks merge_attacks_lbl

* Add labels for attacks variables
label variable Attacks "Number of terrorist attacks (year-department)"
label variable Victims "Total number of victims (year-department)"

* Sort the dataset
sort ano region_number urban_area

* Verify final observation count
qui count
local final_obs = r(N)

if `final_obs' == `master_obs' {
    noi di "✓ Final dataset has same number of observations as master: " %9.0fc `final_obs'
}
else {
    noi di "⚠ WARNING: Observation count changed!"
    noi di "  Master: " %9.0fc `master_obs'
    noi di "  Final:  " %9.0fc `final_obs'
}
noi di ""

* Save the merged dataset
save "$output_file", replace

noi di "✓ Dataset saved successfully!"
noi di "  File: $output_file"
noi di ""

**# ==============================================================================
**# 8. FINAL SUMMARY
**# ==============================================================================

noi di "=== FINAL SUMMARY ==="
noi di ""
noi di "Input datasets:"
noi di "  - Master (Myers-Benford-Poverty): " %9.0fc `master_obs' " observations"
noi di "  - Using (Attacks):                " %9.0fc `using_obs' " observations"
noi di ""
noi di "Merge results:"
noi di "  - Total in final dataset:         " %9.0fc `final_obs' " observations"
noi di "  - Matched (with attacks data):    " %9.0fc `matched' " (" %5.2f `matched'/`final_obs'*100 "%)"
noi di "  - Not matched (attacks set to 0): " %9.0fc `not_matched_master' " (" %5.2f `not_matched_master'/`final_obs'*100 "%)"
noi di ""
noi di "Attack statistics:"
qui sum Attacks
noi di "  - Mean attacks per obs:           " %9.2f r(mean)
noi di "  - Total attacks in dataset:       " %9.0f r(sum)
qui sum Victims
noi di "  - Mean victims per obs:           " %9.2f r(mean)
noi di "  - Total victims in dataset:       " %9.0f r(sum)
noi di ""
noi di "Variables in final dataset: " c(k)
noi di ""

* Sample of merged data
capture confirm variable urban_area
if _rc == 0 {
    noi di "Sample of merged data (first 10 observations):"
    list ano region_number urban_area Attacks Victims _merge_attacks ///
         in 1/10, clean noobs abbreviate(12)
}
else {
    noi di "Sample of merged data (first 10 observations):"
    list ano region_number Attacks Victims _merge_attacks ///
         in 1/10, clean noobs abbreviate(12)
}
noi di ""

noi di "=== MERGE COMPLETED SUCCESSFULLY ==="
noi di "NOTE: Both urban and rural areas within the same year-department"
noi di "      have the same Attacks and Victims values (department totals)"
noi di ""