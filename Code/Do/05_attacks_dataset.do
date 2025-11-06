********************************************************************************
* PROGRAM: Process Terrorist Attacks Dataset
* PURPOSE: Aggregate case-level data to year-department level
* DATE: October 2025
********************************************************************************

clear all
set more off

* Define paths
global input_path "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\03_Data\CMH"
global output_path "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\03_Data\Merge"

********************************************************************************
* 1. IMPORT DATA FROM EXCEL (Sheet1)
********************************************************************************

* Import the Excel file from Sheet1
import excel using "$input_path\CasosAT_202503.xlsx", ///
    sheet("Sheet1") firstrow clear case(preserve)

* Display first few observations to verify import
di as result _n "=== FIRST 5 OBSERVATIONS AFTER IMPORT ==="
list in 1/5

********************************************************************************
* 2. CREATE REGION NUMBER FROM MUNICIPALITY CODE (CAREFULLY)
********************************************************************************

* Display original municipality codes to verify format
di as result _n "=== CHECKING MUNICIPALITY CODES ==="
list CódigoDANEdeMunicipio Departamento in 1/10

* Since the variable is already string, extract first 2 digits directly
gen region_number = real(substr(CódigoDANEdeMunicipio, 1, 2))

* Verify extraction worked correctly
di as result _n "=== VERIFYING REGION NUMBER EXTRACTION ==="
list CódigoDANEdeMunicipio region_number Departamento in 1/10

* Check for any missing values in region_number
count if missing(region_number)
if r(N) > 0 {
    di as error "WARNING: " r(N) " observations have missing region_number"
    list CódigoDANEdeMunicipio Departamento if missing(region_number)
}

* Verify specific examples from your data:
* Bogotá should be 11, Santander should be 68
di as result _n "=== VERIFICATION OF SPECIFIC DEPARTMENTS ==="
tab region_number if strpos(Departamento, "BOGOTA") > 0
tab region_number if strpos(Departamento, "SANTANDER") > 0

********************************************************************************
* 3. RENAME AND PREPARE VARIABLES
********************************************************************************

* Rename year variable
rename Año ano

* Rename department variable (keep original values)
rename Departamento Depto

* Rename victims variable
rename TotaldeVíctimasdelCaso victims

* Replace missing victims with 0 for summation
replace victims = 0 if missing(victims)

* Verify no missing values in key variables
di as result _n "=== CHECKING FOR MISSING VALUES ==="
tab ano, missing
tab region_number, missing
misstable summarize ano region_number Depto victims

********************************************************************************
* 4. CREATE YEAR-DEPARTMENT LEVEL DATASET
********************************************************************************

* Show summary before collapse
di as result _n "=== BEFORE COLLAPSE ==="
di as text "Total cases: " _N
tab ano, missing

* Generate a constant variable to count cases
gen case_count = 1

* Collapse to year-department level
* Count attacks (number of cases) and sum victims
collapse (sum) Attacks=case_count (sum) Victims=victims, ///
    by(ano region_number Depto)

* Show summary after collapse
di as result _n "=== AFTER COLLAPSE ==="
di as text "Total year-department observations: " _N

* Convert ano from string to numeric (double)
destring ano, replace

* Reorder variables
order ano region_number Depto Attacks Victims

* Check variable types
describe ano region_number Depto Attacks Victims

* Format display for numeric variables individually
format ano %10.0f
format region_number %10.0f
format Attacks %12.0f
format Victims %12.0f

********************************************************************************
* 5. VERIFY AND LABEL DATA
********************************************************************************

* Add variable labels
label variable ano "Year"
label variable region_number "Department Code (First 2 digits of DANE code)"
label variable Depto "Department Name"
label variable Attacks "Number of terrorist attacks"
label variable Victims "Total number of victims"

* Display data structure
describe

* Display summary statistics
summarize

* Show sample of final data
di as result _n "=== SAMPLE OF FINAL DATASET ==="
list in 1/10

* Sort data by year and region
sort ano region_number

* Display summary by year
di as result _n "=== ATTACKS AND VICTIMS BY YEAR ==="
table ano, statistic(sum Attacks) statistic(sum Victims)

* Display summary by department (top 10)
di as result _n "=== TOP 10 DEPARTMENTS BY TOTAL ATTACKS ==="
gsort -Attacks
list Depto region_number Attacks Victims in 1/10

********************************************************************************
* 6. SAVE OUTPUT DATASET
********************************************************************************

* Save as Stata dataset
save "$output_path\08_attacks.dta", replace

* Confirmation message
di as result _n "=========================================="
di as result "Dataset successfully created and saved!"
di as result "=========================================="
di as text "Input file:  $input_path\CasosAT_202503.xlsx"
di as text "Output file: $output_path\08_attacks.dta"
di as text "Total year-department observations: " _N
summarize ano
di as text "Years covered: " r(min) " to " r(max)
di as text "Total attacks in dataset: " = sum(Attacks)
di as text "Total victims in dataset: " = sum(Victims)

********************************************************************************
* END OF PROGRAM
********************************************************************************