**********************************************************************
*First approximation to the Benford's law using sedlac datalib
*Author: Luis Castellanos
*Last modification: 2025-03-22
**********************************************************************

cls
clear all

**# Optional: onetime installation of Datalib
/*clear all
global rootdatalib "s:\Datalib"
net from "S:\Datalib\_ado"
net install lel, replace
net install essentials, replace
essentials, replace
datalib_update, replace
discard*/

global rootdatalib "s:\Datalib"


**# PRY

global countries "pry"
global years "2014" "2015" "2016" "2017" "2018" "2019" "2020" "2021" "2022" "2023" "2024"

foreach country in $countries {
foreach year in $years { 
*foreach quarter in q01 q02 q03 q04 {
	cap datalib, country(`country') year(`year') mod(all) clear
	if _N > 1 {
	display "`country'_`year'_sedlac"
	*tab ocupado[iw=pondera]
    gen firstdigit = real(substr(string(ipcf_ppp17), 1, 1))
	contract firstdigit
	gen x = _n 
	gen expected = log10(1 + 1/x) 
	correlate firstdigit expected
	/* Create and save the Benford's Law graph */
	twoway histogram firstdigit [fw=_freq], barw(0.5) bfcolor(ltblue) blcolor(navy) discrete fraction ///
	|| connected expected x, xla(1/9) ytitle("observed and expected" "probabilities under Benford's Law") ///
	yla(, ang(h) format("%02.1f")) legend(off) title("`country' `year'")

	/* Save the graph as PNG file */
	graph export "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\Outputs\PNG\Bendfords Law\Datalib\\`country'_`year'_Bendford.png", replace width(1200) height(900)
}   
}
}

**# BOL
global countries "bol"
global years "2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024"

foreach country in $countries {
    foreach year in $years { 
        cap datalib, country(`country') year(`year') mod(all) clear
        if _N > 1 {
            display "`country'_`year'_sedlac"
            *tab ocupado[iw=pondera]
            gen firstdigit = real(substr(string(ipcf_ppp17), 1, 1))
            contract firstdigit
            gen x = _n 
            gen expected = log10(1 + 1/x) 
            correlate firstdigit expected
            
            /* Create and save the Benford's Law graph */
            twoway histogram firstdigit [fw=_freq], barw(0.5) bfcolor(ltblue) blcolor(navy) discrete fraction ///
            || connected expected x, xla(1/9) ytitle("observed and expected" "probabilities under Benford's Law") ///
            yla(, ang(h) format("%02.1f")) legend(off) title("`country' `year'")
            
            /* Save the graph as PNG file */
            graph export "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\Outputs\PNG\Bendfords Law\Datalib\\`country'_`year'_Bendford.png", replace width(1200) height(900)
        }   
    }
}

**# ARG
global countries "arg"
global years "2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024"

foreach country in $countries {
    foreach year in $years { 
        cap datalib, country(`country') year(`year') mod(all) clear
        if _N > 1 {
            display "`country'_`year'_sedlac"
            *tab ocupado[iw=pondera]
            gen firstdigit = real(substr(string(ipcf_ppp17), 1, 1))
            contract firstdigit
            gen x = _n 
            gen expected = log10(1 + 1/x) 
            correlate firstdigit expected
            
            /* Create and save the Benford's Law graph */
            twoway histogram firstdigit [fw=_freq], barw(0.5) bfcolor(ltblue) blcolor(navy) discrete fraction ///
            || connected expected x, xla(1/9) ytitle("observed and expected" "probabilities under Benford's Law") ///
            yla(, ang(h) format("%02.1f")) legend(off) title("`country' `year'")
            
            /* Save the graph as PNG file */
            graph export "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\Outputs\PNG\Bendfords Law\Datalib\\`country'_`year'_Bendford.png", replace width(1200) height(900)
        }   
    }
}

**# CHL
global countries "chl"
global years "2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024"

foreach country in $countries {
    foreach year in $years { 
        cap datalib, country(`country') year(`year') mod(all) clear
        if _N > 1 {
            display "`country'_`year'_sedlac"
            *tab ocupado[iw=pondera]
            gen firstdigit = real(substr(string(ipcf_ppp17), 1, 1))
            contract firstdigit
            gen x = _n 
            gen expected = log10(1 + 1/x) 
            correlate firstdigit expected
            
            /* Create and save the Benford's Law graph */
            twoway histogram firstdigit [fw=_freq], barw(0.5) bfcolor(ltblue) blcolor(navy) discrete fraction ///
            || connected expected x, xla(1/9) ytitle("observed and expected" "probabilities under Benford's Law") ///
            yla(, ang(h) format("%02.1f")) legend(off) title("`country' `year'")
            
            /* Save the graph as PNG file */
            graph export "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\Outputs\PNG\Bendfords Law\Datalib\\`country'_`year'_Bendford.png", replace width(1200) height(900)
        }   
    }
}

**# Col
global countries "col"
global years "2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024"

foreach country in $countries {
    foreach year in $years { 
        cap datalib, country(`country') year(`year') mod(all) clear
        if _N > 1 {
            display "`country'_`year'_sedlac"
            *tab ocupado[iw=pondera]
            gen firstdigit = real(substr(string(ipcf_ppp17), 1, 1))
            contract firstdigit
            gen x = _n 
            gen expected = log10(1 + 1/x) 
            correlate firstdigit expected
            
            /* Create and save the Benford's Law graph */
            twoway histogram firstdigit [fw=_freq], barw(0.5) bfcolor(ltblue) blcolor(navy) discrete fraction ///
            || connected expected x, xla(1/9) ytitle("observed and expected" "probabilities under Benford's Law") ///
            yla(, ang(h) format("%02.1f")) legend(off) title("`country' `year'")
            
            /* Save the graph as PNG file */
            graph export "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\Outputs\PNG\Bendfords Law\Datalib\\`country'_`year'_Bendford.png", replace width(1200) height(900)
        }   
    }
}

**# Bra
global countries "bra"
global years "2017 2018 2019 2020 2021 2022 2023 2024"

foreach country in $countries {
    foreach year in $years { 
        cap datalib, country(`country') year(`year') mod(all) clear
        if _N > 1 {
            display "`country'_`year'_sedlac"
            *tab ocupado[iw=pondera]
            gen firstdigit = real(substr(string(ipcf_ppp17), 1, 1))
            contract firstdigit
            gen x = _n 
            gen expected = log10(1 + 1/x) 
            correlate firstdigit expected
            
            /* Create and save the Benford's Law graph */
            twoway histogram firstdigit [fw=_freq], barw(0.5) bfcolor(ltblue) blcolor(navy) discrete fraction ///
            || connected expected x, xla(1/9) ytitle("observed and expected" "probabilities under Benford's Law") ///
            yla(, ang(h) format("%02.1f")) legend(off) title("`country' `year'")
            
            /* Save the graph as PNG file */
            graph export "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\Measurement repo\State-capacty-and-Mesurement\Outputs\PNG\Evidence Bendfords Law\Datalib\\`country'_`year'_Bendford.png", replace width(1200) height(900)
        }   
    }
}

**# ecu
global countries "ecu"
global years "2017 2018 2019 2020 2021 2022 2023 2024"

foreach country in $countries {
    foreach year in $years { 
        cap datalib, country(`country') year(`year') mod(all) clear
        if _N > 1 {
            display "`country'_`year'_sedlac"
            *tab ocupado[iw=pondera]
            gen firstdigit = real(substr(string(ipcf_ppp17), 1, 1))
            contract firstdigit
            gen x = _n 
            gen expected = log10(1 + 1/x) 
            correlate firstdigit expected
            
            /* Create and save the Benford's Law graph */
            twoway histogram firstdigit [fw=_freq], barw(0.5) bfcolor(ltblue) blcolor(navy) discrete fraction ///
            || connected expected x, xla(1/9) ytitle("observed and expected" "probabilities under Benford's Law") ///
            yla(, ang(h) format("%02.1f")) legend(off) title("`country' `year'")
            
            /* Save the graph as PNG file */
            graph export "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\Measurement repo\State-capacty-and-Mesurement\Outputs\PNG\Evidence Bendfords Law\Datalib\\`country'_`year'_Bendford.png", replace width(1200) height(900)
        }   
    }
}


*********************************************************************************
*Save Databases for the measuring project

**# Colombia
datalib, country(col) year(2018) mod(all) clear
compress
save "C:\Users\wb593225\OneDrive - WBG\Desktop\Luis - Private\Mesurement project\Sedlac\SEDLAC_col_2018_all.dta", replace
