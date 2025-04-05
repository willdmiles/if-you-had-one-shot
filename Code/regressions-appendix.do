
// set working directory here --->
cd "/Users/williammiles/Dropbox/Intellectual Outliers/NBER-wp/"
// ---


***************************************************************************
**
** Table E.1
**  
** Replication of baseline results with unproven targets only
**
***************************************************************************


use "Data/unproven_targets_only.dta", clear
 

// label variables
label var av_scale "Average Experimenter Scale"
label var av_scale_lag1 "Average Experimenter Scale (t-1)"
label var shannon "Target Diversity"

egen atc1_year = group(atc1 project_start_year)


// approach diversity and outcomes

eststo clear 

reghdfe shannon av_scale ,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe shannon av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo


esttab  , nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity"  "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace
 
esttab using "Tables/baseline_unproven_targets_only.tex",   nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity"  "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace
 
 
 
***************************************************************************
**
** Table E.2 (a)
**  
** Replication of key results with market defined in longer time windows
** Unit of Observation = Therapeutic Class—Two Year Period
**
***************************************************************************


use "Data/2year_window.dta", clear

// label variables
label var av_scale "Average Experimenter Scale"
label var av_scale_lag1 "Average Experimenter Scale (t-1)"
label var shannon "Target Diversity"

egen atc1_year = group(atc1 project_start_year)


eststo clear 

reghdfe shannon av_scale ,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe shannon av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo


esttab  , nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity"  "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace

esttab using "Tables/baseline_2yearwindow.tex", nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity"  "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace

 
***************************************************************************
**
** Table E.2 (b)
**  
** Replication of key results with market defined in longer time windows
** Unit of Observation = Therapeutic Class—Five Year Period
**
***************************************************************************


use "Data/5year_window.dta", clear

// label variables
label var av_scale "Average Experimenter Scale"
label var av_scale_lag1 "Average Experimenter Scale (t-1)"
label var shannon "Target Diversity"

egen atc1_year = group(atc1 project_start_year)

eststo clear 

reghdfe shannon av_scale ,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe shannon av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo


esttab  , nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity"  "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace

esttab using "Tables/baseline_5yearwindow.tex",   nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity"  "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace
 

***************************************************************************
**
** Table E.3 (a)
**  
** Measuring Diversity with HHI
**
***************************************************************************

 
use "Data/market_year_dataset.dta", clear

// transform variables/ fix missing data
g ln_discovery = ln(n_discovery)
g ln_discovery_lag1 = ln(n_discovery_lag1)
replace av_firm_age = . if av_firm_age < 0

// label variables
label var av_scale "Average Experimenter Scale"
label var av_scale_lag1 "Average Experimenter Scale\textsubscript{t-1)}"
label var share_multi "Multi-Experiment Share"
label var share_multi_lag1 "Multi-Experiment Share\textsubscript{t-1)}"
label var shannon "Target Diversity"
label var ln_discovery "ln(Discovery)"
label var av_firm_age "Average Firm Age"
label var atleastone_phase1 "At least 1 Pre-Clinical Success"
label var phase1_share "Share of Pre-Clinical Success"
label var atleastone_launch "At least 1 Drug Launch"
label var launch_share "Share of Drug Launch"
label var project_start_year "Project Start Year"
label var n_firms "Number of Firms"
label var n_trials "Number of Projects Started"
label var n_approaches "Number of Targets"

// create act1-year fe and drop where missing
egen atc1_year = group(atc1 project_start_year)
drop if missing(atc1)
 
//
 
eststo clear 

reghdfe hhi av_scale ,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe hhi av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo


reghdfe atleastone_phase1 hhi,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 hhi n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo


esttab , nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("HHI" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace
 
 
esttab using "Tables/measuring_diversity_with_HHI.tex", nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("HHI" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace



***************************************************************************
**
** Table E.3 (b)
**  
** Measuring Success with Drug Launch
**
***************************************************************************

eststo clear

reghdfe launch_share av_scale,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe launch_share av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_launch shannon,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "No", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_launch shannon n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo


esttab  , nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("\shortstack{Share of\\ Drug Launch}" "\shortstack{At least 1\\Drug Launch}", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace
 
 
esttab using "Tables/measuring_success_as_launch.tex", nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("\shortstack{Share of\\ Drug Launch}" "\shortstack{At least 1\\Drug Launch}", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace
 
 
***************************************************************************
**
** Table E.4
**  
** 2SLS
**
***************************************************************************


eststo clear 


reghdfe av_scale av_scale_lag1 n_trials n_firms n_approaches, a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
local f=e(F)
eststo

ivreghdfe shannon (av_scale = av_scale_lag1)  n_trials n_firms n_approaches, a( atc1_year)  cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
quietly estadd scalar f=round(`f', 0.001)
eststo

esttab ,  nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a f, fmt(3 3 %9.0fc %9.3f %9.3fc)  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" "First Stage F-test")) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups( "\shortstack{Average\\Experimenter\\Scale (t)}" "\shortstack{Target\\Diversity}", pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) mtitles("First Stage" "2SLS" ) drop(n_trials n_firms n_approaches _cons) replace
 
esttab using "Tables/2sls.tex",  nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a f, fmt(3 3 %9.0fc %9.3f %9.3fc)  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" "First Stage F-test")) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups( "\shortstack{Average\\Experimenter\\Scale (t)}" "\shortstack{Target\\Diversity}", pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) mtitles("First Stage" "2SLS" ) drop(n_trials n_firms n_approaches _cons) replace
 


