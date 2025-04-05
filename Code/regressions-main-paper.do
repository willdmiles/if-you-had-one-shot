
// set working directory here --->
// ---


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


**********************************************************************
** TABLE 3: DESCRIPTIVE STATS
**********************************************************************

eststo clear

estpost tabstat shannon  av_scale av_scale_lag1 share_multi share_multi_lag1 ln_discovery av_firm_age atleastone_phase1 phase1_share atleastone_launch launch_share project_start_year n_firms n_trials n_approaches, c(stat) stat(mean sd min p25 p50 p75 max n)

esttab using "Tables/descriptive_statistics.tex", replace collabels("Mean" "St. Dev." "Min" "Q1" "Median" "Q3" "Max" "N") cells("mean(fmt(3)) sd min p25 p50 p75 max count(fmt(0))") label nomtitles nonumbers noobs 


**********************************************************************
** TABLE 4: CORRELATION MATRIX
**********************************************************************

eststo clear
estpost correlate shannon  av_scale av_scale_lag1 share_multi share_multi_lag1 ln_discovery av_firm_age atleastone_phase1 phase1_share atleastone_launch launch_share project_start_year n_firms n_trials n_approaches, matrix listwise

esttab using "Tables/correlation_matrix.tex", replace cells(b (fmt(%4.2fc))) label unstack not noobs nonumbers compress eqlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)" "(13)" "(14)" "(15)") collabels(none) noisily


**********************************************************************
** TABLE 5:  The market-level relationship between experimenter scale, the diversity of approaches, and success
**********************************************************************

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

esttab using "Tables/baseline_correlations.tex",  nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity"  "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace


**********************************************************************
** TABLE 6:  Linking approach diversity to the success of experiments
**********************************************************************

eststo clear 

reghdfe phase1_share shannon  n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share shannon av_scale n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon  n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon av_scale  n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo


esttab ,  nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons)

esttab using "Tables/linking_diversity_to_success.tex", nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) replace



**********************************************************************
** TABLE 7: The relationship between experimenter scale and target diversity while controlling for target discovery and firm age
**********************************************************************


eststo clear 

reghdfe shannon av_scale ln_discovery  n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe shannon av_scale  av_firm_age n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe shannon av_scale ln_discovery av_firm_age n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale ln_discovery  n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale  av_firm_age n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe phase1_share av_scale ln_discovery av_firm_age n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon ln_discovery  n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon  av_firm_age n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

reghdfe atleastone_phase1 shannon ln_discovery av_firm_age n_trials n_firms n_approaches,  a( atc1_year) cluster(atc1)
quietly estadd local market_structure_controls "Yes", replace
quietly estadd local atc1_year_fe "Yes", replace
eststo

esttab ,  nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity" "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) order(av_scale shannon) replace
 

esttab using "Tables/adding-controls-to-baseline.tex",nobaselevels varwidth(40) noconstant stats(market_structure_controls atc1_year_fe N r2_a , fmt(3 3 %9.0fc %9.3f )  labels("Market Structure Controls" "ATC-1\$\times\$Year FE" "Observations" "Adj R-squared" )) star(* .1 ** .05 *** .01) label se ar2 b(3) nogaps nonotes   mgroups("Target Diversity" "\shortstack{Share of\\ Pre-Clinical Success}" "\shortstack{At least 1\\Pre-Clinical Success}", pattern(1 0 0 1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}) ) nomtitles drop(n_trials n_firms n_approaches _cons) order(av_scale shannon)  replace





