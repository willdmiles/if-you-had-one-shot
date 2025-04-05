### If you had one shot: Scale and herding in innovation experiments

Ashish Arora, Sharique Hasan, and William D. Miles

Last Updated: April 5th, 2025

---
*Notes:*

* This README.md file describes the contents of the supplementary materials that can be used to replicate the paper. 
* If one were to run all of the code files, they would generate the tables and figures described below. Note that some figures are created directly from raw data that cannot be shared. 
* All code, data, and outputs from the simulations are organized in their own, separate folder

---

#### Code
* `regressions-main-paper.do` &mdash; Stata code that takes as input market-year-dataset.dta and produced all .tex files for all tables featured in the main paper 
* `regressions-appendix.do` &mdash; Stata code that takes as input various modifications on the main dataset to produce all .tex files for all tables featured in the Appendix
* `plot.R` &mdash; R code to create all figures featured in the main paper and Appendix that can be produced from the provided datasets
	
---

#### Data
*Notes: Only the final dataset and raw GWAS data are provided*

* `market-year-dataset.dta` &mdash; 	dataset used in all analyses featured in the main paper
* `unproven_targets_only.dta` &mdash; dataset used in robustness analysis featured in the Appendix where targets-therapeutic classes are dropped from the construction of the dataset once there is a launched drug for that target-therapeutic class
* `2year_window.dta` &mdash; dataset used in robustness analysis featured in the Appendix where the unit of analysis is the market-two year window
* `5year_window.dta` &mdash; dataset used in robustness analysis featured in the Appendix where the unit of analysis is the market-five year window
* `gwas_catalog.tsv` &mdash; raw GWAS data downloaded on 11-03-2024
	
---
#### Tables


* (3) `descriptive_statistics.tex`
* (4) `correlation_matrix.tex` 
* (5) `baseline_correlations.tex` &mdash; The market-level relationship between experimenter scale, the diversity of approaches, and success
* (6) `linking_diversity_to_success.tex` &mdash; Linking approach diversity to the success of experiments
* (7) `adding-controls-to-baseline.tex` &mdash; The relationship between experimenter scale and target diversity while controlling for target discovery and firm age
* (E.1) `baseline_unproven_targets_only.tex`
* (E.2) 
	* (a) `baseline_2yearwindow.tex`
	* (b) `baseline_5yearwindow.tex`
* (E.3)
	* (a) `measuring_diversity_with_HHI.tex`
	* (b) `measuring_success_as_launch.tex`
* (E.4) `2sls.tex`

---
#### Figures

* (3) &mdash; Binned scatter plots of the market-level relationship between experimenter scale, the diversity of approaches, and success
	* (a) `target_diversity_exp_scale.pdf` 
	* (b) `scale_phase1_share.pdf` 
	* (c) `diversity_atleastone_phase1.pdf` 
* (D.1) `discovery_over_time.pdf` &mdash; a line graph of  new gene-disease discoveries over time

---
#### Simulations

Code

* `baseline-simulations.R`
* `discovery-simulations.R`

Data

* `baseline-simulation-results.csv`
* `discovery-simulation-results.csv`

Figures

* (C.1) baseline simulations
	* (a) `baseline-sims-diversity-exp-share.pdf`
	* (b) `baseline-sims-success-share-exp-share.pdf`
	* (c) `baseline-sims-atleastone-success-diversity.pdf`
* (C.2) simulations with discovery
	* (a) `sim_discovery_shannon.pdf`
	* (b) `sim_discovery_atleastone_exp_share.pdf`
	* (c) `sim_discovery_atleastone_diversity.pdf`
	* (d) `sim_discovery_success_share_exp_share.pdf`
	* (e) `sim_discovery_success_share_diversity.pdf`



---

#### Manuscript (.tex files)

* Includes all .tex files that make up the main paper and the appendix
