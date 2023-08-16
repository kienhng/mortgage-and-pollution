clear all
set more off

*ssc install estout
*********************
* FOLLOW THESE STEPS
*********************

**# 1. Set CD

	// Chinh
	cd "C:\Users\kingd\OneDrive\D\DOCUMENTS\OOUeb\5. Kien's Thesis\uon-msc-thesis"
	
	// Kien
	cd ""
	
**# 2. Load data

	use "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis\Data\PanelData\panel_test.dta"

	xtset fips year
	destring state_code, replace

	gen carc_release = total_releases*carcinogen

**# 3. Run regs

/*	Flow will be like this:
	- Store regression results with `eststo'
	- Add desired scalars or notations such as: number of obs, outcome mean, FE "Yes", Control "Yes", R2, or other kinds of tests,... This can be done with `estadd'. !!! Note: almost all the useful and common scalars such as the R2, loglikelihood, chi2,... are already be stored inside each reg. Check the name of these value by `help rep' or `help logit' or whatever estimator you use
	- Repeat this process with multiple regressions. Usually, we can fit 3, 4, 5 (vertical layout) or at max 9 or 10 (horizontal layout) in a table, aesthetically. !!! Remember, all the estimate stored values must be under distinct names
	- After storing all these regs, make tables with `estout'
	- !!! Read `help estout' to know how to manipulate the table
*/	

	// Example
	
	sysuse auto.dta
	
	// Here I make 4 regs, store them as reg1...4
	// Then I add "Repair FE" and "Clustered Std Err" notations; also add "Outcome mean" -- These are not included in stored `reg'
	
	eststo reg1: reg price headroom trunk turn displacement gear_ratio foreign i.rep78, vce(cluster foreign)
		estadd local repfe "Yes", replace
		estadd local clstr "Yes", replace
		sum `e(depvar)' if e(sample)
		estadd scalar Mean = r(mean)
		
	eststo reg2: reg price weight length turn displacement gear_ratio foreign, vce(cluster foreign)
		estadd local repfe "No", replace
		estadd local clstr "Yes", replace
		sum `e(depvar)' if e(sample)
		estadd scalar Mean = r(mean)
		
	eststo reg3: reg mpg headroom trunk turn displacement gear_ratio foreign i.rep78
		estadd local repfe "Yes", replace
		estadd local clstr "No", replace
		sum `e(depvar)' if e(sample)
		estadd scalar Mean = r(mean)
		
	eststo reg4: reg mpg weight length turn displacement gear_ratio foreign
		estadd local repfe "No", replace
		estadd local clstr "No", replace
		sum `e(depvar)' if e(sample)
		estadd scalar Mean = r(mean)
	
	// Then I make table of them
	
	esttab reg1 reg2 reg3 reg4 ///
		using "Writing/tab-exp.tex", ///
		eqlabels("") ///
		title(Example tables \label{tab-exp}) ///
		label b(3) se nogaps nodep star(* 0.10 ** 0.05 *** 0.01) ///
		s(repfe clstr N Mean r2, label("Repair FE" "Clustered Std Err" "No. of Obs." "Outcome mean"  "R2") fmt(%3s %3s %9.0g 3 3)) ///
		keep(headroom trunk weight length turn displacement gear_ratio foreign) ///
		order(headroom trunk weight length turn displacement gear_ratio foreign) ///
		nonote addnote("Standard errors in parentheses." "* p $<$ 0.10, ** p $<$ 0.05, *** p $<$ 0.01.") ///
		replace
	
	
// 	xtreg rate_spread total_releases ltv_ratio property_value income, fe robust
//
// 	xtreg rate_spread carc_release ltv_ratio property_value income, fe robust