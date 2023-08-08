clear all
set more off

*********************
* FOLLOW THESE STEPS
*********************

**# 1. Set CD

	// Chinh
	cd "C:\Users\kingd\OneDrive\D\DOCUMENTS\OOUeb\5. Kien's Thesis\uon-msc-thesis"]
	
	// Kien
	cd "..."
	
**# 2. Load data

	use "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis\Data\PanelData\panel_test.dta"

	xtset fips year
	destring state_code, replace

	gen carc_release = total_releases*carcinogen

**# 3. Run regs

/*	Flow will be like this:
	- Store regression results with `eststo'
	- Add desired scalars or notations such as: number of obs, outcome mean, FE "Yes", Control "Yes", R2, or other kinds of tests,... This can be done with `estadd'. !!! Note: almost all the useful and common scalars such as the R2, loglikelihood, chi2,... are already be stored inside each reg. Check the name of these value by `help rep' or `help logit' or whatever estimator you use
	- Repeat this process with multiple regressions. Usually, we can fit 3, 4, 5, or at max 9 or 10 (horizontal layout) in a table, aesthetically. !!! Remember, all the estimate stored values must be under distinct names
	- After storing all these regs, make tables with `estout'
*/	

	// Example
	
	sysuse auto.dta
	
	eststo reg1: reg price headroom trunk turn displacement gear_ratio foreign i.rep78, vce(cluster foreign)
		estadd 
	
	
	
	
	
	
	
	
	
	
	
	


	xtreg rate_spread total_releases ltv_ratio property_value income, fe robust

	xtreg rate_spread carc_release ltv_ratio property_value income, fe robust