* Name: psm_sample.do
* Author: Kien Hoang-Le
* Description: PSM on the 200,000 obs sample

**# 1. Setting up environment

capture log close
clear
set linesize 80
set more off

*ssc install reghdfe
*ssc install ftools

dis "`c(username)'"

// Set WD

if "`c(username)'"=="HDC" {
	cd "C:\Users\kingd\OneDrive\uon-msc-thesis"
}

if "`c(username)'" == "HLK" {
	cd "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis"
} 

use "Data\PanelData\psm_sample.dta", clear

**# 2. Create new variables
**## 2.1. Encoding
encode year_fips, gen(year_fip1)
drop year_fips
rename year_fip1 year_fips

encode state, gen(state_1)
drop state
rename state_1 state

encode lei, gen(lei_1)
drop lei
rename lei_1 lei

encode region, gen(region_1)
drop region
rename region_1 region

destring year, replace

// gen ln_income = log(income)

**## 2.2. Create treatment variable:
**### Carc Total
gen carc_2level = 0
replace carc_2level = 1 if (carc_releases > 0) //Treatment

**### Carc Onsite
gen lncarc_on_pa = log(pa_carc_onsite)

gen carc_on2level = 0
replace carc_on2level = 1 if (carc_onsite > 0) //Treatment

gen carc_on4level = 0
replace carc_on4level = 1 if (carc_onsite > 0) //0
replace carc_on4level = 2 if (carc_onsite > 72.77791) //p25
replace carc_on4level = 3 if (carc_onsite > 324.262) //p50
replace carc_on4level = 4 if (carc_onsite > 931.1233) //p75

**### Carc Air
gen carc_air2level = 0
replace carc_air2level = 1 if (carc_air > 0)

**# 3. teffects psmatch

// preserve

s

sample 5.6 if carc_on2level == 1

sample 30

count if carc_on2level == 1
	estadd scalar n_treat = r(N)

eststo psm: teffects psmatch (rate_spread) (carc_on2level i.purpose loan_to_value_ratio ln_income i.race i.metro_dummy cnty_unemp_rate loan_to_income), atet
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local lfe "No", replace
	estadd local lyfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	count if carc_on2level == 1
	estadd scalar n_treat = r(N)

eststo nnm: teffects nnmatch (rate_spread i.purpose loan_to_value_ratio ln_income i.race i.metro_dummy cnty_unemp_rate loan_to_income) (carc_on2level), atet
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local lfe "No", replace
	estadd local lyfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	count if carc_on2level == 1
	estadd scalar n_treat = r(N)

eststo ra: teffects ra (rate_spread i.purpose loan_to_value_ratio ln_income i.race i.metro_dummy cnty_unemp_rate loan_to_income) (carc_on2level), atet
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local lfe "No", replace
	estadd local lyfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	count if carc_on2level == 1
	estadd scalar n_treat = r(N)

tebalance overid

tebalance density ln_onsite_release_total
tebalance summarize

// restore
