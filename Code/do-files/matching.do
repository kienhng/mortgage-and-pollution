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
	cd "C:\Users\kingd\OneDrive\mortgage-and-pollution"
}

if "`c(username)'" == "lekie" {
	cd "C:\Users\lekie\OneDrive\3_Work_Research\mortgage-and-pollution"
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
gen carc_2level = 1
replace carc_2level = 0 if (carc_releases > 0) //Treatment

s

// sample 5.5 if carc_2level == 1

sample 20 if carc_2level == 0

count if carc_2level == 1
	scalar define n_treat = r(N)

eststo psm: teffects psmatch (rate_spread) (carc_2level dec_property_value dec_loan_to_value dec_income i.purpose i.age i.gender i.race i.region), atet
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local lfe "No", replace
	estadd local lyfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	estadd scalar n_treat

tebalance summarize
tebalance density

// restore
