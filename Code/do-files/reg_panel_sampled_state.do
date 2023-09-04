* Name: reg_panel_sampled.do
* Author: Kien Hoang-Le
* Description: regression analysis on a sample panel

**# 1. Setting up environment

capture log close
clear
set linesize 80
set more off

*ssc install reghdfe
*ssc install ftools
*ssc install logout
*ssc install estout

dis "`c(username)'"

// Set WD

if "`c(username)'"=="HDC" {
	cd "C:\Users\kingd\OneDrive\uon-msc-thesis"
}

if "`c(username)'" == "LLR User" {
	cd "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis"
}

if "`c(username)'" == "lixkh23" {
	cd "C:\Users\lixkh23\OneDrive\3_Work_Research\uon-msc-thesis"
}

use "Data\PanelData\sampled_panel.dta", clear


destring year, replace
destring aland_cou houden_cou cnty_unemp_rate, replace

**## Data Labelling
label var interest_rate "Interest Rate"
label var rate_spread "Rate Spread"
label var us30_spread "U.S. 30-Year Treasury Rate Spread"
label var agency "Corresponding Regulatory Agency"
label var dec_loan_amount "Decile Loan Amount"
label var ln_loan_amount "Ln(Loan Amount)"
label var loan_to_value_ratio "Loan-to-Value Ratio"
label var dec_loan_to_value "Decile Loan-to-Value Ratio"
label var dec_property_value "Decile Property Value"
label var ln_property_value "Ln(Property Value)"
label var income "Income"
label var loan_to_income "Loan-to-Income Ratio"
label var dec_loan_to_income "Decile Loan-to-Income Ratio"
label var purpose "Loan Purpose"
label var race "Race"
label var caucasian "Caucasian Dummy"
label var age "Age"
label var gender "Gender"
label var bank "Bank Dummy"
label var dummy_purchaser "Purchaser Dummy"
label var fips "County FIPS"
label var state "State"
label var onsite_release_total "Total On-site Release"
label var total_releases "Total Releases"
label var carc_releases "Total Carcinogen Releases"
label var carc_onsite "Total On-site Carcinogen Releases"
label var air_releases "Total Air Releases"
label var carc_air "Total Carcinogen Air Releases"
label var carcinogen "Carcinogen Dummy"
label var pbt "Persistent Bioaccumulative and Toxic Dummy"
label var ln_total_releases "Ln(Total Releases)"
label var ln_onsite_release "Ln(Total On-site Release)"
label var ln_air_releases "Ln(Total Air Releases)"
label var ln_carc_releases "Carcinogen Exposure"
label var ln_carc_onsite "Carcinogen Exposure"
label var ln_carc_air "Carcinogen Air Exposure"
label var nfac_county "Number of facilities in County"
label var nfac_carc_county "Number of carcinogen-releases facilities in County"
label var metro_dummy "Metropolitan Dummy"
label var houden_cou "County Housing Density"
label var aland_cou "County Land Area"
label var aland_cou_sqkm "County Land Area (Squared Km)"
label var cnty_avg_income "County Average Income"
label var cnty_total_wage "County Total Wage"
label var cnty_unemp_rate "County Unemployment Rate"
label var cnty_labor_force "County Total Labor Force"

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

**## New Dummy Variables
g black = 0
replace black = 1 if (race == 3)

g covid = 0
replace covid = 1 if (year == 2020 | year == 2021)

g

egen med_income = median(income)
g income_dummy = 0
replace income_dummy = 1 if(income > med_income)

**## 2.2. Per Area Variables
g lnpa_carc_release = log(pa_carc_release)
g lnpa_carc_onsite = log(pa_carc_onsite)
g lnpa_carc_air = log(pa_carc_air)

label var lnpa_carc_release "Ln(Total Carcinogen Release per State Area)"
label var lnpa_carc_onsite "Ln(Total Carcinogen On-site Release per State Area)"
label var lnpa_carc_air "Ln(Total Carcinogen Air Release per State Area)"

**## 2.3. Create treatment variable:
**## Continuos Treatment Variables
drop ln_carc_releases
drop ln_carc_onsite
drop ln_total_releases

g other_release = total_releases - carc_releases
g ln_total_releases = log(total_releases)
g ln_carc_releases = log(carc_releases)
g ln_carc_onsite = log(carc_onsite)
g ln_other_release = log(total_releases - carc_releases)

label var ln_other_release "Other non-Carcinogen Release"
label var ln_total_releases "Total Releases"
label var ln_carc_releases "Carcinogen Exposure"
label var ln_carc_onsite "Carcinogen Exposure"

**### Carc Total Dummy
bys year: egen carc_med = median(carc_releases)
gen carc_dummy = 0
replace carc_dummy = 1 if carc_releases > carc_med

**### Carc Onsite Dummy
bys year: egen carc_on_med = median(carc_onsite)
gen carcon_dummy = 0
replace carcon_dummy = 1 if carc_onsite > carc_on_med

**### Carc Air Dummy
gen carcair_dummy = 0
replace carcair_dummy = 1 if (carc_air > 0)

label var carc_dummy "Carcinogen Exposure"
label var carcon_dummy "Carcinogen Exposure"
label var carcair_dummy "Carcinogen Exposure"

s
**## Summary Statistics
drop if loan_to_value_ratio > 1

tabstat carc_releases ln_carc_releases onsite_release_total ln_onsite_release carc_air ln_carc_air aland_cou houden_cou cnty_unemp_rate, statistics(count mean sd p25 p50 p75 min max) col(statistics)

tabstat rate_spread interest_rate loan_to_value_ratio income ln_income property_value ln_property_value age gender bank purpose, statistics(count mean sd p25 p50 p75 min max) col(statistics)

**# 3. Baseline Model
**## 3.1. Rate Spread
**### Year State FE Rate Spread
eststo bl1: reghdfe rate_spread c.ln_carc_releases i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year i.state) cluster(fips)
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**### Year##State FE Rate Spread
eststo bl2: reghdfe rate_spread c.ln_carc_releases i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year#i.state) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**### Year State Lei FE Rate Spread
eststo bl3: reghdfe rate_spread c.ln_carc_releases i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year#i.state i.lei) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local lfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**## 3.2. Interest Rate
**### Year State FE Interest Rate
eststo bl_ir1: reghdfe interest_rate c.ln_carc_releases i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year i.state) cluster(fips)
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**### Year#State FE Interest Rate
eststo bl_ir2: reghdfe interest_rate c.ln_carc_releases i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year#i.state) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**### Year State Lei FE Interest Rate
eststo bl_ir3: reghdfe interest_rate c.ln_carc_releases i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year#i.state i.lei) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local lfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

	
**# 4. Robustness check
**## 4.1. Using US_30 tresuary
**### Year State FE Rate Spread
eststo rb1: reghdfe us30_spread c.ln_carc_air i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year i.state) cluster(fips)
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**### Year##State FE Rate Spread
eststo rb2: reghdfe us30_spread c.ln_carc_air i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year#i.state) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**### Year State Lei FE Rate Spread
eststo rb3: reghdfe us30_spread c.ln_carc_air i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year#i.state i.lei) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local lfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**### Year State FE Interest Rate
eststo rb_ir1: reghdfe interest_rate c.ln_carc_air i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year i.state) cluster(fips)
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**### Year##State FE Interest Rate
eststo rb_ir2: reghdfe interest_rate c.ln_carc_air i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year#i.state) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**### Year State Lei FE Interest Rate
eststo rb_ir3: reghdfe interest_rate c.ln_carc_air i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value i.metro_dummy aland_cou houden_cou cnty_unemp_rate ln_other_release, absorb(i.year#i.state i.lei) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local lfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
	
**## 4.2 Running PSM for robustness check
*do "Code\do-files\matching.do"

**# 5. Additional analysis
**## 5.1 Using High-Low treatment
**### Year State FE Rate Spread
// eststo hilo1: reghdfe rate_spread i.carc_dummy i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state) cluster(fips)
// 	estadd local ysfe "Yes", replace
//  	estadd local lfe "No", replace
// 	estadd local clstr "Yes", replace
// 	sum `e(depvar)' if e(sample)
// 	estadd scalar Mean = r(mean)
//
// **### Year State Lei FE Rate Spread
// eststo hilo2: reghdfe rate_spread i.carc_dummy i.purpose i.bank  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state i.lei) cluster(fips)
// 	estadd local ysfe "Yes", replace
//  	estadd local lfe "Yes", replace
// 	estadd local clstr "Yes", replace
// 	sum `e(depvar)' if e(sample)
// 	estadd scalar Mean = r(mean)
//	
// **### Year State FE Interest Rate
// eststo hilo3: reghdfe interest_rate i.carc_dummy i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state) cluster(fips)
// 	estadd local ysfe "Yes", replace
//  	estadd local lfe "No", replace
// 	estadd local clstr "Yes", replace
// 	sum `e(depvar)' if e(sample)
// 	estadd scalar Mean = r(mean)
//
// **### Year State Lei FE Interest Rate
// eststo hilo4: reghdfe interest_rate i.carc_dummy i.purpose i.bank  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state i.lei) cluster(fips)
// 	estadd local ysfe "Yes", replace
//  	estadd local lfe "Yes", replace
// 	estadd local clstr "Yes", replace
// 	sum `e(depvar)' if e(sample)
// 	estadd scalar Mean = r(mean)

**## 5.2 Interact with Purpose
**### Year State FE Rate Spread
eststo itr_purp1: reghdfe rate_spread c.ln_carc_releases##i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state) cluster(fips)
	estadd local ysfe "Yes", replace
 	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**### Year State Lei FE Rate Spread
eststo itr_purp2: reghdfe rate_spread c.ln_carc_releases##i.purpose i.bank  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state i.lei) cluster(fips)
	estadd local ysfe "Yes", replace
 	estadd local lfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**### Year State FE Interest Rate
eststo itr_purp3: reghdfe interest_rate c.ln_carc_releases##i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state) cluster(fips)
	estadd local ysfe "Yes", replace
 	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**### Year State Lei FE Interest Rate
eststo itr_purp4: reghdfe interest_rate c.ln_carc_releases##i.purpose i.bank  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state i.lei) cluster(fips)
	estadd local ysfe "Yes", replace
 	estadd local lfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**## 5.3 Interact with Bank
**### Year State FE Rate Spread
eststo itr_bank1: reghdfe rate_spread c.ln_carc_releases##i.bank i.purpose  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state) cluster(fips)
	estadd local ysfe "Yes", replace
 	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)


**### Year State Lei FE Rate Spread
eststo itr_bank2: reghdfe rate_spread c.ln_carc_releases##i.bank i.purpose  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state i.lei) cluster(fips)
	estadd local ysfe "Yes", replace
 	estadd local lfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**### Year State FE Interest Rate
eststo itr_bank3: reghdfe interest_rate c.ln_carc_releases##i.bank i.purpose  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state) cluster(fips)
	estadd local ysfe "Yes", replace
 	estadd local lfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)


**### Year State Lei FE Interest Rate
eststo itr_bank4: reghdfe interest_rate c.ln_carc_releases##i.bank i.purpose  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_total_releases, absorb(i.year##i.state i.lei) cluster(fips)
	estadd local ysfe "Yes", replace
 	estadd local lfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

	
**## 5.5 Interact with Race
**### Year State FE Rate Spread
eststo itr_race1: reghdfe rate_spread c.ln_carc_releases##i.black i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year i.state) cluster(fips)
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)

**### Year State FE Rate Spread
eststo itr_race2: reghdfe rate_spread c.ln_carc_releases##i.black i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**### Year State FE Interest Rate
eststo itr_race3: reghdfe interest_rate c.ln_carc_releases##i.black i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year i.state) cluster(fips)
	estadd local yfe "Yes", replace
	estadd local sfe "Yes", replace
	estadd local ysfe "No", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	
**### Year State FE Interest Rate
eststo itr_race4: reghdfe interest_rate c.ln_carc_releases##i.black i.purpose i.bank loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou i.metro_dummy cnty_unemp_rate ln_other_release, absorb(i.year##i.state) cluster(fips)
	estadd local yfe "No", replace
	estadd local sfe "No", replace
	estadd local ysfe "Yes", replace
	estadd local clstr "Yes", replace
	sum `e(depvar)' if e(sample)
	estadd scalar Mean = r(mean)
	

	
// **## 5.4 DiD with 2020 Covid
// **### Year State FE
// eststo add_did1: reghdfe rate_spread c.ln_carc_releases c.ln_carc_releases#i.covid i.purpose i.bank  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income##c.c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou  aland_cou houden_cou cnty_unemp_rate, absorb(i.year i.state) cluster(fips)
// 	estadd local yfe "Yes", replace
// 	estadd local sfe "Yes", replace
// 	estadd local ysfe "No", replace
// // 	estadd local lfe "No", replace
// // 	estadd local lyfe "No", replace
// 	estadd local clstr "Yes", replace
// 	sum `e(depvar)' if e(sample)
// 	estadd scalar Mean = r(mean)
//
// **### Year##State FE
// eststo add_did2: reghdfe rate_spread c.ln_carc_releases c.ln_carc_releases#i.covid  i.purpose i.bank  loan_to_value_ratio c.age##c.age c.ln_income##c.ln_income##c.c.ln_income##c.ln_income i.gender i.race ln_property_value aland_cou houden_cou  aland_cou houden_cou cnty_unemp_rate, absorb(i.year##i.state) cluster(fips)
// 	estadd local yfe "Yes", replace
// 	estadd local sfe "Yes", replace
// 	
// // 	estadd local lfe "No", replace
// // 	estadd local lyfe "No", replace
// 	estadd local clstr "Yes", replace
// 	sum `e(depvar)' if e(sample)
// 	estadd scalar Mean = r(mean)

s

**# 6. Table Export
 





