* Name: reg_panel_sampled.do
* Author: Kien Hoang-Le
* Description: regression analysis on a sample of the full panel

**# 1. Setting up environment

capture log close
clear
set linesize 80
set more off

// ssc install reghdfe
// ssc install ftools

// Kien:
// use "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis\Data\PanelData\sampled_panel.dta" 

// Chinh:
use "C:\Users\kingd\OneDrive\D\DOCUMENTS\OOUeb\5. Kien's Thesis\uon-msc-thesis\Data\sampled_panel.dta", clear

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

**## 2.2. Log-transformation:
gen lncarc_pa = log(pa_carc_release)
gen lncarc_on_pa = log(pa_carc_onsite)
gen lncarc_air = log(pa_carc_air)

**## 2.3. Create treatment variable:
**### Carc Total

gen carc_2level = 0
replace carc_2level = 1 if (carc_releases > 0) //Treatment

**### Carc Onsite

gen carc_on2level = 1
replace carc_on2level = 0 if (pa_carc_onsite > 0) //Treatment

**### Carc Air
gen carc_air2level = 0
replace carc_air2level = 1 if (carc_air > 0)

**# 3. Running analysis
**reghdfe rate_spread ln_carc_onsite purpose over_conflimit loan_to_value_ratio aland_cou popden_cou , absorb(i.year i.dec_loan_to_income##i.dec_loan_to_value##i.race##i.age i.dec_property_value##i.urbru_class i.state)

reghdfe rate_spread c.lncarc_on_pa ln_total_releases purpose over_conflimit loan_to_value_ratio loan_to_income aland_cou popden_cou unemp_rate county_median_income, absorb(i.year##i.state##i.dec_loan_to_value##i.dec_property_value##i.dec_income##i.race##i.property_urb_ru) vce(cluster fips)

reghdfe rate_spread c.lncarc_on_pa##i.race ln_total_releases purpose over_conflimit loan_to_value_ratio loan_to_income aland_cou popden_cou unemp_rate county_median_income, absorb(i.year i.state i.dec_loan_to_value##i.dec_property_value##i.dec_income##i.race##i.property_urb_ru) vce(cluster state)


**# 4. Running PSM for robustness check
sample 10

teffects psmatch (rate_spread) (carc_on2level dec_income dec_loan_to_income dec_loan_to_value), atet

tebalance density
tebalance summarize
