* Name: reg_panel_sampled.do
* Author: Kien Hoang-Le
* Description: regression analysis on a 20%-sample of the full panel

**# 1. Setting up environment

capture log close
clear
set linesize 80
set more off

*ssc install reghdfe
*ssc install ftools

use "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis\Data\PanelData\sampled_panel.dta" 

**# 2. Create new variables
**## 2.1. Encoding
encode year_fips, gen(year_fip1)
drop year_fips
rename year_fip1 year_fips

destring year, replace

**## 2.2. Create treatment variable:
*keep if (effect_10km > aland_cou)
gen carc_2level = 0
replace carc_2level = 1 if (carc_releases > 0 & carc_effect_10km > aland_cou) //Treatment

gen carc_ex2level = 0
replace carc_ex2level = 1 if (carc_releases > 0)

*gen carc_re2level_extreme = 0 if carc_re4level == 1
*replace carc_re2level_extreme = 1 if carc_re4level == 3
*keep if (carc_re4level == 1 | carc_re4level == 3)

gen carc_re4level = 0 if carc_releases == 0
replace carc_re4level = 1 if carc_releases > 0
replace carc_re4level = 2 if carc_releases > 8.35899 //p25
replace carc_re4level = 3 if carc_releases >  10.57929 //p50
replace carc_re4level = 4 if carc_releases > 11.98792 //p75

**## 2.3. Create treatment variable: air releases
gen carc_air2level = 0
replace carc_air2level = 1 if (carc_air > 0 & aland_cou/(effect_5km*0.9) > 1)
* replace carc_air2level = 2 if carc_air >  2.407872  //median

**# 3. Running analysis

reg rate_spread i.carc_2level total_releases dec_property_value aland_pct_urb nfac_county aland_cou i.dec_loan_to_income##i.dec_income i.year, cluster(year_fips)

reghdfe rate_spread i.carc_2level i.dec_property_value i.dec_loan_to_value##i.dec_loan_to_income i.dec_income i.race i.over_conflimit, absorb(year fips lei) cluster(year_fips)

teffects psmatch (rate_spread) (carc_2level dec_income aland_pct_urb dec_property_value dec_loan_to_income), atet, if (region == "Southeast" & year == 2021)

tebalance density
tebalance summarize
