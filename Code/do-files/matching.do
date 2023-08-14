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

use "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis\Data\PanelData\psm_sample.dta" 

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

**## 2.2. Create treatment variable:
**### Carc Total
gen carc_2level = 0
replace carc_2level = 1 if (carc_releases > 0) //Treatment

**### Carc Onsite
gen lncarc_on_pa = log(pa_carc_onsite)

gen carc_on2level = 1
replace carc_on2level = 0 if (carc_onsite > 0) //Treatment

gen carc_on4level = 0
replace carc_on4level = 1 if (carc_onsite > 0) //0
replace carc_on4level = 2 if (carc_onsite > 72.77791) //p25
replace carc_on4level = 3 if (carc_onsite > 324.262) //p50
replace carc_on4level = 4 if (carc_onsite > 931.1233) //p75

**### Carc Air
gen carc_air2level = 0
replace carc_air2level = 1 if (carc_air > 0)

**# 3. teffects psmatch
teffects psmatch (rate_spread) (carc_on2level loan_to_income loan_to_value_ratio race property_urb_ru), atet

tebalance density
tebalance summarize