* Name: panel_sampled.do
* Author: Kien Hoang-Le
* Description: regression analysis on a 20%-sample of the full panel

use "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis\Data\PanelData\reg_panel_sampled.dta" 

gen carc_4level = 0 if carc_per_area == 0
replace carc_4level = 1 if carc_per_area > 0
replace carc_4level = 2 if carc_per_area > 0.00000173 //p25
replace carc_4level = 3 if carc_per_area > .0000154 //p50
replace carc_4level = 4 if carc_per_area >  .0000562 //p75

gen carc_2level = 0 if carc_level == 1
replace carc_2level = 1 if carc_level == 0

gen carc_air4level = 0 if carc_air_pa == 0
replace carc_air4level = 1 if carc_air_pa > 0
replace carc_air4level = 2 if carc_air_pa > 0.0000000488 //p25
replace carc_air4level = 3 if carc_air_pa > 0.00000121 //p50
replace carc_air4level = 4 if carc_air_pa > 0.00000662 //p75


reg rate_spread i.carc_4level loan_to_value_ratio income loan_amount aland_pct_urb, robust

reg rate_spread i.carc_air4level loan_to_value_ratio property_value aland_pct_urb nfac_county c.income##i.applicant_age##i.race i.year, robust

reg rate_spread i.carc_4level loan_to_value_ratio property_value aland_pct_urb nfac_county c.income##i.applicant_age##i.race i.year, robust

