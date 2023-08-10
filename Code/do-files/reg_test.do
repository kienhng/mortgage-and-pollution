use "C:\Users\LLR User\OneDrive\3_Work_Research\uon-msc-thesis\Data\PanelData\panel_test.dta"

xtset fips year
destring state_code, replace

gen carc_release = total_releases*carcinogen

xtreg rate_spread total_releases ltv_ratio property_value income, fe robust

xtreg rate_spread carc_release ltv_ratio property_value income, fe robust