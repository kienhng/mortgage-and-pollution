<!> Running global-constants script before doing anything <!>


The process to build a joined panel data from HMDA data (mortgage data) and TRI data (industrial pollution data)

1/ Clean HMDA data: clean-HMDA.R script
- The final result is a subset of the HMDA dataset with 5.7mil obs over 4 years and 30 variables
- The geo identifier is FIPS code

2/ Clean TRI data: clean-TRI.R script
- The data TRI data does not have FIPS code, but the county names.
- So I need to obtain a FIPS dataset (from the US Census Bureau), clean the TRI county names and assign the FIPS for TRI using county names.
=> The result is a TRI dataset with FIPS code as geo identifier.

3/ Match HMDA data and TRI data based on county FIPS code: generate-reg-panel-weighted.R
- Before matching, the TRI data need to be aggregated on the county level

(*) Other scripts:

4/ Visualisation (charts and maps): using visualisation-chart.R and visualisation-map.R scripts

5/ Folder do-files contain Stata code to run regression analysis and matching
