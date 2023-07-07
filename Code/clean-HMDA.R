packages <- c("readxl","data.table","tidyverse","haven","zipcodeR", "httr","jsonlite")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

## Old code to save and load first data file
# hmda_raw <- as.data.table(read_dta(paste0(wd,hmda.folder,"hmda_2018-2021.dta")))
# hmda2018=hmda_raw[year==2018]
# hmda2019=hmda_raw[year==2019]
# hmda2020=hmda_raw[year==2020]
# hmda2021=hmda_raw[year==2021]
# save(hmda2018, file = file.path(paste0(wd,hmda.folder,"hmda2018.RData")))
# save(hmda2019, file = file.path(paste0(wd,hmda.folder,"hmda2019.RData")))
# save(hmda2020, file = file.path(paste0(wd,hmda.folder,"hmda2020.RData")))
# save(hmda2021, file = file.path(paste0(wd,hmda.folder,"hmda2021.RData")))

load(paste0(wd,hmda.folder,"hmda2018.RData"))
load(paste0(wd,hmda.folder,"hmda2019.RData"))
load(paste0(wd,hmda.folder,"hmda2020.RData"))
load(paste0(wd,hmda.folder,"hmda2021.RData"))
lapply(list(hmda2018,hmda2019,hmda2020,hmda2021), as.data.table)

#---- Compare HMDA and TRI data on ID ----
##---- Clean and Merge test ----
# Clean Test
mean(hmda2018[income > 0 & income < quantile(income, probs = 0.95, na.rm = T)][,income])

test2018 <- hmda2018[loan_term == 360][loan_to_value_ratio > 0 & loan_to_value_ratio < 100.01][rate_spread > -5 & rate_spread < 5][interest_rate < 15][income > 0 & income < quantile(income, probs = 0.95, na.rm = T)]
test2019 <- hmda2019[loan_term == 360][loan_to_value_ratio > 0 & loan_to_value_ratio < 100.01][rate_spread > -5 & rate_spread < 5][interest_rate < 15][income > 0 & income < quantile(income, probs = 0.95, na.rm = T)]
test2020 <- hmda2020[loan_term == 360][loan_to_value_ratio > 0 & loan_to_value_ratio < 100.01][rate_spread > -5 & rate_spread < 5][interest_rate < 15][income > 0 & income < quantile(income, probs = 0.95, na.rm = T)]
test2021 <- hmda2021[loan_term == 360][loan_to_value_ratio > 0 & loan_to_value_ratio < 100.01][rate_spread > -5 & rate_spread < 5][interest_rate < 15][income > 0 & income < quantile(income, probs = 0.95, na.rm = T)]

hist(hmda2018[rate_spread < 2 & rate_spread > -1][,rate_spread])
hist(hmda2018[income < 4*10^5 & income > 0][,income])

# Collect all unique tracts in HMDA 2018
hmda18_tracts <- test2018[,.N,census_tract][,census_tract]
hmda18_county <- test2018[,.N,st_cnty_fips][,st_cnty_fips]

# Merge data test
setkey(test2018,fips)
setkey(tri_match, fips)

## Summarise HMDA data by year-county
hmda_test <- rbind(test2018,test2019,test2020,test2021)
hmda_test[,year_fips := paste0(year,"-",st_cnty_fips)]
hmda_test_summary <- hmda_test[,lapply(.(loan_to_value_ratio,rate_spread,property_value,income),mean),by = year_fips]
colnames(hmda_test_summary) <- c("year_fips","ltv_ratio","rate_spread","property_value","income")

## Summarise TRI data by year-county
tri_test_summary <- tri_test[,lapply(.SD,mean), by = year_fips]
tri_test_summary <- tri_state[tri_test_summary, on = "year_fips"]
tri_test_summary[,c("i.fips","i.year") := NULL]

## Merge TRI and HMDA by year-county
panel_test <- hmda_test_summary[tri_test_summary, on= "year_fips"]
panel_test
write_dta(panel_test,path = paste0(wd,panel.folder,"panel_test.dta"))
