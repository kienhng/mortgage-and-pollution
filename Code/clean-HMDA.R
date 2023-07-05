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

wd <- "C:/Users/LLR User/OneDrive/3_Work_Research/uon-msc-thesis"
hmda.folder <- "/Data/RawData-HMDA/"

load(paste0(wd,hmda.folder,"hmda2018.RData"))
load(paste0(wd,hmda.folder,"hmda2019.RData"))
load(paste0(wd,hmda.folder,"hmda2020.RData"))
load(paste0(wd,hmda.folder,"hmda2021.RData"))
lapply(list(hmda2018,hmda2019,hmda2020,hmda2021), as.data.table)

#---- Compare HMDA and TRI data on ID ----
##---- Clean and Merge test ----
# Clean Test
test2018 <- hmda2018[loan_term == 360][loan_to_value_ratio > 0 & loan_to_value_ratio < 100.01][rate_spread > -5 & rate_spread < 5][interest_rate < 15][income > 0]

# Collect all unique tracts in HMDA 2018
hmda18_tracts <- test2018[,.N,census_tract][,census_tract]
hmda18_county <- test2018[,.N,st_cnty_fips][,st_cnty_fips]