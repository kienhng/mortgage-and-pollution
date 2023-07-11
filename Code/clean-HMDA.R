packages <- c("readxl","data.table","tidyverse","haven","zipcodeR", "httr","jsonlite")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

# Old code to save and load first data file
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

#---- Subset HMDA data ----
raw_list <- list(hmda2018,hmda2019,hmda2020,hmda2021)
new_list <- list(1:4)

for (i in 1:4) {
  new_list[[i]] <- raw_list[[i]][loan_term == 360
                                 ][loan_to_value_ratio > 0 & loan_to_value_ratio < 100.01
                                   ][rate_spread > -4 & rate_spread < 4
                                     ][interest_rate < 15
                                       ][income > 0 & income < quantile(income, probs = 0.999, na.rm = T)
                                         ][!(applicant_age %in% c("8888","9999", NA))
                                           ][property_value < quantile(property_value, probs = 0.999, na.rm = T)
                                             ]
}

hmda2018_samp <- new_list[[1]]
hmda2019_samp <- new_list[[2]]
hmda2020_samp <- new_list[[3]]
hmda2021_samp <- new_list[[4]]

## Create subset of HMDA
hmda_samp <- rbind(hmda2018_samp,hmda2019_samp,hmda2020_samp,hmda2021_samp)
hmda_samp[,year_fips := paste0(year,"-",st_cnty_fips)]

# Export HMDA sample
saveRDS(hmda_samp,file = paste0(wd,panel.folder,"hmda_samp.rds"))