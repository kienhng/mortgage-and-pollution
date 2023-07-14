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
                                 ][loan_to_value_ratio > 0 & loan_to_value_ratio < 100.001
                                   ][rate_spread > -4 & rate_spread < 4
                                     ][interest_rate < 15
                                       ][income > 0 & income < quantile(income, probs = 0.999, na.rm = T)
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
setkey(hmda_samp, year_fips)

## Wrangling HMDA data
hmda_match <- hmda_samp[,.(year,year_fips,census_tract,derived_race,derived_sex,type_of_purchaser,
                         loan_amount,loan_to_value_ratio,interest_rate,rate_spread,
                         property_value,income,applicant_race1,applicant_age,agency,
                         purpose)]

## Rescale
hmda_match[,loan_amount := log(loan_amount)]
hmda_match[,property_value := log(property_value)]
hmda_match[,income := log(income)]

## Relabel data
hmda_match[,race := ifelse(derived_race == "White",1,2)]
hmda_match[derived_race == "Joint", race := 3]
hmda_match[derived_race == "Free Form Text Only", race := NA]
hmda_match[,derived_race := NULL]

hmda_match[derived_sex == "Female", derived_sex := 1]
hmda_match[derived_sex == "Male", derived_sex := 2]
hmda_match[derived_sex == "Joint", derived_sex := 3]
hmda_match[derived_sex == "Sex Not Available", derived_sex := NA]

hmda_match[applicant_age == "<25", applicant_age := 1]
hmda_match[applicant_age == "25-34", applicant_age := 2]
hmda_match[applicant_age == "35-44", applicant_age := 3]
hmda_match[applicant_age == "45-54", applicant_age := 4]
hmda_match[applicant_age == "55-64", applicant_age := 5]
hmda_match[applicant_age == "65-74", applicant_age := 6]
hmda_match[applicant_age == ">74", applicant_age := 7]
hmda_match[applicant_age == "8888", applicant_age := NA]
hmda_match[applicant_age == "9999", applicant_age := NA]

## Change data into factor variables
hmda_match[,race := as.factor(race)]
hmda_match[,derived_sex := as.factor(derived_sex)]
hmda_match[,type_of_purchaser := as.factor(type_of_purchaser)]
hmda_match[,applicant_race1 := as.factor(applicant_race1)]
hmda_match[,applicant_age := as.factor(applicant_age)]
hmda_match[,agency := as.factor(agency)]
hmda_match[,purpose := as.factor(purpose)]

# Export HMDA sample
saveRDS(hmda_match,file = paste0(wd,panel.folder,"hmda_match.rds"))
saveRDS(hmda_samp,file = paste0(wd,hmda.folder,"hmda_sample.rds"))
