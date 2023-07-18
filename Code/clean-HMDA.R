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
## Create the second outcome variable
hmda2018[,us30_spread := interest_rate - US30Y.2018]
hmda2019[,us30_spread := interest_rate - US30Y.2019]
hmda2020[,us30_spread := interest_rate - US30Y.2020]
hmda2021[,us30_spread := interest_rate - US30Y.2021]

raw_list <- list(hmda2018,hmda2019,hmda2020,hmda2021)
new_list <- list(1:4)

for (i in 1:4) {
  new_list[[i]] <- raw_list[[i]][loan_term == 360
                                 ][loan_to_value_ratio > 0 & loan_to_value_ratio < 100.001
                                   ][rate_spread > -4 & rate_spread < 4
                                     ][interest_rate < 15
                                       ][income > 0 & income < quantile(income, probs = 0.999, na.rm = T)
                                           ][property_value < quantile(property_value, probs = 0.999, na.rm = T)
                                             # New variable loan_to_income
                                             ][,loan_to_income := loan_amount/income
                                               # Binning variables into deciles
                                               ][,loan_to_value_ratio := loan_to_value_ratio + rnorm(length(loan_to_value_ratio)) * 1e-10
                                               ][,dec_loan_to_value := cut(loan_to_value_ratio,
                                                                           quantile(loan_to_value_ratio,probs=seq(0,1,by=0.1)),
                                                                           include.lowest=T,labels=F)
                                               ][,dec_income := cut(income, 
                                                                    quantile(income,probs=seq(0,1,by=0.1)),
                                                                    include.lowest=T,labels=F)
                                               ][,dec_property_value := cut(property_value,
                                                                            quantile(property_value,probs=seq(0,1,by=0.1)),
                                                                            include.lowest=T,labels=F)
                                               ][,dec_loan_to_income := cut(loan_to_income, 
                                                                            quantile(loan_to_income,probs=seq(0,1,by=0.1)),
                                                                            include.lowest=T,labels=F)]
}

hmda2018_cl <- new_list[[1]]
hmda2019_cl <- new_list[[2]]
hmda2020_cl <- new_list[[3]]
hmda2021_cl <- new_list[[4]]

## Create subset of HMDA
hmda_cl <- rbind(hmda2018_cl,hmda2019_cl,hmda2020_cl,hmda2021_cl)
hmda_cl[,year_fips := paste0(year,"-",st_cnty_fips)]
setkey(hmda_cl, year_fips)

## Wrangling HMDA data
hmda_match <- hmda_cl[,.(year,year_fips,rate_spread,us30_spread,
                         applicant_race1,derived_sex,applicant_age,
                         loan_amount,
                         loan_to_value_ratio,dec_loan_to_value,
                         property_value,dec_property_value,
                         income,dec_income,
                         loan_to_income,dec_loan_to_income,
                         purpose)]

## Relabel data
hmda_match[,race := ifelse(applicant_race1 == "White",1,2)]
hmda_match[applicant_race1 == "Joint", race := 3]
hmda_match[applicant_race1 == "Free Form Text Only", race := NA]
hmda_match[,applicant_race1 := NULL]

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
hmda_match[,applicant_age := as.factor(applicant_age)]
hmda_match[,purpose := as.factor(purpose)]

# Export HMDA sample
saveRDS(hmda_match,file = paste0(wd,hmda.folder,"hmda_match.rds"))
saveRDS(hmda_cl,file = paste0(wd,hmda.folder,"hmda_clean.rds"))
