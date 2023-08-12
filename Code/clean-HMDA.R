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
## Create the outcome variable based on US30 yield
hmda2018[,us30_spread := interest_rate - US30Y.2018]
hmda2019[,us30_spread := interest_rate - US30Y.2019]
hmda2020[,us30_spread := interest_rate - US30Y.2020]
hmda2021[,us30_spread := interest_rate - US30Y.2021]

## Create the variable for FHFA conforming loan limit value
hmda2018[,over_conflimit := ifelse((loan_amount-CONF.LOAN.LIMIT.2018)>0,2,1)]
hmda2019[,over_conflimit := ifelse((loan_amount-CONF.LOAN.LIMIT.2019)>0,2,1)]
hmda2020[,over_conflimit := ifelse((loan_amount-CONF.LOAN.LIMIT.2020)>0,2,1)]
hmda2021[,over_conflimit := ifelse((loan_amount-CONF.LOAN.LIMIT.2021)>0,2,1)]

## Create the variable for HMDA states
hmda2018[,st:=substr(st_cnty_fips,1,nchar(st_cnty_fips)-3)]
hmda2019[,st:=substr(st_cnty_fips,1,nchar(st_cnty_fips)-3)]
hmda2020[,st:=substr(st_cnty_fips,1,nchar(st_cnty_fips)-3)]
hmda2021[,st:=substr(st_cnty_fips,1,nchar(st_cnty_fips)-3)]

raw_list <- list(hmda2018,hmda2019,hmda2020,hmda2021)
new_list <- list(1:4)

for (i in 1:4) {
  new_list[[i]] <- raw_list[[i]][!(st%in%c("60","66","69","72","78")) # Remove mainland-US territories
                                 ][loan_term == 360
                                 ][,loan_to_value_ratio := loan_amount/property_value
                                   ][loan_to_value_ratio > 0 & loan_to_value_ratio < 100 | loan_to_value_ratio == 100
                                     ][income > 0 & income < quantile(income, probs = 0.99, na.rm = T)
                                       ][property_value < quantile(property_value, probs = 0.99, na.rm = T)
                                         & property_value > quantile(property_value, probs = 0.01, na.rm = T)
                                         ][rate_spread < quantile(rate_spread, probs = 0.99, na.rm = T)
                                           & rate_spread > quantile(rate_spread, probs = 0.01, na.rm = T)
                                           ][interest_rate < 15
                                             # New variable loan_to_income
                                                 ][,loan_to_income := loan_amount/income
                                                   # Binning variables into deciles
                                                 ][,dec_loan_amount := cut(loan_amount,
                                                                           quantile(loan_amount,probs=seq(1,0,by=-0.1)),
                                                                           include.lowest=T,labels=F)
                                                   ][,loan_to_value_ratio := loan_to_value_ratio + rnorm(length(loan_to_value_ratio))*1e-10
                                                   ][,dec_loan_to_value := cut(loan_to_value_ratio,
                                                                               quantile(loan_to_value_ratio,probs=seq(1,0,by=-0.1)),
                                                                               include.lowest=T,labels=F)
                                                   ][,dec_income := cut(income, 
                                                                        quantile(income,probs=seq(1,0,by=-0.1)),
                                                                        include.lowest=T,labels=F)
                                                   ][,dec_property_value := cut(property_value,
                                                                                quantile(property_value,probs=seq(1,0,by=-0.1)),
                                                                                include.lowest=T,labels=F)
                                                   ][,dec_loan_to_income := cut(loan_to_income, 
                                                                                quantile(loan_to_income,probs=seq(1,0,by=-0.1)),
                                                                                include.lowest=T,labels=F)]
}

gc()
hmda2018_cl <- new_list[[1]]
hmda2019_cl <- new_list[[2]]
hmda2020_cl <- new_list[[3]]
hmda2021_cl <- new_list[[4]]

## Create subset of HMDA
hmda_cl <- rbind(hmda2018_cl,hmda2019_cl,hmda2020_cl,hmda2021_cl)
rm(new_list,raw_list,hmda2018,hmda2019,hmda2020,hmda2021,hmda2018_cl,hmda2019_cl,hmda2020_cl,hmda2021_cl)
gc()

hmda_cl[,year_fips := paste0(year,"-",st_cnty_fips)]
setkey(hmda_cl, year_fips)
## Wrangling HMDA data
hmda_match <- hmda_cl[,.(year,lei,census_tract,year_fips,rate_spread,us30_spread,
                         applicant_race1,applicant_age,
                         #derived_sex,
                         loan_amount,dec_loan_amount,
                         loan_to_value_ratio,dec_loan_to_value,
                         property_value,dec_property_value,
                         income,dec_income,
                         loan_to_income,dec_loan_to_income,
                         purpose,
                         over_conflimit)]

## Relabel data
hmda_match[,race := 1]
hmda_match[applicant_race1 %in% c(2,21,22,23,24,25,26,27), race := 2]
hmda_match[applicant_race1 == 3, race := 3]
hmda_match[applicant_race1 %in% c(4,41,42,43,44), race := 4]
hmda_match[applicant_race1 == 5, race := 5]
hmda_match[applicant_race1 %in% c(6,7), race := NA]
hmda_match[is.na(applicant_race1),race := NA]

hmda_match[,applicant_race1 := NULL]

# hmda_match[derived_sex == "Female", derived_sex := 1]
# hmda_match[derived_sex == "Male", derived_sex := 2]
# hmda_match[derived_sex == "Joint", derived_sex := 3]
# hmda_match[derived_sex == "Sex Not Available", derived_sex := NA]

hmda_match[applicant_age == "<25", age := 1]
hmda_match[applicant_age == "25-34", age := 2]
hmda_match[applicant_age == "35-44", age := 3]
hmda_match[applicant_age == "45-54", age := 4]
hmda_match[applicant_age == "55-64", age := 5]
hmda_match[applicant_age == "65-74", age := 6]
hmda_match[applicant_age == ">74", age := 7]
hmda_match[applicant_age == "8888", age := NA]
hmda_match[applicant_age == "9999", age := NA]
hmda_match[,applicant_age := NULL]

## Change data into factor variables
hmda_match[,race := as.factor(race)]
hmda_match[,age := as.factor(age)]
hmda_match[,purpose := as.factor(purpose)]
hmda_match[,over_conflimit := as.factor(over_conflimit)]

# Export HMDA sample
saveRDS(hmda_match,file = paste0(wd,hmda.folder,"hmda_match.rds"))
saveRDS(hmda_cl,file = paste0(wd,hmda.folder,"hmda_clean.rds"))
