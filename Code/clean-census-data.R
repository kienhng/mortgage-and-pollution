packages <- c("data.table","rgdal","tidyverse","stringr","ggplot2","haven","zipcodeR","readxl","tigris")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

## **Note**
## The census data include 3 components:
### 1/ Tract level data: rural/urban classification
### 2/ County Geography: Area and Population of county
### 3/ County Income: Total wage and average wage (annual) of county
### 4/ County Unemployment rate: Labor force and Unemp rate of county

## Tract data
tract_raw <- as.data.table(read.csv(paste0(wd,census.folder,"tract_census2010.csv")))

## Geography data
cnty_geography <- as.data.table(read_excel(paste0(wd,census.folder,"/county_geography.xlsx")))

## Wage data
cnty_wage2018 <- as.data.table(read_excel(paste0(wd,census.folder,"/allhlcn18.xlsx"),sheet = "US_St_Cn_MSA"))
cnty_wage2019 <- as.data.table(read_excel(paste0(wd,census.folder,"/allhlcn19.xlsx"),sheet = "US_St_Cn_MSA"))
cnty_wage2020 <- as.data.table(read_excel(paste0(wd,census.folder,"/allhlcn20.xlsx"),sheet = "US_St_Cn_MSA"))
cnty_wage2021 <- as.data.table(read_excel(paste0(wd,census.folder,"/allhlcn21.xlsx"),sheet = "US_St_Cn_MSA"))

## Employment data
cnty_emp2018 <- as.data.table(read_excel(paste0(wd,census.folder,"/laucnty18.xlsx")))
cnty_emp2019 <- as.data.table(read_excel(paste0(wd,census.folder,"/laucnty19.xlsx")))
cnty_emp2020 <- as.data.table(read_excel(paste0(wd,census.folder,"/laucnty20.xlsx")))
cnty_emp2021 <- as.data.table(read_excel(paste0(wd,census.folder,"/laucnty21.xlsx")))

#---- 1. Clean tract data ----
colnames(tract_raw) <- c("fips","state","cnty","census_tract","primary_ruca_code","secondary_ruca_code",
                         "tract_pop","aland_tract","popden_tract")

##---- 1.1. Change the primary RUCA code ----
## The rules are:
## 1,2,3 are metropolitan = 1
## 4,5,6 are micropolitan = 2
## 7,8,9 are small towns = 3
## 10 is rural = 4
## 99 is NA

tract_raw[primary_ruca_code %in% c(1,2,3),metro_dummy := 1] ## Metropolitan
tract_raw[primary_ruca_code %in% c(4,5,6),metro_dummy := 2] ## Micropolitan
tract_raw[primary_ruca_code %in% c(7,8,9),metro_dummy := 2] ## Small town
tract_raw[primary_ruca_code == 10,metro_dummy := 2] ## Rural
tract_raw[primary_ruca_code == 99,metro_dummy := NA]

##---- 1.2. Collect needed tract variable ----
tract_dat <- tract_raw[,.(census_tract,metro_dummy)]

#---- 2. Clean cnty data ----
##---- 2.1. clean geography data and demographic data ----
### Geography data
cnty_geography[,fips := paste0(STATE,COUNTY)]
cnty_geography <- cnty_geography[,.(fips,REGION,HOUDEN_COU,ALAND_COU)]
colnames(cnty_geography) <- str_to_lower(colnames(cnty_geography))
cnty_geography[,aland_cou_sqkm:= aland_cou/1000000]

### Wage data
cnty_wage_raw <- rbind(cnty_wage2018,cnty_wage2019,cnty_wage2020,cnty_wage2021)
names(cnty_wage_raw)[1] <- "fips"

cnty_wage_raw[,year_fips := paste0(Year,"-",fips)]
cnty_wage_raw <- cnty_wage_raw[,c("fips","Year","year_fips","Area Type","Ownership","Industry",
                                "Annual Average Pay","Annual Total Wages")]
colnames(cnty_wage_raw) <- c("fips","year","year_fips","area_type","ownership","industry","cnty_avg_income","cnty_total_wage")

cnty_wage_raw <- cnty_wage_raw[area_type == "County"][ownership == "Total Covered"]
cnty_wage <- cnty_wage_raw[,.(year_fips,fips,cnty_avg_income,cnty_total_wage)]

### Unemployment rate data
cnty_emp2018 <- cnty_emp2018[-c(1:5)]
cnty_emp2019 <- cnty_emp2019[-c(1:5)]
cnty_emp2020 <- cnty_emp2020[-c(1:5)]
cnty_emp2021 <- cnty_emp2021[-c(1:5)]

colnames(cnty_emp2018) <- c("laus_code","st_code","cnty_code","cnty_st","year","Non","cnty_labor_force","cnty_employed","cnty_unemployed","cnty_unemp_rate")
colnames(cnty_emp2019) <- c("laus_code","st_code","cnty_code","cnty_st","year","Non","cnty_labor_force","cnty_employed","cnty_unemployed","cnty_unemp_rate")
colnames(cnty_emp2020) <- c("laus_code","st_code","cnty_code","cnty_st","year","Non","cnty_labor_force","cnty_employed","cnty_unemployed","cnty_unemp_rate")
colnames(cnty_emp2021) <- c("laus_code","st_code","cnty_code","cnty_st","year","Non","cnty_labor_force","cnty_employed","cnty_unemployed","cnty_unemp_rate")

cnty_emp <- rbind(cnty_emp2018,cnty_emp2019,cnty_emp2020,cnty_emp2021)

cnty_emp[,fips := paste0(st_code,cnty_code)]
cnty_emp[,year_fips := paste0(year,"-",fips)]
cnty_emp <- cnty_emp[,.(year_fips,cnty_unemp_rate,cnty_labor_force,cnty_unemployed)]

#---- 3. Join wage, income and geography into cnty-level data ---- 
cnty_census <- merge(cnty_geography,cnty_wage,all.y=FALSE,by="fips")
cnty_census <- merge(cnty_census,cnty_emp,all.x=TRUE,by="year_fips")

#---- 4. Export data ----
saveRDS(tract_dat,file=paste0(wd,census.folder,"tract_data.rds"))
saveRDS(cnty_census,file=paste0(wd,census.folder,"county_census.rds"))
