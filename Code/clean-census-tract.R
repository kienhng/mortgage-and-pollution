packages <- c("data.table","rgdal","tidyverse","stringr","ggplot2","haven","zipcodeR","readxl","tigris")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

tract_raw <- as.data.table(read.csv(paste0(wd,census.folder,"tract_census2010.csv")))

#---- 1. Rename tract data ----
colnames(tract_raw) <- c("fips","state","county","census_tract","primary_ruca_code","secondary_ruca_code",
                         "tract_pop","aland_tract","popden_tract")

#---- 2. Change the primary RUCA code ----
## The rules are:
## 1,2,3 are metropolitan = 1
## 4,5,6 are micropolitan = 2
## 7,8,9 are small towns = 3
## 10 is rural = 4
## 99 is NA

tract_raw[primary_ruca_code %in% c(1,2,3),urbru_class := 1]
tract_raw[primary_ruca_code %in% c(4,5,6),urbru_class := 2]
tract_raw[primary_ruca_code %in% c(7,8,9),urbru_class := 3]
tract_raw[primary_ruca_code == 10,urbru_class := 4]
tract_raw[primary_ruca_code == 99,urbru_class := NA]

#---- 3. Export data ----
tract_dat <- tract_raw[,.(fips,census_tract,primary_ruca_code,urbru_class)]
saveRDS(tract_dat,file=paste0(wd,census.folder,"tract_data.RDS"))
