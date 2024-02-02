packages <- c("data.table","rgdal","tidyverse","stringr","ggplot2","haven","zipcodeR","readxl","tigris")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

setwd(wd)
setwd("Data")
setwd("RawData-TRI")

## Load data
fips <- as.data.table(read_dta(paste0(wd,census.folder,"/fips_code.dta")))

tri_names <- Sys.glob("????_us.csv")
raw_list <- list()

for (i in 1:length(tri_names)) {
  raw_file <- as.data.table(read_csv(tri_names[i]))
  raw_list[[i]] <- raw_file
}
tri_dat <- rbindlist(raw_list)

#---- 1. Rename Variables ----
## Remove the index in colnames
str_view(colnames(tri_dat), "^0*?[1-9]\\d*\\.")
## The "\\." is the dot
## and "^0*?[1-9]\\d*" is to match any number greater than 

names_df <- t(as.data.table(str_split(colnames(tri_dat), "^0*?[1-9]\\d*\\. ")))
new_colnames <- names_df[,2]

## Adding x in front of colnames starting with a digit
new_colnames <- ifelse(grepl("^\\d", new_colnames), # logic compare string with matched regex
                       gsub("^", "x", new_colnames), # add x at the start ^ of the strings 
                       new_colnames) # return the orginal value if grepl returns FALSE

# There are 5 string replacements in here
new_colnames <- str_replace_all(  
    str_replace_all(
        str_replace_all(
          str_replace_all(
            str_replace_all(
              str_to_lower(new_colnames)
            ," ", "_")
          ,"-", "")
        ,"__", "_")
      ,"\\(","")
    ,"\\)","")
colnames(tri_dat) <- new_colnames

#---- 2. Clean Data ----
##---- Sample selection from TRI data ----
tri_clean <- tri_dat %>%
  filter(!(st %in% c("AS","GU", "MP", "PR", "VI"))) %>% # Remove outside-US territories
  filter(zip != 99686) %>% # Remove VALDEZ-CORDOVA CENSUS AREA county
  filter(classification != "Dioxin") %>%
  filter(is.na(total_releases)==F) %>%
  filter(total_releases > 0) %>%
  select(year, frs_id, trifd, facility_name,  city, county, st, zip, chemical, 
         latitude, longitude, 
         industry_sector_code,  primary_naics, 
         clean_air_act_chemical, classification, 
         metal, metal_category, carcinogen, unit_of_measure, 
         x5.1_fugitive_air,  x5.2_stack_air, x5.3_water, x5.4_underground,
         onsite_release_total, 
         offsite_release_total, 
         total_releases)

#---- 3. Create FIPS code for TRI data ----
##---- Thinking process ----
# The TRI data does not have FIPS codes, but rather state and county names
# So I need to create a "text" FIPS codes, based on state names and county names, called the ST-CT code
# Then I match the TRI data with the FIPS code data, based on ST-CT code

##---- Add state code to TRI data ----
tri_clean[,state := st]
fips[,fips := as.numeric(paste0(state_code,county_code))]

st_code <- fips[,.N,c("state","state_code")][,c("state","state_code")][!(state %in% c("AS","GU", "MP", "PR", "VI", "UM"))]
tri_clean <- tri_clean[st_code,on = "state"]

##---- Create a state-county variable in TRI to match with FIPS ----
## The process is as follows:
## 1. Make a variable of state-county in TRI dataset (extract a smaller st_ct vector on first check to reduce computing work)
## 2. Check the TRI state-county vector with the state-county variable in check-FIPS dataset
## 3. Find the differences
## 4. Make changes to both and check the differences until the two match

## 1. Make a variable of TRI state-county and extract it
tri_clean[, st_ct := paste0(st,"-",county)]
tri_stct <- tri_clean[,.N,st_ct][,st_ct]

## 2. Check the TRI state-county vector with FIPS state-county variable
check_fips <- fips[,c("state","county","fips","state_code")][,county := str_to_upper(county)]
check_fips[,county := str_remove(check_fips$county, "\\b( COUNTY)\\b")]

# 3-4. Find the differences and make adjustments
tri_right_name <- tri_stct %in% check_fips[,st_ct := paste0(state,"-",county)][,.N,st_ct][,st_ct]
tri_stct[!tri_right_name]
## Before adjustments: 43 differences

## First adjustment: remove . in both datasets => 34 differences
check_fips[,county := str_remove(check_fips$county, "\\.")]
check_fips[,st_ct := str_remove(check_fips$st_ct, "\\.")]
tri_stct <- str_remove(tri_stct, "\\.")

tri_clean[,st_ct := str_remove(tri_clean$st_ct, "\\.")]

## Second adjustment: remove () and remove MUNICIPIO => 6 differences
tri_stct <- str_remove(tri_stct,"[(]")
tri_stct <- str_remove(tri_stct, "[)]")
tri_stct <- str_remove(tri_stct, "\\b( MUNICIPIO)\\b")

tri_clean[,st_ct := str_remove(tri_clean$st_ct, "[(]")]
tri_clean[,st_ct := str_remove(tri_clean$st_ct, "[)]")]
tri_clean[,st_ct := str_remove(tri_clean$st_ct, "\\b( MUNICIPIO)\\b")]

## Third adjustment: manually change the var name
tri_stct <- str_replace(tri_stct,"AK-SOUTHEAST FAIRBANKS CENSU$","AK-SOUTHEAST FAIRBANKS CENSUS AREA")
tri_stct <- str_replace(tri_stct,"AK-ALEUTIANS WEST CENSUS ARE$","AK-ALEUTIANS WEST CENSUS AREA")
tri_stct <- str_replace(tri_stct,"AK-FAIRBANKS NORTH STAR BORO$","AK-FAIRBANKS NORTH STAR BOROUGH")
tri_stct <- str_replace(tri_stct,"NV-CARSON CITY CITY$","NV-CARSON CITY")
tri_stct <- str_replace(tri_stct,"WI-JUNEAU BOROUGH$","WI-JUNEAU")
tri_stct <- str_replace(tri_stct,"WI-ST CROIX ISLAND$","WI-ST CROIX")

tri_clean[,st_ct := str_replace(tri_clean$st_ct,"AK-SOUTHEAST FAIRBANKS CENSU$","AK-SOUTHEAST FAIRBANKS CENSUS AREA")]
tri_clean[,st_ct := str_replace(tri_clean$st_ct, "AK-ALEUTIANS WEST CENSUS ARE$","AK-ALEUTIANS WEST CENSUS AREA")]
tri_clean[,st_ct := str_replace(tri_clean$st_ct, "AK-FAIRBANKS NORTH STAR BORO$","AK-FAIRBANKS NORTH STAR BOROUGH")]
tri_clean[,st_ct := str_replace(tri_clean$st_ct, "NV-CARSON CITY CITY$","NV-CARSON CITY")]
tri_clean[,st_ct := str_replace(tri_clean$st_ct, "WI-JUNEAU BOROUGH$","WI-JUNEAU")]
tri_clean[,st_ct := str_replace(tri_clean$st_ct, "WI-ST CROIX ISLAND$","WI-ST CROIX")]

#---- 4. Merge tri_clean with FIPS data ----
tri_match <- check_fips[tri_clean, on = "st_ct"]

tri_match[,year_fips := paste0(year,"-",fips)]
tri_match <- tri_match[,.(frs_id,trifd,state,state_code,latitude,longitude,county,fips,year,year_fips,
                          industry_sector_code,primary_naics, 
                          carcinogen,
                          classification,
                          x5.1_fugitive_air,
                          x5.2_stack_air,
                          onsite_release_total,
                          total_releases)]

# # Export TRI data
# saveRDS(tri_clean, file = paste0(wd,tri.folder,"tri_clean.rds")) # Full cleaned data
# saveRDS(tri_match, file = paste0(wd,tri.folder,"tri_match.rds")) # Uncollapsed TRI data

##----------- Draft analysis ------------------##
##----------- Draft analysis ------------------##
##----------- Draft analysis ------------------##

tri_check <- tri_match[year > 2004]

tri_check[,location:=paste0(latitude,longitude)]
tri_check[,.N,location]
tri_check[,time_total:=prod(unique(as.numeric(year))),location]

tri_check[,uniqueN(location),time_total][order(time_total)]

prod(2000:2022)


tri_check <- tri_check[time_total %in% c(prod(2012:2022),prod(2013:2022),prod(2014:2022),prod(2015:2022),
                                         prod(2016:2022),prod(2017:2022),prod(2018:2022),prod(2019:2022),
                                         prod(2020:2022),prod(2021:2022),2022),
                       target:="yes"]

tri_check[target=="yes" & carcinogen=="YES" & total_releases > 300,target_high_poll:="yes"]

tri_check[,uniqueN(frs_id)]
tri_check[target=="yes",uniqueN(frs_id)]
tri_check[target_high_poll=="yes",uniqueN(frs_id)]

tri_check[target_high_poll=="yes",.N,state][order(N)]

## Ratio
tri_check[target=="yes",uniqueN(frs_id)]/tri_check[,uniqueN(frs_id)]*100
tri_check[target_high_poll=="yes",uniqueN(frs_id)]/tri_check[,uniqueN(frs_id)]*100

##----------- Draft analysis ------------------##
##----------- Draft analysis ------------------##
##----------- Draft analysis ------------------##

