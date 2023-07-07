packages <- c("data.table","rgdal","tidyverse","stringr","ggplot2","haven","zipcodeR")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

tri_2018 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2018_us.csv")))
tri_2019 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2019_us.csv")))
tri_2020 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2020_us.csv")))
tri_2021 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2021_us.csv")))

fips <- as.data.table(read_dta(paste0(wd,"/Data/fips_code.dta")))
tri_dat <- rbind(tri_2018,tri_2019,tri_2020,tri_2021)

## Check consistency in column names
#tri.list <- list(tri_2018,tri_2019,tri_2020,tri_2021)
#for (i in tri.list) {
#  for (k in tri.list) {
#    print(any(colnames(i) != colnames(k)))
#  }
#}

#---- Rename Variables ----
## Remove the index in colnames
str_view(colnames(tri_dat), "^0*?[1-9]\\d*\\.")
# The "\\." is the dot
# and "^0*?[1-9]\\d*" is to match any number greater than 
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

#---- Clean Data ----
tri_clean <- tri_dat %>%
#  filter(carcinogen == "YES") %>%
  filter(!(st %in% c("AS","GU", "MP", "PR", "VI"))) %>% ## Remove outside-US territories
  filter(total_releases > 0) %>%
#  filter(x5.1_fugitive_air>0& x5.2_stack_air>0 & x5.3_water==0 & x5.4_underground==0) %>% ## Select air pollution only
  select(year, trifd,  frs_id, facility_name,  city, county, st, zip, chemical, latitude, longitude, 
         industry_sector_code, primary_sic,  primary_naics, clean_air_act_chemical, classification, 
         metal, metal_category, carcinogen, unit_of_measure, 
         x5.1_fugitive_air,  x5.2_stack_air, x5.3_water, x5.4_underground, 
         x5.5.1a_rcra_c_landfill, x5.5.1b_other_landfills, 
         x5.5.2_land_treatment, x5.5.3a_rcra_surface_im, x5.5.3b_other_surface_i, x5.5.4_other_disposal, 
         potw_total_transfers, onsite_release_total, 
         offsite_release_total, offsite_recycled_total, offsite_energy_recovery_t, 
         offsite_treated_total, 
         total_releases, 
         x8.1a_onsite_contained, x8.1b_onsite_other, x8.1c_offsite_contain, 
         x8.1d_offsite_other_r, x8.2_energy_recover_on, x8.3_energy_recover_of, 
         x8.4_recycling_on_site, x8.5_recycling_off_sit, x8.6_treatment_on_site, x8.7_treatment_off_site, 
         production_wste_8.18.7, x8.8_onetime_release, prod_ratio_or_activity, x8.9_production_ratio
         )

#---- Create FIPS code for TRI data ----
##---- Thinking process ----
# The TRI data does not have FIPS codes, but rather state and county names
# So I need to create a "text" FIPS codes, based on state names and county names, called the ST-CT code
# Then I match the TRI data with the FIPS code data, based on ST-CT code

##---- Add state code to TRI data ----
tri_clean[,state := st]
fips[,fips := as.numeric(paste0(state_code,county_code))]

st_code <- fips[,.N,c("state","state_code")][,c("state","state_code")][!(state %in% c("AS","GU", "MP", "PR", "VI", "UM"))]
tri_clean <- tri_clean[st_code,on = "state"]

##---- Create a state-county variable in TRI to match with FIPS data ----
## The process is as follows:
## 1. Make a variable of state-county in TRI dataset (extract a smaller st_ct vector on first check to reduce computing work)
## 2. Check the TRI state-county vector with the state-county variable in check-FIPS dataset
## 3. Find the differences
## 4. Make changes to both and check the differences until the two match

## 1. Make a variable of TRI state-county and extract it
tri_clean[, st_ct := paste0(st,"-",county)]
tri_stct <- tri_clean[,.N,st_ct][,st_ct]

## 2. Check the TRI state-county vector with FIPS state-county variable
check_fips <- fips[,c("state","county","fips")][,county := str_to_upper(county)]
check_fips[,county := str_remove(check_fips$county, "\\b( COUNTY)\\b")]

# 3-4. Find the differences and make adjustments
tri_right_name <- tri_stct %in% check_fips[,st_ct := paste0(state,"-",county)][,.N,st_ct][,st_ct]
tri_stct[!tri_right_name]
## Before adjustments: 43 differences

## First adjustment: remove . in both datasets
check_fips[,county := str_remove(check_fips$county, "\\.")]
tri_stct <- str_remove(tri_stct, "\\.")

tri_clean[,st_ct := str_remove(tri_clean$st_ct, "\\.")]
## 34 differences

## Second adjustment: remove () and remove MUNICIPIO
tri_stct <- str_remove(tri_stct,"[(]")
tri_stct <- str_remove(tri_stct, "[)]")
tri_stct <- str_remove(tri_stct, "\\b( MUNICIPIO)\\b")

tri_clean[,st_ct := str_remove(tri_clean$st_ct, "[(]")]
tri_clean[,st_ct := str_remove(tri_clean$st_ct, "[)]")]
tri_clean[,st_ct := str_remove(tri_clean$st_ct, "\\b( MUNICIPIO)\\b")]
## 6 differences

## Third adjustment:
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

##---- Merge TRI_clean with FIPS data ----
tri_match <- check_fips[tri_clean, on = "st_ct"]
tri_test <- tri_match[,.(fips,year,carcinogen,total_releases,onsite_release_total,offsite_release_total)]
tri_test[,carcinogen := ifelse(carcinogen == "YES",1,0)]
tri_test[,year_fips := paste0(year,"-",fips)]

tri_state <- tri_match[,.(year,fips,state_code)]
tri_state[,year_fips := paste0(year,"-",fips)]
tri_state <- unique(tri_state)
