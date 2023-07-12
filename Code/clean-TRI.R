packages <- c("data.table","rgdal","tidyverse","stringr","ggplot2","haven","zipcodeR","readxl")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

tri2018 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2018_us.csv")))
tri2019 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2019_us.csv")))
tri2020 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2020_us.csv")))
tri2021 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2021_us.csv")))

fips <- as.data.table(read_dta(paste0(wd,census.folder,"/fips_code.dta")))
county_census <- as.data.table(read_excel(paste0(wd,census.folder,"/2020_UA_COUNTY.xlsx")))

tri_dat <- rbind(tri2018,tri2019,tri2020,tri2021)

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
##---- Clean Census Data ----
county_census[,fips := paste0(STATE,COUNTY)]
tri_census <- county_census[,.(fips,POP_COU,HOU_COU,ALAND_COU,ALAND_PCT_URB,POPPCT_URB,HOUPCT_URB)]
tri_census[,fips := as.numeric(fips)]
colnames(tri_census) <- str_to_lower(colnames(tri_census))

##---- Sample selection from TRI data ----
tri_sample <- tri_dat %>%
#  filter(carcinogen == "YES") %>%
  filter(!(st %in% c("AS","GU", "MP", "PR", "VI"))) %>% # Remove outside-US territories
  filter(zip != 99686) %>% # Remove VALDEZ-CORDOVA CENSUS AREA county
  filter(classification != "Dioxin") %>%
  filter(total_releases > 0) %>%
# filter(x5.1_fugitive_air>0& x5.2_stack_air>0 & x5.3_water==0 & x5.4_underground==0) %>% ## Select air pollution only
  select(year, trifd,  frs_id, facility_name,  city, county, st, zip, chemical, latitude, longitude, 
         industry_sector_code, primary_sic,  primary_naics, 
         clean_air_act_chemical, classification, 
         metal, metal_category, carcinogen, unit_of_measure, 
         x5.1_fugitive_air,  x5.2_stack_air, x5.3_water, x5.4_underground,
         onsite_release_total, 
         offsite_release_total, 
         offsite_recycled_total, 
         offsite_treated_total, 
         total_releases,
         x8.6_treatment_on_site, 
         x8.7_treatment_off_site)
#---- 3. Create FIPS code for TRI data ----
##---- Thinking process ----
# The TRI data does not have FIPS codes, but rather state and county names
# So I need to create a "text" FIPS codes, based on state names and county names, called the ST-CT code
# Then I match the TRI data with the FIPS code data, based on ST-CT code

##---- Add state code to TRI data ----
tri_sample[,state := st]
fips[,fips := as.numeric(paste0(state_code,county_code))]

st_code <- fips[,.N,c("state","state_code")][,c("state","state_code")][!(state %in% c("AS","GU", "MP", "PR", "VI", "UM"))]
tri_sample <- tri_sample[st_code,on = "state"]

##---- Create a state-county variable in TRI to match with FIPS ----
## The process is as follows:
## 1. Make a variable of state-county in TRI dataset (extract a smaller st_ct vector on first check to reduce computing work)
## 2. Check the TRI state-county vector with the state-county variable in check-FIPS dataset
## 3. Find the differences
## 4. Make changes to both and check the differences until the two match

## 1. Make a variable of TRI state-county and extract it
tri_sample[, st_ct := paste0(st,"-",county)]
tri_stct <- tri_sample[,.N,st_ct][,st_ct]

## 2. Check the TRI state-county vector with FIPS state-county variable
check_fips <- fips[,c("state","county","fips")][,county := str_to_upper(county)]
check_fips[,county := str_remove(check_fips$county, "\\b( COUNTY)\\b")]

# 3-4. Find the differences and make adjustments
tri_right_name <- tri_stct %in% check_fips[,st_ct := paste0(state,"-",county)][,.N,st_ct][,st_ct]
tri_stct[!tri_right_name]
## Before adjustments: 43 differences

## First adjustment: remove . in both datasets => 34 differences
check_fips[,county := str_remove(check_fips$county, "\\.")]
check_fips[,st_ct := str_remove(check_fips$st_ct, "\\.")]
tri_stct <- str_remove(tri_stct, "\\.")

tri_sample[,st_ct := str_remove(tri_sample$st_ct, "\\.")]

## Second adjustment: remove () and remove MUNICIPIO => 6 differences
tri_stct <- str_remove(tri_stct,"[(]")
tri_stct <- str_remove(tri_stct, "[)]")
tri_stct <- str_remove(tri_stct, "\\b( MUNICIPIO)\\b")

tri_sample[,st_ct := str_remove(tri_sample$st_ct, "[(]")]
tri_sample[,st_ct := str_remove(tri_sample$st_ct, "[)]")]
tri_sample[,st_ct := str_remove(tri_sample$st_ct, "\\b( MUNICIPIO)\\b")]

## Third adjustment: manually change the var name
tri_stct <- str_replace(tri_stct,"AK-SOUTHEAST FAIRBANKS CENSU$","AK-SOUTHEAST FAIRBANKS CENSUS AREA")
tri_stct <- str_replace(tri_stct,"AK-ALEUTIANS WEST CENSUS ARE$","AK-ALEUTIANS WEST CENSUS AREA")
tri_stct <- str_replace(tri_stct,"AK-FAIRBANKS NORTH STAR BORO$","AK-FAIRBANKS NORTH STAR BOROUGH")
tri_stct <- str_replace(tri_stct,"NV-CARSON CITY CITY$","NV-CARSON CITY")
tri_stct <- str_replace(tri_stct,"WI-JUNEAU BOROUGH$","WI-JUNEAU")
tri_stct <- str_replace(tri_stct,"WI-ST CROIX ISLAND$","WI-ST CROIX")

tri_sample[,st_ct := str_replace(tri_sample$st_ct,"AK-SOUTHEAST FAIRBANKS CENSU$","AK-SOUTHEAST FAIRBANKS CENSUS AREA")]
tri_sample[,st_ct := str_replace(tri_sample$st_ct, "AK-ALEUTIANS WEST CENSUS ARE$","AK-ALEUTIANS WEST CENSUS AREA")]
tri_sample[,st_ct := str_replace(tri_sample$st_ct, "AK-FAIRBANKS NORTH STAR BORO$","AK-FAIRBANKS NORTH STAR BOROUGH")]
tri_sample[,st_ct := str_replace(tri_sample$st_ct, "NV-CARSON CITY CITY$","NV-CARSON CITY")]
tri_sample[,st_ct := str_replace(tri_sample$st_ct, "WI-JUNEAU BOROUGH$","WI-JUNEAU")]
tri_sample[,st_ct := str_replace(tri_sample$st_ct, "WI-ST CROIX ISLAND$","WI-ST CROIX")]

#---- 4. Merge tri_sample with FIPS data ----
tri_match <- check_fips[tri_sample, on = "st_ct"]
tri_match[,carcinogen := ifelse(carcinogen == "YES",1,0)]
tri_match[,pbt := ifelse(classification == "PBT",1,0)]
tri_match[,carc_releases := carcinogen*total_releases]
tri_match[,pbt_releases := pbt*total_releases]
tri_match[,carc_pbt_releases := pbt*carcinogen*total_releases]
tri_match[,carc_air := carcinogen*x5.1_fugitive_air]

tri_match[,year_fips := paste0(year,"-",fips)]
tri_match <- tri_match[,.(frs_id,state,county,fips,year,year_fips,carcinogen,pbt,x5.1_fugitive_air,
                          onsite_release_total,total_releases,carc_releases,pbt_releases,carc_pbt_releases,carc_air)]


#---- 5. Collapse the tri_match by year_fips ----
tri_match_total <- tri_match[,lapply(.(x5.1_fugitive_air,
                    onsite_release_total,total_releases,
                    carc_releases,pbt_releases,carc_pbt_releases,carc_air),sum),by = year_fips]
tri_match_mean <- tri_match[,lapply(.(carcinogen,pbt),mean),by = year_fips]

## Create ready-to-match data including TRI and Census: tri_match_summary
tri_match_summary <- tri_match_total[tri_match_mean, on = "year_fips"]
colnames(tri_match_summary) <- c("year_fips","fugitive_air",
                                 "onsite_release_total","total_releases",
                                 "carc_releases","pbt_releases","carc_pbt_releases","carc_air","carcinogen","pbt")

tri_match_summary <- unique(tri_match[,.(year_fips,fips,state)], by = "year_fips")[tri_match_summary, on = "year_fips"]
tri_match_summary <- tri_census[tri_match_summary, on = "fips"]
tri_match_summary[,carc_per_area := carc_releases/aland_cou]
summary(tri_match_summary[,carc_per_area]) #median = 0.000003

# Export TRI data
saveRDS(tri_match, file = paste0(wd,tri.folder,"tri_match.rds")) # Uncollapsed TRI data
saveRDS(tri_match_summary, file = paste0(wd,panel.folder,"tri_match_sum.rds")) # Collapsed TRI data
