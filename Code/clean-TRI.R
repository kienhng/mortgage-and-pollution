packages <- c("data.table","rgdal","tidyverse","stringr","ggplot2","haven","zipcodeR")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

wd=getwd()
tri.folder="/Data/RawData-TRI"

#tri_2017 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2017_us.csv")))
tri_2018 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2018_us.csv")))
tri_2019 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2019_us.csv")))
tri_2020 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2020_us.csv")))
tri_2021 <- as.data.table(read_csv(paste0(wd,tri.folder,"/2021_us.csv")))
tri_fipscode <- read_dta(paste0(wd,"/Data/fips_code.dta"))

# Merge data
## Check consistency in column names
tri.list <- list(tri_2018,tri_2019,tri_2020,tri_2021)
for (i in tri.list) {
  for (k in tri.list) {
    print(any(colnames(i) != colnames(k)))
  }
}

## Merge data
tri_dat <- rbind(tri_2018,tri_2019,tri_2020,tri_2021)

#--------------------------
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

## Remove unused columns
tri_clean <- tri_dat %>%
  filter(carcinogen == "YES") %>%
  filter(!(st %in% c("AS","GU", "MP", "PR", "VI"))) %>% ## Remove outside-US territories
  filter(total_releases > 0) %>%
#  filter(x5.1_fugitive_air>0& x5.2_stack_air>0 & x5.3_water==0 & x5.4_underground==0) %>% ## Select air pollution only
  select(year,
         trifd, 
         frs_id,
         facility_name, 
         city,
         county,
         st,
         zip,
         chemical,
         latitude,
         longitude,
         industry_sector_code,
         primary_sic, 
         primary_naics,
         clean_air_act_chemical,
         classification,
         metal,
         metal_category,
         carcinogen,
         unit_of_measure,
         x5.1_fugitive_air,
         x5.2_stack_air,
         x5.3_water,
         x5.4_underground,
         x5.5.1a_rcra_c_landfill,
         x5.5.1b_other_landfills,
         x5.5.2_land_treatment,
         x5.5.3a_rcra_surface_im,
         x5.5.3b_other_surface_i,
         x5.5.4_other_disposal,
         potw_total_transfers,
         onsite_release_total,
         offsite_release_total,
         offsite_recycled_total,
         offsite_energy_recovery_t,
         offsite_treated_total,
         total_releases,
         x8.1a_onsite_contained,
         x8.1b_onsite_other,
         x8.1c_offsite_contain,
         x8.1d_offsite_other_r,
         x8.2_energy_recover_on,
         x8.3_energy_recover_of,
         x8.4_recycling_on_site,
         x8.5_recycling_off_sit,
         x8.6_treatment_on_site,
         x8.7_treatment_off_site,
         production_wste_8.18.7,
         x8.8_onetime_release,
         prod_ratio_or_activity,
         x8.9_production_ratio
         )

tri_clean[,sum(total_releases),year]
tri_dat[,.N]
tri_clean[,.N]

tri_clean[tri_clean[,x5.1_fugitive_air>0& x5.2_stack_air>0 & x5.3_water==0 & x5.4_underground==0]][,.N,zip][N > 4]
tri_clean[tri_clean[,x5.1_fugitive_air>0& x5.2_stack_air>0 & x5.3_water==0 & x5.4_underground==0 & carcinogen == "YES"]][,.N,chemical][N>100]

tri_clean[,.N,total_releases >0]

