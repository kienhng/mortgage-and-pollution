rm(list=ls())

packages <- c("usmap","maptools","rgdal","tidycensus","tidyverse","stringr","usmap","ggplot2","maptools","haven")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

wd=getwd()

raw_2018 <- read_csv("2018_us.csv")
raw_2019 <- read_csv("2019_us.csv")
raw_fipscode <- read_dta("fips_code.dta")

#-------------------------------------------------------------------------------
#                            Rename Columns
#-------------------------------------------------------------------------------

colnames(raw_2018)
## Remove the index in colnames
str_view(colnames(raw_2018), ".\\. ") # The first dot is actually a regex, while the \\. is the real dot
names_df <- as.data.frame(str_split(colnames(raw_2018), ".\\. "),
                          col.names = NULL) # Split the colnames string with the separator". "
new_colnames <- as.list(names_df[2,])

## Adding x in front of colnames starting with a digit
new_colnames <- ifelse(grepl("^\\d", new_colnames), # logic compare string with matched regex
                       gsub("^", "x", new_colnames), # add x at the start ^ of the strings 
                       new_colnames) # return the orginal value if grepl returns FALSE

colnames(raw_2018) <- # There are 5 string replacements in here
  str_replace_all(  
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

colnames(raw_2018)

# Subset data 

## Remove unused columns
clean_2018 <- raw_2018 %>%
  filter(carcinogen == "YES") %>%
  filter(!(st %in% c("AS","GU", "MP", "PR", "VI"))) %>% ## Remove outside-US territories
  select(year,
         trifd, 
         frs_id,
         facility_name, 
         city,
         county,
         st,
         zip, 
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

#-------------------------------------------------------------------------------
#                            Data Visualization
#-------------------------------------------------------------------------------

# Transform data into spatial format
c2018_transformed <- usmap_transform(clean_2018
                                        ,input_names = c("longitude", "latitude")
                                        ,output_names = c("x", "y"))

us_pop <- get_estimates(geography = "state", product = "population")
colnames(us_pop) <- c("county", "fips", "variable", "population")

plot_usmap(regions = "state", 
           data = us_pop, 
           values = "population",
           color = "white") +
  scale_color_gradient2(low = "red", 
                        mid = "white", 
                        high = "blue", 
                        midpoint = median(us_pop$population)) +
  geom_point(data = c2018_transformed, 
             aes(x = x, y = y, size = x5.2_stack_air),
             color = "yellow",
             alpha = 0.25)
  #geom_point(data = c2018_transformed,
   #          aes(x = x, y = y, size = x5.3_water),
    #         color = "blue",
     #        alpha = 0.25)

# FIPS codes data
fips <- raw_fipscode %>%
  mutate(fips = str_c(state_code, county_code)) %>%
  select(state, county, fips)
