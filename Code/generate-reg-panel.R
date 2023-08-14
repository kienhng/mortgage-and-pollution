packages <- c("readxl","data.table","tidyverse","haven")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

#---- 1. HMDA sample ----
hmda_weighted <- readRDS(paste0(wd,hmda.folder,"hmda_weighted.rds"))

#---- 2. TRI data ----
tri_match <- readRDS(paste0(wd,tri.folder,"tri_match.rds"))
setkey(tri_match, year_fips)
setkey(hmda_weighted, year_fips)

#---- 3. Summarise TRI to county level and create treatment variables----
##---- Create main TRI releases variables ----
tri_match[,carcinogen := ifelse(carcinogen == "YES",1,0)]
tri_match[,pbt := ifelse(classification == "PBT",1,0)]

tri_match[,carc_releases := carcinogen*total_releases]
tri_match[,carc_onsite := carcinogen*onsite_release_total]

tri_match[,air_releases := x5.1_fugitive_air + x5.2_stack_air]
tri_match[,carc_air := carcinogen*air_releases]

##---- Collapse the tri_match by year_fips ----
tri_match_total <- tri_match[,lapply(.(x5.1_fugitive_air,
                                       onsite_release_total,
                                       total_releases,
                                       carc_releases,
                                       carc_onsite,
                                       air_releases,
                                       carc_air),sum), ## Sum of all releases in one county
                             by = year_fips]
tri_match_mean <- tri_match[,lapply(.(carcinogen,pbt),mean),by = year_fips]
tri_match_median <- tri_match[,median(primary_naics),by=year_fips]

## Create TRI dataset on county level
tri_coulev <- tri_match_total[tri_match_mean, on = "year_fips"]
tri_coulev <- tri_coulev[tri_match_median, on = "year_fips"]

colnames(tri_coulev) <- c("year_fips","fugitive_air",
                          "onsite_release_total",
                          "total_releases",
                          "carc_releases",
                          "carc_onsite",
                          "air_releases",
                          "carc_air",
                          "carcinogen","pbt",
                          "primary_naics")

tri_coulev <- unique(tri_match[,.(year_fips,fips,state)], by = "year_fips")[tri_coulev, on = "year_fips"]

##---- Update county-level variables related to total_releases ----
##---- Variables transformation ----
## Log transformation
tri_coulev[,ln_total_releases := log(total_releases+1)]
tri_coulev[,ln_onsite_release_total := log(onsite_release_total+1)]
tri_coulev[,ln_air_releases := log(air_releases+1)]

tri_coulev[,ln_carc_releases := log(carc_releases+1)]
tri_coulev[,ln_carc_onsite := log(carc_onsite+1)]
tri_coulev[,ln_carc_air := log(carc_air+1)]

## Make nfac_county: number of facilities in a county
tri_match[,latlon_id := paste0(latitude,longitude)]
nfac_county <- tri_match[,unique(latlon_id),fips][,.N,fips]
tri_coulev <- tri_coulev[nfac_county, on = "fips"]
tri_coulev[,nfac_county := N]
tri_coulev[,N := NULL]

## Make nfac_carc_county: number of facilities in a county release carcinogenic waste
nfac_carc_county <- tri_match[carcinogen == 1][,unique(latlon_id),fips][,.N,fips]
tri_coulev <- tri_coulev[nfac_carc_county, on = "fips"]
tri_coulev[,nfac_carc_county := N]
tri_coulev[,N := NULL]

## Create effect area of all facilities in a county
# tri_coulev[,effect_1km := nfac_county*RADIUS.1KM]
# tri_coulev[,effect_2km := nfac_county*RADIUS.2KM]
# tri_coulev[,effect_5km := nfac_county*RADIUS.5KM]
# tri_coulev[,effect_10km := nfac_county*RADIUS.10KM]
# tri_coulev[,effect_20km := nfac_county*RADIUS.20KM]
# 
# tri_coulev[,carc_effect_1km := nfac_carc_county*RADIUS.1KM]
# tri_coulev[,carc_effect_2km := nfac_carc_county*RADIUS.2KM]
# tri_coulev[,carc_effect_5km := nfac_carc_county*RADIUS.5KM]
# tri_coulev[,carc_effect_10km := nfac_carc_county*RADIUS.10KM]
# tri_coulev[,carc_effect_20km := nfac_carc_county*RADIUS.20KM]

#---- 4. Create panel ----
## Get panel with partially matched HMDA and TRI
full_panel <- merge(hmda_weighted, tri_coulev, all.x = FALSE, by = "year_fips")
setkey(full_panel, year_fips)
rm(hmda_weighted, tri_match)

#---- 5. Add US census data to the panel data ----
##---- 5.1 Tract level data ----
tract_dat <- readRDS(paste0(wd,census.folder,"tract_data.RDS"))
tract_dat[,fips := NULL]

full_panel[,census_tract:=as.numeric(census_tract)]
tract_dat[,census_tract:=as.numeric(census_tract)]

full_panel <- merge(full_panel,tract_dat,all.x = TRUE, by = "census_tract")

##---- 5.2 County level ----
### Load Census Data (Census data will be matched with the final full_panel)
county_census <- as.data.table(read_excel(paste0(wd,census.folder,"/2020_UA_COUNTY.xlsx")))
county_demog <- as.data.table(read_excel(paste0(wd,census.folder,"/CountyUnemployment.xlsx"),sheet = "2022_unemp_income"))
county_census[,fips := paste0(STATE,COUNTY)]
county_census <- merge(county_census,county_demog,all.x=TRUE,by="fips")
county_census

### Collect needed variables in Census Data
census_dat <- county_census[,.(fips,REGION,POPDEN_COU,POP_COU,ALAND_COU,unemp_rate,county_median_income)]
colnames(census_dat) <- str_to_lower(colnames(census_dat))

### Transform variables
census_dat[,fips := as.numeric(fips)]
census_dat[,aland_cou_sqkm:= aland_cou/1000000]

### Merge census data with full panel
full_panel[,c("year","fips") := tstrsplit(year_fips, "-", fixed=TRUE)] ## To collect the fips value again
full_panel[,fips := as.numeric(fips)]
full_panel <- merge(full_panel,census_dat,all.x = FALSE, by = "fips")

### Update total releases variables
full_panel[,pa_release := total_releases/aland_cou_sqkm] ## Per-area variables
full_panel[,pa_onsite := onsite_release_total/aland_cou_sqkm] ## Per-area variables
full_panel[,pa_air := air_releases/aland_cou_sqkm] ## Per-area variables

### Update carc releases variables
full_panel[,pa_carc_release := carc_releases/aland_cou_sqkm] ## Per-area variables
full_panel[,pa_carc_onsite := carc_onsite/aland_cou_sqkm] ## Per-area variables
full_panel[,pa_carc_air := carc_air/aland_cou_sqkm] ## Per-area variables

#---- 6. Creat maching sample data ----
## Due to the large size of the regression panel data, I need to creat a smaller sample to run PSM
## The smaller sample will have the same amount of obs with 0 carcinogen release, and a random sample of non-0 carc releases

carc_zero <- full_panel[carc_releases == 0]
carc_nonzero_sample <- full_panel[carc_releases != 0][sample(.N,400000)]

psm_sample <- rbind(carc_zero, carc_nonzero_sample)

#---- 7. Export data ----
##---- Full panel data ----
saveRDS(tri_coulev,file=paste0(wd,panel.folder,"tri_yearfips.rds"))
saveRDS(full_panel,file=paste0(wd,panel.folder,"full_panel.rds"))

##---- Sampled panel data ----
sampled_panel <- full_panel[sample(.N,2000000)]
write_dta(sampled_panel ,path = paste0(wd,panel.folder,"sampled_panel.dta"))

##---- PSM sample data ----
write_dta(psm_sample ,path = paste0(wd,panel.folder,"psm_sample.dta"))

