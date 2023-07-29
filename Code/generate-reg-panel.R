packages <- c("readxl","data.table","tidyverse","haven")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

#---- 1. HMDA sample ----
hmda_match <- readRDS(paste0(wd,hmda.folder,"hmda_match.rds"))

#---- 2. TRI data ----
tri_match <- readRDS(paste0(wd,tri.folder,"tri_match.rds"))
setkey(tri_match, year_fips)
setkey(hmda_match, year_fips)

#---- 3. Census Data and match with TRI ----
county_census <- as.data.table(read_excel(paste0(wd,census.folder,"/2020_UA_COUNTY.xlsx")))
county_census[,fips := paste0(STATE,COUNTY)]

## Collect needed variables
tri_census <- county_census[,.(fips,REGION,POPDEN_COU, POP_COU,HOU_COU,ALAND_COU,
                               ALAND_PCT_URB,POPPCT_URB,HOUPCT_URB)]
tri_census[,fips := as.numeric(fips)]
colnames(tri_census) <- str_to_lower(colnames(tri_census))
## Census data will be matched with TRI data

#---- 4. Summarise TRI to county level and create treatment variables----
##---- Create main TRI releases variables ----
tri_match[,carcinogen := ifelse(carcinogen == "YES",1,0)]
tri_match[,pbt := ifelse(classification == "PBT",1,0)]

tri_match[,carc_releases := carcinogen*total_releases]
tri_match[,carc_onsite := carcinogen*onsite_release_total]
tri_match[,carc_offsite := carcinogen*offsite_release_total]

# tri_match[,pbt_releases := pbt*total_releases]
# tri_match[,carc_pbt_releases := pbt*carcinogen*total_releases]

tri_match[,air_releases := x5.1_fugitive_air + x5.2_stack_air]
tri_match[,carc_air := carcinogen*air_releases]

##---- Collapse the tri_match by year_fips ----
tri_match_total <- tri_match[,lapply(.(x5.1_fugitive_air,
                                       onsite_release_total,offsite_release_total,
                                       total_releases,
                                       carc_releases,
                                       carc_onsite,carc_offsite,
                                       air_releases,
                                       carc_air),sum),
                             by = year_fips]
tri_match_mean <- tri_match[,lapply(.(carcinogen,pbt),mean),by = year_fips]
tri_match_median <- tri_match[,median(primary_naics),by=year_fips]

## Create TRI dataset on county level
tri_coulev <- tri_match_total[tri_match_mean, on = "year_fips"]
tri_coulev <- tri_coulev[tri_match_median, on = "year_fips"]

colnames(tri_coulev) <- c("year_fips","fugitive_air",
                          "onsite_release_total","offsite_release_total",
                          "total_releases",
                          "carc_releases",
                          "carc_onsite","carc_offsite",
                          "air_releases",
                          "carc_air",
                          "carcinogen","pbt",
                          "primary_naics")

tri_coulev <- unique(tri_match[,.(year_fips,fips,state)], by = "year_fips")[tri_coulev, on = "year_fips"]
tri_coulev <- tri_census[tri_coulev, on = "fips"]

##---- Update county-level variables ----
tri_coulev[,carc_pa := carc_releases/aland_cou]
tri_coulev[,carc_air_pa := carc_air/aland_cou]

##---- Log transformation ----
tri_coulev[,ln_total_releases := log(total_releases+1)]
tri_coulev[,ln_onsite_release_total := log(onsite_release_total+1)]
tri_coulev[,ln_offsite_release_total := log(offsite_release_total+1)]

tri_coulev[,ln_carc_releases := log(carc_releases+1)]
tri_coulev[,ln_carc_onsite := log(carc_onsite+1)]
tri_coulev[,ln_carc_offsite := log(carc_offsite+1)]

tri_coulev[,ln_air_releases := log(air_releases+1)]
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

## Create effect area of all facilities in a county
tri_coulev[,effect_1km := nfac_carc_county*RADIUS.1KM]
tri_coulev[,effect_5km := nfac_carc_county*RADIUS.5KM]
tri_coulev[,effect_10km := nfac_carc_county*RADIUS.10KM]
tri_coulev[,effect_20km := nfac_carc_county*RADIUS.20KM]

#---- 5. Create panel ----
reg_panel <- merge(hmda_match, tri_coulev, all.x = FALSE, by = "year_fips")
setkey(reg_panel, year_fips)

##---- Take sample from reg_panel ----
reg_panel_sampled <- reg_panel[sample(.N,1000000)]

#---- 6. Export data ----
saveRDS(tri_coulev,file=paste0(wd,panel.folder,"tri_yearfips.rds"))
write_dta(reg_panel_sampled ,path = paste0(wd,panel.folder,"reg_panel_sampled.dta"))
