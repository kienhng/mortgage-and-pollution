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

#---- 3. Census Data ----
## Census data will be matched with the final full_panel
county_census <- as.data.table(read_excel(paste0(wd,census.folder,"/2020_UA_COUNTY.xlsx")))
county_census[,fips := paste0(STATE,COUNTY)]

## Collect needed variables in Census Data
census_dat <- county_census[,.(fips,REGION,POPDEN_COU,POP_COU,HOU_COU,ALAND_COU,
                               ALAND_PCT_URB,POPPCT_URB,HOUPCT_URB)]
colnames(census_dat) <- str_to_lower(colnames(census_dat))

## Transform variables
census_dat[,fips := as.numeric(fips)]
census_dat[,aland_cou_sqkm:= aland_cou/1000000]

#---- 4. Summarise TRI to county level and create treatment variables----
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
                                       carc_air),sum),
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
##---- Log transformation ----
tri_coulev[,ln_total_releases := log(total_releases+1)]
tri_coulev[,ln_onsite_release_total := log(onsite_release_total+1)]

tri_coulev[,ln_carc_releases := log(carc_releases+1)]
tri_coulev[,ln_carc_onsite := log(carc_onsite+1)]

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
tri_coulev[,N := NULL]

## Create effect area of all facilities in a county
tri_coulev[,effect_1km := nfac_county*RADIUS.1KM]
tri_coulev[,effect_2km := nfac_county*RADIUS.2KM]
tri_coulev[,effect_5km := nfac_county*RADIUS.5KM]
tri_coulev[,effect_10km := nfac_county*RADIUS.10KM]
tri_coulev[,effect_20km := nfac_county*RADIUS.20KM]

tri_coulev[,carc_effect_1km := nfac_carc_county*RADIUS.1KM]
tri_coulev[,carc_effect_2km := nfac_carc_county*RADIUS.2KM]
tri_coulev[,carc_effect_5km := nfac_carc_county*RADIUS.5KM]
tri_coulev[,carc_effect_10km := nfac_carc_county*RADIUS.10KM]
tri_coulev[,carc_effect_20km := nfac_carc_county*RADIUS.20KM]

#---- 5. Create panel ----
## Get panel with partially matched HMDA and TRI
full_panel <- merge(hmda_match, tri_coulev, all.x = TRUE, by = "year_fips")
setkey(full_panel, year_fips)

## Get panel with fully matched HMDA and TRI
hmda_unmatch <- full_panel[is.na(carcinogen)]
for (i in names(tri_coulev)) {
  set(hmda_unmatch, which(is.na(hmda_unmatch[[i]])),i,0)
}

tmatched_panel <- full_panel[!(is.na(carcinogen))]
rm(hmda_match)
rm(full_panel)
rm(tri_match)
gc()

## Get full panel with all HMDA values and all TRI values
full_panel <- rbind(tmatched_panel, hmda_unmatch)
saveRDS(tmatched_panel,file = paste0(wd,panel.folder,"tmatched_panel.rds"))
saveRDS(hmda_unmatch,file = paste0(wd,hmda.folder,"hmda_unmatch.rds"))

rm(tmatched_panel, hmda_unmatch)
gc()

## Add US census data to the panel data
full_panel[,c("year","fips") := tstrsplit(year_fips, "-", fixed=TRUE)] ## To collect the fips value again
full_panel[,fips := as.numeric(fips)]
full_panel <- merge(full_panel,census_dat,all.x = TRUE, by = "fips")
gc()

census_dat[,aland_cou_sqkm:= aland_cou/1000000]

##---- Take sample from full_panel ----
sampled_panel <- full_panel[sample(.N,500000)]

#---- 6. Export data ----
saveRDS(tri_coulev,file=paste0(wd,panel.folder,"tri_yearfips.rds"))
saveRDS(full_panel,file=paste0(wd,panel.folder,"full_panel.rds"))
write_dta(sampled_panel ,path = paste0(wd,panel.folder,"sampled_panel.dta"))

