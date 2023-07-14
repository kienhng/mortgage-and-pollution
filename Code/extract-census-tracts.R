packages <- c("data.table","tidyverse","haven","rgdal","ggmap","usmap","ggplot2","tigris","readxl")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

#---- 1. Import data and collect lat/lon data from TRI sample ----
tri_match <- readRDS(paste0(wd,tri.folder,"tri_match.rds"))
census_dat <- as.data.table(read_excel(paste0(wd,census.folder,"census_dat.xlsx")))

coords <- tri_match[,.(state,state_code,latitude,longitude)]
census_tract <- census_dat[,.(STATEFP,COUNTYFP,TRACTCE,GEOID,ALAND,AWATER,INTPTLAT,INTPTLON)]

## Code to collect the census tract in the first time.
## The output is the census_dat.csv
# census <- rep(list(1),51)
# for (i in 1:51) {
#   census[[i]] <- as.data.table(tigris::tracts(state = st_name[i]))
# }
# # Remove the 13th column (name: geometry) since it has different class for each state
# for (i in 1:51) {
#   census[[i]][,geometry := NULL]
# }
# # Run the loop to bring all data in the list into a big data table (census_dat)
# census_dat <- census[[1]]
# for (i in 2:51) {
#   census_dat <- rbind(census_dat,census[[i]])
# }
# write.csv(census_dat,file=paste0(wd,census.folder,"census_dat.csv"))

#---- 2. Merge census tract data with HMDA data ----
setkey(census_tract, GEOID)
census_tract[,census_tract := as.numeric(GEOID)]
hmda_match[,census_tract := as.numeric(census_tract)]

