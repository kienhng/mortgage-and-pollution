packages <- c("readxl","data.table","tidyverse","haven","zipcodeR", "httr","jsonlite")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

wd <- getwd()
hmda.folder <- "/Data/RawData-HMDA"
hmda_raw <- as.data.table(read_dta(paste0(wd,hmda.folder,"/hmda_2018-2021.dta")))

colnames(hmda_raw)
head(hmda_raw)

hmda_raw[,.N,year]

#---- Compare HMDA and TRI data on ID ----
hmda_tracts <- as.numeric(hmda_raw[year == 2021][,census_tract])
tri_zip <- as.numeric(tri_clean[,zip])
tri_tracts <- list()

for (i in 1:length(tri_zip)) {
  tryCatch(
    {
      tri_tracts[[length(tri_tracts)+1]] <- get_tracts(tri_zip[i])[["GEOID"]]
    },
  error=function(e){}
  )
}

check(10)

get_tracts_skip_errors(tri_tracts)

get_tracts(tri_zip[1])[["GEOID"]]