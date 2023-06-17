packages <- c("readxl","data.table","tidyverse")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

wd = "C:/Users/LLR User/OneDrive - The University of Nottingham/3. UoN Thesis and Study/uon-master-dissertation"
data.folder = "/UK Environmental Data/"
raw.dat <- as.data.table(read_excel(paste0(wd,data.folder,"2021_Dataset/2021_PI_Dataset.xlsx"), sheet="2021 Substances"))


description <- as.data.table(read_excel(paste0(wd,data.folder,"2021_Dataset/2021_PI_Dataset.xlsx"), sheet="Explanation of terms"))



help(read_excel)

#---- Clean Data ----
# Remove first 8 rows and change names
colnames(raw.dat) <- as.character(raw.dat[9,])
description <- raw.dat[1:8,1]
raw.dat <- raw.dat[-c(1:9),]

# Rename variables
old.var <- colnames(raw.dat)
new.var <- c("id", "activity","operator","site.address","site.postcode",
             "easting","northing","ea.area","route","substance","report.threshold_kg",
             "release_kg","industry.sector","industry.subsector")
colnames(raw.dat) <- new.var

raw.dat[,.N,c("substance","operator")][N > 30]

#---- Data Exploratory ----

