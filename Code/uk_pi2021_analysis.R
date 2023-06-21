packages <- c("readxl","data.table","tidyverse")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

wd = "C:/Users/LLR User/OneDrive - The University of Nottingham/3. UoN Thesis and Study/uon-master-dissertation"
data.folder = "/UK Environmental Data/"
sheets = excel_sheets(paste0(wd,data.folder,"2021_Dataset/2021_PI_Dataset.xlsx"))

raw.dat.sub <- as.data.table(read_excel(paste0(wd,data.folder,"2021_Dataset/2021_PI_Dataset.xlsx"), sheet=sheets[1]))
raw.dat.rad <- as.data.table(read_excel(paste0(wd,data.folder,"2021_Dataset/2021_PI_Dataset.xlsx"), sheet=sheets[3]))
raw.description <- as.data.table(read_excel(paste0(wd,data.folder,"2021_Dataset/2021_PI_Dataset.xlsx"), sheet=sheets[4]))

View(raw.dat.sub)

#---- Clean Data ----
## Clean Substances Data
dat.sub <- raw.dat.sub[-c(1:9),]
colnames(dat.sub) <- as.character(raw.dat.sub[9,])
old.var <- colnames(dat.sub)
new.var <- c("id", "activity","operator","site.address","site.postcode",
             "easting","northing","ea.area","route","substance","report.threshold_kg",
             "release_kg","industry.sector","industry.subsector")
colnames(dat.sub) <- new.var

## Clean Radioactive data
dat.rad <- raw.dat.rad[-c(1:9),]
colnames(dat.rad) <- as.character(raw.dat.rad[9,])
old.var <- colnames(dat.rad)
new.var <- c("id", "operator", "site.address", "site.postcode",
             "easting","northing","ea.area","substance","route","release","unit",
             "annual.report.threshold","report.threshold", "industry.sector")
colnames(dat.rad) <- new.var

#---- Data Exploratory ----
dat.sub[,.N,industry.sector]
dat.sub[release_kg > 1][,c("substance","release_kg","report.threshold_kg")][,.N,substance][N > 100][order(N)]
dat.sub[substance %in% c("Carbon dioxide","Methane", "Particulate matter - PM10", "Ammonia")]

# Select only Air route
small.dat.sub <- dat.sub[route=="Air"][,c("release_kg","substance","site.postcode","report.threshold_kg")][is.na(release_kg)==F]

small.dat.sub[,.N,report.threshold_kg]
small.dat.sub[,release_kg := as.integer(release_kg)]
small.dat.sub[,over := release_kg - as.integer(report.threshold_kg)] # Check the data which Release > Threshold
hist(small.dat.sub[over < 100 & over > -100][,over])

hist(small.dat.sub[release_kg < 50][,release_kg])