packages <- c("readxl","data.table","tidyverse")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

wd = "C:/Users/LLR User/OneDrive - The University of Nottingham/3. UoN Thesis and Study/uon-master-dissertation"
data.folder = "/UK Environmental Data/"
sheets = excel_sheets(paste0(wd,data.folder,"2021_Dataset/2021_PI_Dataset.xlsx"))
