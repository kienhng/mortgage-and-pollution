packages <- c("usmap","ggplot2","dplyr")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install,install.packages,character.only=T)
lapply(packages,library,character.only=T)

tri_yearfips <- readRDS(file=paste0(wd,panel.folder,"tri_yearfips.rds"))

tri_yearfips <- tri_yearfips[,.(fips,state,f)]
