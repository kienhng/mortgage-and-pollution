packages <- c("readxl","data.table","tidyverse","haven")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)


sampled_panel <- as.data.table(read_dta(paste0(wd,panel.folder,"sampled_panel.dta")))
full_panel <- readRDS(file=paste0(wd,panel.folder,"full_panel.rds"))

#---- 1. Summary Statisitcs ---- 

full_panel[,carc_2level := ifelse(carc_releases > 0 & effect_10km > aland_cou, 1,0)]
full_panel[,.N,carc_2level]

sampled_panel

normlised_control <- full_panel[dec_property_value == 1][dec_income == 1][dec_loan_to_value == 1][race == 1][carc_2level == 0][,rate_spread]
normlised_treatment <- full_panel[dec_property_value == 1][dec_income == 1][dec_loan_to_value == 1][race == 1][carc_2level == 1][,rate_spread]

t.test(normlised_control,normlised_treatment)

full_panel[,radius_check := sqrt(aland_cou/pi)]
summary(full_panel[,radius_check])

full_panel[is.na(aland_cou)]