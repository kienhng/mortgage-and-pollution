packages <- c("readxl","data.table","tidyverse","haven")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

## Cheking propensity score

reg_panel <- as.data.table(read_dta(paste0(wd,panel.folder,"reg_panel_sampled1.dta")))

reg_panel[,carc_re2level := ifelse(carc_releases == 0,0,1)]
reg_panel[,carc_re2level := ifelse(is.na(carc_re2level)==T,0,carc_re2level)]
reg_panel[,.N,.(carc_re2level,region)][order(region)]
reg_panel[region == ""]

carc_re2level_extreme total_releases dec_income dec_property_value dec_loan_to_income