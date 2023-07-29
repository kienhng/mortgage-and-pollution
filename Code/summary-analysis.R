packages <- c("readxl","data.table","tidyverse","haven")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

## Cheking propensity score

reg_sample <- as.data.table(read_dta(paste0(wd,panel.folder,"reg_panel_sampled.dta")))

reg_sample[,carc_2level := ifelse(carc_releases > 0 & aland_cou/(effect_5km*0.9) > 1, 1,0)]
t.test(reg_sample[carc_2level == 1][,rate_spread], reg_sample[carc_2level == 0][,rate_spread])

reg_panel[,.N,nfac_county][order(nfac_county)]
reg_panel[,avg_release_county := carc_releases/nfac_county]

reg_panel[,mean(avg_release_county),nfac_county][order(V1)]
