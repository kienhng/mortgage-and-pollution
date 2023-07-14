packages <- c("readxl","data.table","tidyverse","haven")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

#---- 1. HMDA sample ----
hmda_match <- readRDS(paste0(wd,panel.folder,"hmda_match.rds"))

#---- 2. TRI sample ----
tri_match_sum <- readRDS(paste0(wd,panel.folder,"tri_match_sum.rds"))

setkey(tri_match_sum, year_fips)
setkey(hmda_match, year_fips)

#---- 3. Create panel and run summary ----
reg_panel <- merge(tri_match_sum, hmda_match, all.x = FALSE, by = "year_fips")
setkey(reg_panel, year_fips)

##---- Creat treatment variables ----
reg_panel[,carc_air_pa := carc_air/aland_cou]
reg_panel[,carc_air_level := ifelse(carc_air_pa > 0, 1, 0)]
reg_panel[,carc_level := ifelse(carc_per_area > 0.00000, 1, 0)]
# ][,carc_level := ifelse(carc_per_area == 0, 0, carc_level)]

##---- Take sample from reg_panel ----
reg_panel_sampled <- reg_panel[sample(.N,4000000)]

write_dta(reg_panel_sampled ,path = paste0(wd,panel.folder,"reg_panel_sampled.dta"))
#write_dta(reg_panel,path = paste0(wd,panel.folder,"reg_panel.dta"))
#write_dta(reg_panel18,path = paste0(wd,panel.folder,"reg_panel18.dta"))
#write_dta(reg_panel21,path = paste0(wd,panel.folder,"reg_panel21.dta"))

##---- Run analysis ----
summary(lm(rate_spread ~ carc_per_area + loan_to_value_ratio + income + loan_amount + aland_pct_urb, reg_panel))
summary(lm(rate_spread ~ carc_level + race + loan_to_value_ratio + income + loan_amount + aland_pct_urb, reg_panel[carc_level != 2]))

hist(reg_panel18[carc_level == 0][rate_spread > -2 & rate_spread < 2]$rate_spread)
