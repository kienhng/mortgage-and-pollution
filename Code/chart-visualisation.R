packages <- c("readxl","data.table","tidyverse","haven","ggplot2","ggsci")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install, install.packages, character.only=T)
lapply(packages, library, character.only=T)

text_style <- theme(text = element_text(size=14),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=14),
                    axis.text=element_text(14))

sampled_panel <- as.data.table(read_dta(paste0(wd,panel.folder,"sampled_panel.dta")))

#---- 1. Summary Statisitcs ----
graph_panel <- sampled_panel[,ln_carc_releases:=log(carc_releases)
                             ][ln_carc_releases!="-Inf"
                              ][,ln_pa_carc_releases:=log(pa_carc_release)
                                ][ln_pa_carc_releases!="-Inf"
                                ][ln_carc_releases>quantile(ln_carc_releases,probs=0.01)&ln_carc_releases<quantile(ln_carc_releases,prob=0.99)
                                 ][!is.na(metro_dummy)
                                   ][!is.na(bank)]

graph_panel[,carc_dummy:="High Carcinogen Exposure"]
graph_panel[carc_releases > median(carc_releases),carc_dummy:="Low Carcinogen Exposure"]

graph_panel[,income_dummy:="Below Median Income"]
graph_panel[income > median(income),income_dummy:="Above Median Income"]

# Create text variables
graph_panel[race==1,text_race:="White"]
graph_panel[race==2,text_race:="Asian"]
graph_panel[race==3,text_race:="Black & African American"]
graph_panel[race==4,text_race:="Native Americans"]

graph_panel[bank==0,text_bank:="Bank"]
graph_panel[bank==1,text_bank:="Non-bank"]

graph_panel[purpose==1,text_purpose:="New purchase"]
graph_panel[purpose==3,text_purpose:="Refinancing"]

graph_panel <- graph_panel %>% 
  mutate(across(region, factor, levels=c("West","Midwest","Northeast","Southwest","Southeast")))

#---- 1. Race box plot ----
ratespread_race <- ggplot(data=graph_panel,aes(y=rate_spread,x=factor(text_race),fill=factor(text_race))) +
  ylim(-0.75,1.25) +
  geom_boxplot(alpha=0.9) +
  labs(x="",
       y="Rate Spread by U.S. Regions",
       fill="") +
  theme_light() +
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
         legend.position="bottom") +
  scale_fill_manual(values=c("grey90","orange2","grey80","grey70")) + 
  facet_wrap(vars(factor(region))) +
  text_style
print(ratespread_race)

setwd(paste0(wd,graph.folder))
ggsave(file="ratespread_race.png",ratespread_race,height = 15, width = 25, units="cm")

#---- 2. Bank and Non-bank box plot ----
spread_carcdummy_bank <- ggplot(data=graph_panel,aes(y=rate_spread,x=factor(text_bank),fill=factor(text_bank))) +
  ylim(-0.75,1.25) +
  geom_boxplot(alpha=0.9) + 
  labs(x="",
       y="Mortgage Rate Spread") +
  guides(fill="none") +
  scale_fill_manual(values=c("orange2","grey80")) + 
  theme_light() +
  text_style
print(spread_carcdummy_bank)

setwd(paste0(wd,graph.folder))
ggsave(file="spread_carcdummy_bank.png",spread_carcdummy_bank,height = 15, width = 15, units="cm")

#---- 3. Purpose box plot ----
spread_carcdummy_purpose <- ggplot(data=graph_panel,aes(y=rate_spread,x=factor(text_purpose),fill=factor(text_purpose))) +
  ylim(-0.75,1.25) +
  geom_boxplot(alpha=0.9) + 
  labs(x="",
       y="Mortgage Rate Spread") +
  scale_fill_manual(values=c("orange2","grey80")) + 
  guides(fill="none") +
  theme_light() +
  text_style

print(spread_carcdummy_purpose)
setwd(paste0(wd,graph.folder))
ggsave(file="spread_carcdummy_purpose.png",spread_carcdummy_purpose,height = 15, width = 15, units="cm")
