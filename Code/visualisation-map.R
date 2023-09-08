packages <- c("maps","mapdata","usmap","ggplot2","dplyr","data.table","sf","ggmap")
need.install <- packages[!(packages %in% installed.packages()[,"Package"])]

lapply(need.install,install.packages,character.only=T)
lapply(packages,library,character.only=T)

text_style <- theme(text = element_text(size=14),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=14))

#---- 1. Create release map ----
tri_yearfips <- readRDS(file = paste0(wd,panel.folder,"tri_yearfips.rds"))
release_map <- tri_yearfips[,.(carc_releases = log(sum(carc_releases)+1),
                               carc_onsite = log(sum(carc_onsite)+1),
                               total_releases = log(sum(total_releases)+1)),
                            by = fips]

release_map[,fips := as.character(fips)]
release_map[nchar(fips) < 5,fips := paste0(0, fips)]
counties <- as.data.table(usmap::countypop)

release_map <- as.data.table(merge(release_map, counties, all.y=T, on="fips"))

for (i in names(release_map)) {
  release_map[is.na(get(i)),(i):=0] ## To put all NA county to zero
}

## Draw map
release_map <- plot_usmap(data = release_map, values = "total_releases", color = "gray80") +
  scale_fill_gradient(low="lightyellow",
                      high="red3") +
  labs(title="",
       fill="Total Releases       \n(Log-Transformed)      ") +
  theme(panel.background = element_rect(color="grey80",size=0.5),
        legend.position = "bottom",
        legend.justification = "center") +
  # guides(color = guide_legend(override.aes = list(size=5))) +
  text_style
print(release_map)
setwd(paste0(wd,graph.folder))
ggsave(file = "release_map.jpg", release_map, width = 30, height = 20, units = "cm")
  
#---- 2. Create number of facilities map ----
tri_match <-readRDS(file=paste0(wd,tri.folder,"tri_match.rds"))
nfac_map <- tri_match[!(state %in% c("HI"))][,.(state,latitude,longitude,fips,year,carcinogen)]
                      
# nfac_map <- st_as_sf(nfac_map,coords=c("longitude","latitude"))
# nfac_map <- st_set_crs(nfac_map,value=4326)

nfac_map_transformed <- usmap::usmap_transform(nfac_map,
                       input_names=c("longitude","latitude"),
                       output_names=c("x","y"))

facility_map <- plot_usmap(regions = "states") +
  geom_point(data = nfac_map_transformed,
             aes(x = x, y = y, color = carcinogen, shape = carcinogen),
             size = 1) +
  scale_color_manual(values=c("orange","red4"),
                     labels=c("No","Yes")) +
  scale_shape_manual(values=c(19,17),
                     labels=c("No","Yes")) +
  labs(title="",
       color="Carcinogen-released facility",
       shape="Carcinogen-released facility") +
  theme(panel.background = element_rect(color = "grey80",size=0.5),
        legend.position = "bottom",
        legend.justification = "center") +
  guides(color = guide_legend(override.aes = list(size=2))) +
  text_style

print(facility_map)
setwd(paste0(wd,graph.folder))
ggsave(file = "facility_map.jpg", facility_map, width = 30, height = 20, units = "cm")

#---- 3. Create rate_spread map ----
hmda_match <- readRDS(file=paste0(wd,hmda.folder,"hmda_match.rds"))
hmda_match[,fips:=substring(year_fips,6)]
hmda_match[,fips := as.character(fips)]
hmda_match[nchar(fips) < 5,fips := paste0(0, fips)]

## Create rate spread by counties
fips_rate <- hmda_match[,.(rate_spread=mean(rate_spread),interest_rate=mean(interest_rate),us30_spread=mean(us30_spread)),fips]
counties <- as.data.table(usmap::countypop)

rate_map <- as.data.table(merge(fips_rate, counties, all.y=T, on="fips"))

## Draw map
rate_mapplot <- plot_usmap(data = rate_map, values = "rate_spread", color = "gray80") +
  scale_fill_steps2() +
  labs(title="",
       fill="Rate Spread") +
  theme(panel.background = element_rect(color="grey80",size=0.5),
        legend.position = "bottom",
        legend.justification = "center",
        legend.key.width=unit(2,"cm")) +
  text_style
print(rate_mapplot)
setwd(paste0(wd,graph.folder))
ggsave(file = "rate_map.jpg", rate_mapplot, width = 30, height = 20, units = "cm")


