#-------------------------------------------------------------------------------
#                            Data Visualization
#-------------------------------------------------------------------------------

# Transform data into spatial format
c2018_transformed <- usmap_transform(clean_2018
                                     ,input_names = c("longitude", "latitude")
                                     ,output_names = c("x", "y"))

us_pop <- get_estimates(geography = "state", product = "population")
colnames(us_pop) <- c("county", "fips", "variable", "population")

plot_usmap(regions = "state", 
           data = us_pop, 
           values = "population",
           color = "white") +
  scale_color_gradient2(low = "red", 
                        mid = "white", 
                        high = "blue", 
                        midpoint = median(us_pop$population)) +
  geom_point(data = c2018_transformed, 
             aes(x = x, y = y, size = x5.2_stack_air),
             color = "yellow",
             alpha = 0.25)
#geom_point(data = c2018_transformed,
#          aes(x = x, y = y, size = x5.3_water),
#         color = "blue",
#        alpha = 0.25)

# FIPS codes data
fips <- raw_fipscode %>%
  mutate(fips = str_c(state_code, county_code)) %>%
  select(state, county, fips)
