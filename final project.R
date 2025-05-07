# Step 1: Load our packages:
library(tidycensus)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(classInt)
library(ggspatial)
library(sfhotspot)
library(patchwork)
library(osrm)
library(biscale)
library(sfdep)
library(janitor)
#Step 2: Set working directory

#Step 3: Import EJI data and filter
eji <- read_csv("New York.csv") %>%
  filter( `COUNTY` %in% c("Bronx", "Kings", "New York", "Queens", "Richmond")) %>%
  janitor:: clean_names() %>%
  mutate(geoid = as.character(geoid))

# Saving your data in shapefiles with the {sf} package 
ny_shp <- st_read("tract/tl_2024_36_tract.shp") %>%
  janitor:: clean_names() 
ny_shp <- ny_shp %>%  
  mutate(geoid = as.character(geoid)) 
ny_shp$geoid


eji1 <- right_join(ny_shp, eji, by = "geoid")

st_write(eji1,"eji.shp", append=FALSE)

crosswalkeji <- read_csv("zcta_tract_rel_10.txt") |> 
  clean_names() %>%
  filter(state == "36")

# Join EJI to crosswalk
eji_weighted <- crosswalkeji |> 
  left_join(eji, by = "geoid") |> 
  mutate(weighted_eji = rpl_eji * poppt) |> 
  group_by(zcta5) 

modzcta_shapes <-  st_read("modzcta_2010.shp")
# First, rename zcta5 to match column in modzcta_shapes
eji_weighted <- eji_weighted |> 
  rename(zcta = zcta5)

# Join to spatial shape using standard left join
modzcta_eji <- modzcta_shapes |> 
  left_join(eji_weighted, by = "zcta")


         #Base map
# Apply a theme
ggplot() +
  geom_sf(data = eji1,
          fill = "cornflowerblue",  
          color = "black",  
          linewidth = 0.05,  
          alpha = .5) +
  theme_void()


#COVID dataset
modzcta_data <- read_csv("data-by-modzcta.csv")

modzcta_sf <- modzcta_data %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)  # WGS 84 Coordinate System
ny_shp <- st_transform(ny_shp, st_crs(modzcta_sf))
modzcta_tract <- st_join(modzcta_sf, ny_shp, join = st_within)

#Create cross walk for future use
crosswalk <- modzcta_tract %>%
  select(MODIFIED_ZCTA, tractce) 


write_csv(crosswalk, "modzcta_to_census_tract.csv")

#Apply crosswalk
census_tract_data <- left_join(crosswalk, modzcta_data, by = "MODIFIED_ZCTA")

# Jenks natural breaks (use style = "jenks")
quant_breaks <- classIntervals(modzcta_eji$weighted_rpl_eji, n = 4, style = "quantile")$brks
print(quant_breaks)

# Create the map
ggplot() +
  geom_sf(data = modzcta_eji, 
          aes(fill = weighted_rpl_eji), 
          color = "black", 
          linewidth = 0.2, 
          na.value = "lightgray") +
  
  # Use a continuous scale with custom breaks and labels
  scale_fill_fermenter(
    palette = "Reds",
    direction = 1,
    guide = "legend",
    breaks = quant_breaks,
    name = "Percentile Rank"
  ) +
  
  guides(fill = guide_legend(
    reverse = TRUE,
    title.position = "top",
    title.hjust = 0.5
  )) +
  
  # Title
  labs(title = "Environmental Justice Burden by Modified ZCTA",
       caption = "Author: Tobechi Dimkpa \nSources: CDC EJI \nDate: March 25,2025") +
  
  # Layout adjustments
  theme_void() +
  theme(plot.title = element_text(size = 13,
                                  family  = "Avenir",
                                  vjust = 3,
                                  hjust = 0.10),
        plot.title.position = "plot",
        legend.title = element_text(size = 10),
        legend.position = c(0.22, 0.63),
        legend.key.size = unit(.4, 'cm')) +
  annotation_north_arrow(height = unit(.8, "cm"),
                         width = unit(.6, "cm"),
                         pad_x = unit(0.1, "in"), 
                         pad_y = unit(0.001, "in"),
                         style = north_arrow_minimal) +
  annotation_scale(style = "ticks",
                   pad_x = unit(0.4, "in"),
                   pad_y = unit(.15, "in"),
                   unit_category = "imperial",
                   width_hint = .2) -> map1
  
ggsave(map1,
       filename = "eji_map.jpg")

#Covid case rate, death rate data
# Load the MODZCTA GeoJSON
modzcta_shapes <- st_read("Modified Zip Code Tabulation Areas (MODZCTA)_20250325.geojson")

# Ensure ZCTA column is character type
modzcta_shapes$modzcta <- as.character(modzcta_shapes$modzcta)
modzcta_tract$MODIFIED_ZCTA <- as.character(modzcta_tract$MODIFIED_ZCTA)

# Join COVID data with the polygon shapes
# Drop geometry from modzcta_tract
modzcta_tract_df <- st_drop_geometry(modzcta_tract)

# Join to spatial polygons
covid_zcta <- left_join(modzcta_shapes, modzcta_tract_df, by = c("modzcta" = "MODIFIED_ZCTA"))


#case rate:
rate_quant_breaks <- classIntervals(covid_zcta$COVID_CASE_RATE, n = 5, style = "quantile")$brks
ggplot() +
  geom_sf(data = covid_zcta,
          aes(fill = COVID_CASE_RATE),
          color = "black",
          linewidth = .2) +
  scale_fill_fermenter(direction = 1,
                       palette = "YlOrRd",
                       breaks = rate_quant_breaks) +
  labs(title = "COVID-19 Case Rate \nper 100k Residents",
       fill = "Case Rate") +
  theme_void() + 
  theme(plot.title = element_text(size = 13,
                                  family = "Avenir",
                                  vjust = -10,
                                  hjust = 0.10),
        plot.title.position = "plot",
        legend.title = element_text(size = 12),
        legend.position = c(0.22, 0.63),
        legend.key.size = unit(.4, 'cm')) +
  annotation_scale(pad_x = unit(1.4, "in"),
                   pad_y = unit(.3, "in"),
                   unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(.8, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(1, "in"), 
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) -> map2
ggsave(map2,
       filename = "covid_case_rate.jpg")

#Death rate:
death_quant_breaks <- classIntervals(covid_zcta$COVID_DEATH_RATE, n = 5, style = "quantile")$brks
ggplot() +
  geom_sf(data = covid_zcta,
          aes(fill = COVID_DEATH_RATE),
          color = "black",
          linewidth = .2) +
  scale_fill_fermenter(direction = 1,
                       palette = "YlOrRd",
                       breaks = death_quant_breaks) +
  labs(title = "COVID-19 Death Rate \nper 10k Residents",
       fill = "Case Rate") +
  theme_void() + 
  theme(plot.title = element_text(size = 13,
                                  family = "Avenir",
                                  vjust = -10,
                                  hjust = 0.10),
        plot.title.position = "plot",
        legend.title = element_text(size = 12),
        legend.position = c(0.22, 0.63),
        legend.key.size = unit(.4, 'cm')) +
  annotation_scale(pad_x = unit(1.4, "in"),
                   pad_y = unit(.3, "in"),
                   unit_category = "imperial") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(.8, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(1, "in"), 
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) -> map3
ggsave(map3,
       filename = "covid_death_rate.jpg")

#Bivariate Map: EJI and Covid Case/Death rate
bivariate <- st_join(covid_zcta, modzcta_eji)

#Calculate spatial lag of EJI
bivariate <- bivariate |>
  filter(!is.na(weighted_rpl_eji))
bivariate <- bivariate |>
  mutate(nb = st_knn(geometry, 45),           # Identify neighbors
         wt = st_weights(nb, 
                         allow_zero = TRUE,           # Allow empty neighbor sets
                         style = "W"),                # Row standardized 
         eji_lag = st_lag(weighted_rpl_eji, nb, wt))  # Calculate spatial lag

#Question: Error Message: Variable contains non-finite values. Should I change NA to zero or filter NA?

map1 + map2 + map3