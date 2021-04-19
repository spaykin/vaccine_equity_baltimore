library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

#### About ----

# This script prepares data for mapping Baltimore City, Social Vulnerability Index by census tracts, overlaid with zip boundaries.

##### Load and clean data ----

# Read in MD tract data
md_tracts <- st_read("data_raw/Maryland_Census_Data_-_Census_Tracts/Maryland_Census_Data_-_Census_Tracts.shp")
# Filter for Baltimore
bmore_tracts <- md_tracts %>% filter(CNTY2010 == "24510")

# Read in MD zip code data
zips <- st_read("data_raw/Maryland_Census_Data_-_ZIP_Code_Tabulation_Areas_(ZCTAs)/")
# Filter for Baltimore zip codes
bmore_zips <- zips %>% filter(str_detect(ZCTA5CE10, "^2120|^2121|^2122"))
# Identify priority zip codes
priority_zips <- bmore_zips %>% filter(ZCTA5CE10 %in% c("21215", "21216", "21217", "21218", "21213", "21205"))

# Transform CRS
bmore_tracts <- st_transform(bmore_tracts, 26985)
bmore_zips<- st_transform(bmore_zips, 26985)

# Load MD SVI data
md_svi <- st_read("data_raw/Maryland SVI/SVI2018_MARYLAND_tract.shp")
# Filter for Baltimore, relevant variables, transform CRS
bmore_svi <- md_svi %>% filter(STCNTY == "24510") %>% select(ST, STATE, ST_ABBR, STCNTY, COUNTY, FIPS, LOCATION, E_TOTPOP, E_POV, E_PCI,
                                                             RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES) %>% st_transform(26985)
# Code -999s as NA
bmore_svi <- bmore_svi %>% mutate_if(is.numeric, ~replace(., . == -999.000, NA))

# Baltimore bounding box
bmore_bbox <- st_bbox(bmore_tracts)

##### Map ----

# SVI: Overall Ranking Summary
svi_map <-
  tm_shape(bmore_svi) +
  tm_fill(col = "RPL_THEMES", alpha = 0.7, 
          title = "Social Vulnerability",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 1) +
  tm_text("ZCTA5CE10", size = 0.9, bg.color = "white", bg.alpha = 0.5) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")
svi_map

svi_map_priority <-
  tm_shape(bmore_svi) +
  tm_fill(col = "RPL_THEMES", alpha = 0.8, 
          title = "Social Vulnerability",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_text("ZCTA5CE10", size = 1, bg.color = "white", bg.alpha = 0.5) +
  tm_shape(priority_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 3) +
  tm_text("ZCTA5CE10", size = 1, bg.color = "white", bg.alpha = 0.5) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")

svi_map_priority
tmap_save(svi_map_priority, "maps/svi_map_priority.png")

svi_map_priority1 <-
  tm_shape(bmore_svi) +
  tm_fill(col = "RPL_THEMES", alpha = 0.8, 
          title = "Social Vulnerability",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_text("ZCTA5CE10", size = 1, bg.color = "white", bg.alpha = 0.4) +
  tm_shape(priority_zips) +
  tm_borders(col = "black", lwd = 3) +
  tm_text("ZCTA5CE10", size = 1, col = "black", 
          bg.color = "magenta", bg.alpha = 0.5) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")

svi_map_priority1
tmap_save(svi_map_priority1, "maps/svi_map_priority1.png")


svi_map_priority2 <-
  tm_shape(bmore_svi) +
  tm_fill(col = "RPL_THEMES", alpha = 0.8, 
          title = "Social Vulnerability",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_text("ZCTA5CE10", size = 1, col = "black") +
  tm_shape(priority_zips) +
  tm_borders(col = "black", lwd = 2.5) +
  tm_text("ZCTA5CE10", size = 1, col = "black",
          bg.color = "magenta", bg.alpha = 0.5) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")

svi_map_priority2
tmap_save(svi_map_priority2, "maps/svi_map_priority2.png")

svi_map_priority3 <-
  tm_shape(bmore_svi) +
  tm_fill(col = "RPL_THEMES", alpha = 0.8, 
          title = "Social Vulnerability",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_text("ZCTA5CE10", size = 1.2, col = "black") +
  tm_shape(priority_zips) +
  tm_borders(col = "black", lwd = 2.5) +
  tm_text("ZCTA5CE10", size = 1.2, col = "black",
          bg.color = "magenta", bg.alpha = 0.5) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")

svi_map_priority3
tmap_save(svi_map_priority3, "maps/svi_map_priority3.png")

svi_map_priority4 <-
  tm_shape(bmore_svi) +
  tm_fill(col = "RPL_THEMES", alpha = 0.8, 
          title = "Social Vulnerability",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_text("ZCTA5CE10", size = 1.2, bg.color = "white", bg.alpha = 0.4) +
  tm_shape(priority_zips) +
  tm_borders(col = "black", lwd = 3) +
  tm_text("ZCTA5CE10", size = 1.2, col = "black", 
          bg.color = "magenta", bg.alpha = 0.5) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")

svi_map_priority4
tmap_save(svi_map_priority4, "maps/svi_map_priority4.png")

# SVI: Minority Status and Language
svi_minority_map <- 
  tm_shape(bmore_svi) +
  tm_fill(col = "RPL_THEME3", alpha = 0.8, 
          title = "Social Vulnerability",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_text("ZCTA5CE10", size = 0.8) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")
svi_minority_map

# Per Capita Income
svi_income_map <- 
  tm_shape(bmore_svi) + 
  tm_fill(col = "E_PCI", alpha = 0.8, 
          title = "Income",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_text("ZCTA5CE10", size = 0.8) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")
svi_income_map

#### Save ----

tmap_save(svi_map, "maps/svi_map.png")
tmap_save(svi_minority_map, "maps/svi_minority_map.png")
tmap_save(svi_income_map, "maps/income_map.png")
