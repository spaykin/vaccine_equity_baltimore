library(tidyverse)
library(sf)
library(tmap)
library(USAboundaries)
library(USAboundariesData)

#### About ----

# This script prepares data for mapping Baltimore census tracts by racial demographics, with zip codes overlaid.

##### Load and clean data ----

# Load data
tracts_race <- read.csv("data_raw/acs_race_tracts2019.csv")

# Select and rename variables
tracts_race <- tracts_race %>% rename(whiteP = DP05_0037PE, 
                                      blackP = DP05_0038PE,
                                      latinxP = DP05_0071PE,
                                      amerIndP = DP05_0039PE, 
                                      asianP = DP05_0044PE,
                                      hipiP = DP05_0052E) 
# Convert to numeric
tracts_race$blackP <- as.numeric(tracts_race$blackP)
tracts_race$whiteP <- as.numeric(tracts_race$whiteP)
tracts_race$latinxP <- as.numeric(tracts_race$latinxP)
tracts_race$amerIndP <- as.numeric(tracts_race$amerIndP)

# Merge with SVI dataset
tracts_all <- merge(bmore_svi, tracts_race, by.x = "FIPS", by.y = "geoid")

# Transform CRS
tracts_all <- st_transform(tracts_all, 26985)
bmore_zips <- st_transform(bmore_zips, 26985)

#### Map ----

# Percent Black
race_map <- 
  tm_shape(tracts_all) +
  tm_fill(col = "blackP", alpha = 0.8,
          title = "Percent Black", 
          palette = "BuPu") +
  tm_shape(bmore_zips) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_text("ZCTA5CE10", size = 0.8) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")
race_map

# Percent white
race_white_map <-
  tm_shape(tracts_all) +
  tm_fill(col = "whiteP", alpha = 0.8,
          title = "Percent White", 
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_text("ZCTA5CE10", size = 0.8) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")
race_white_map

# Percent Latinx
race_latinx_map <- 
  tm_shape(tracts_all) +
  tm_fill(col = "latinxP", alpha = 0.8,
          title = "Percent Latinx",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_text("ZCTA5CE10", size = 0.8) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")
race_latinx_map

#### Save ----

tmap_save(race_map, "maps/race_black_map.png")
tmap_save(race_latinx_map, "maps/race_latinx_map.png")
tmap_save(race_white_map, "maps/race_white_map.png")
