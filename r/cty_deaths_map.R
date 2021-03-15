library(tidyverse)
library(sf)
library(tmap)
library(USAboundaries)
library(USAboundariesData)

#### About ----

# This script loads and cleans State of Maryland COVID data for mapping COVID Death Counts by county, as of February 5, 2021.

#### Load and clean data ----

# Load county boundary geometry data
md_counties <- st_read("data_raw/md_counties.shp")

# Load county population data
md_county_pop <- read.csv("data_raw/MD_county_pop.csv")

# Load COVID death count data
county_deaths <- read.csv("data_raw/MDCOVID19_ConfirmedDeathsByCounty.csv")
# Clean up data - filter for count as of Feb 5, reshape
county_deaths <- county_deaths %>% filter(OBJECTID == 309) %>% select(3:26)
county_deaths2 <- county_deaths %>% gather(County, Death_Count, 1:24)
county_deaths3 <- gsub('\\.', ' ', county_deaths2$County, fixed = FALSE)
county_deaths2 <- cbind(county_deaths2, county_deaths3)
county_deaths4 <- county_deaths2 %>% select(County = county_deaths3, Death_Count)

# Merge death count + population data
county_merge <- left_join(county_deaths4, md_county_pop, by = "County")
county_merge[county_merge == "Queen Annes"] <- "Queen Anne's"
county_merge[county_merge == "Prince Georges"] <- "Prince George's"
county_merge[county_merge == "St Marys"] <- "St. Mary's"

# Calculate new variable - death count per 100k
county_merge$Death_Count_100K <- (county_merge$Death_Count / county_merge$Population) * 100000

# Merge cleaned data with county geom
county_merge_geom <- merge(md_counties, county_merge, by.x = "name", by.y = "County")

# Update CRS
county_merge_geom <- st_transform(county_merge_geom, 26985)

#### Map ----

# Raw death count
cty_death_map1 <- 
  tm_shape(county_merge_geom) +
  tm_fill("Death_Count_100K", 
          id = "name", 
          title = "Death Count per 100K") +
  tm_borders(lwd = 0.2) +
  tm_text("name", size = "AREA", ymod = .5) +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
cty_death_map1

# Death count per 100K
cty_death_map2 <- 
  tm_shape(county_merge_geom) +
  tm_fill("Death_Count_100K", 
          id = "name", 
          title = "Death Count per 100K") +
  tm_borders(lwd = 0.2) +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
cty_death_map2

# Raw death county with county labels
cty_death_map3 <- 
  tm_shape(county_merge_geom) +
  tm_fill("Death_Count", 
          id = "name", 
          title = "Total Death Count",
          style = "jenks") +
  tm_borders(lwd = 0.2) +
  tm_text("name", size = "AREA", ymod = .4, col = "black") +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
cty_death_map3

# Death count per 100K with county labels
cty_death_map4 <- 
  tm_shape(county_merge_geom) +
  tm_fill("Death_Count_100K", 
          id = "name", 
          title = "Death Count per 100K") +
  tm_borders(lwd = 0.2) +
  tm_text("name", size = "AREA", ymod = .4, col = "black") +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
cty_death_map4

#### Save ----

# Save maps
tmap_save(cty_death_map1, "maps/deaths_map_adj_text.png")
tmap_save(cty_death_map2, "maps/deaths_map_adj.png")
tmap_save(cty_death_map3, "maps/deaths_map_total_text.png")
tmap_save(cty_death_map4, "maps/deaths_map_total.png")

# Save data 
st_write(county_merge_geom, "data_final/md_county_deaths.csv")
st_write(county_merge_geom, "data_final/md_county_deaths.shp")
