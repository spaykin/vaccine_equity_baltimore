library(tidyverse)
library(sf)
library(tmap)

#### About ----

# This script prepares Maryland state correctional facilities location data for overlay with COVID death maps. 

#### Load and clean data ----

# Load data
corrections <- st_read("data_raw/Maryland_Correctional_Facilities_-_State_Correctional_Facilities/Maryland_Correctional_Facilities_-_State_Correctional_Facilities.shp")

# Update projection
st_crs(corrections)
corrections <- st_transform(corrections, 26985)

#### Map ----

cty_death_corrections <- 
  tm_shape(county_merge_geom) +
  tm_fill("Death_Count_100K", 
          id = "name", 
          title = "Death Count per 100K") +
  tm_borders(lwd = 0.2) +
  #tm_text("name", size = "AREA", ymod = .4, col = "black") +
  tm_shape(corrections) +
  tm_markers(size = 0.5, ymod = 0.4) +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
cty_death_corrections

#### Save ----

tmap_save(cty_death_corrections, "maps/corrections_deaths_adj.png")
tmap_save(cty_death_corrections, "maps/corrections_deaths_adj_text.png")


