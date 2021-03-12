library(tidyverse)
library(sf)
library(tmap)
library(USAboundaries)
library(USAboundariesData)

#### Map 1: MD County Death Counts as of Feb 5 ----

# Data from: https://coronavirus.maryland.gov/datasets/mdcovid19-confirmeddeathsbycounty/data?page=13

# Counties boundaries
md_counties <- st_read("data_raw/md_counties.shp")

# population
md_county_pop <- read.csv("data_raw/MD_county_pop.csv")

# COVID deaths
county_deaths <- read.csv("data_raw/MDCOVID19_ConfirmedDeathsByCounty.csv")
county_deaths <- county_deaths %>% filter(OBJECTID == 309) %>% select(3:26)

county_deaths2 <- county_deaths %>% gather(County, Death_Count, 1:24)
county_deaths3 <- gsub('\\.', ' ', county_deaths2$County, fixed = FALSE)
county_deaths2 <- cbind(county_deaths2, county_deaths3)

county_deaths4 <- county_deaths2 %>% select(County = county_deaths3, Death_Count)

# merge deaths and population data
county_merge <- left_join(county_deaths4, md_county_pop, by = "County")
county_merge[county_merge == "Queen Annes"] <- "Queen Anne's"
county_merge[county_merge == "Prince Georges"] <- "Prince George's"
county_merge[county_merge == "St Marys"] <- "St. Mary's"

# calculate death count per 100k
county_merge$Death_Count_100K <- (county_merge$Death_Count / county_merge$Population) * 100000

# merge with geometry
county_merge_geom <- merge(md_counties, county_merge, by.x = "name", by.y = "County")

# crs
county_merge_geom <- st_transform(county_merge_geom, 26985)

# map
tmap_mode("plot")

# Map 1 - MD county COVID death count per 100K
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

cty_death_map2 <- 
  tm_shape(county_merge_geom) +
  tm_fill("Death_Count_100K", 
          id = "name", 
          title = "Death Count per 100K") +
  tm_borders(lwd = 0.2) +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
cty_death_map2

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

cty_death_map4 <- 
  tm_shape(county_merge_geom) +
  tm_fill("Death_Count", 
          id = "name", 
          title = "Total Death Count",
          style = "jenks") +
  tm_borders(lwd = 0.2) +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
cty_death_map4

tmap_save(cty_death_map1, "maps/deaths_map_adj_text.png")
tmap_save(cty_death_map2, "maps/deaths_map_adj.png")
tmap_save(cty_death_map3, "maps/deaths_map_total_text.png")
tmap_save(cty_death_map4, "maps/deaths_map_total.png")

# save data 
st_write(county_merge_geom, "data_final/md_county_deaths.csv")
st_write(county_merge_geom, "data_final/md_county_deaths.shp")



