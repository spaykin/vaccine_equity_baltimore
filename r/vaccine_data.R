library(tidyverse)
library(sp)
library(tmap)
library(USAboundaries)
install.packages("USAboundariesData", repos = "http://packages.ropensci.org", type = "source")

# Counties boundaries

counties <- us_counties()
md_counties <- counties %>% filter(state_name == "Maryland")

geoid <- md_counties$geoid


#### MD County Map 1: Death Counts ----

# Data from: https://coronavirus.maryland.gov/datasets/mdcovid19-confirmeddeathsbycounty/data?page=13

# population
md_county_pop <- read.csv("data_raw/MD_county_pop.csv")

# deaths
county_deaths <- read.csv("data_raw/MDCOVID19_ConfirmedDeathsByCounty.csv")
county_deaths <- county_deaths %>% filter(OBJECTID == 309) %>% select(3:26)

county_deaths2 <- county_deaths %>% gather(County, Death_Count, 1:24)
county_deaths3 <- gsub('\\.', ' ', county_deaths2$County, fixed = FALSE)
county_deaths2 <- cbind(county_deaths2, county_deaths3)

county_deaths4 <- county_deaths2 %>% select(County = county_deaths3, Death_Count)

county_merge <- left_join(county_deaths4, md_county_pop, by = "County")

county_merge[county_merge == "Queen Annes"] <- "Queen Anne's"
county_merge[county_merge == "Prince Georges"] <- "Prince George's"
county_merge[county_merge == "St Marys"] <- "St. Mary's"

# death count per 100k
county_merge$Death_Count_100K <- (county_merge$Death_Count / county_merge$Population) * 100000

county_merge_geom <- merge(md_counties, county_merge, by.x = "name", by.y = "County")

# map 
tmap_mode("plot")

tm_shape(county_merge_geom) +
  tm_fill("Death_Count_100K", 
          id = "name", 
          title = "Death Count per 100K") +
  tm_layout(frame = FALSE)


#### MD County Map 1: Death Counts ----
     