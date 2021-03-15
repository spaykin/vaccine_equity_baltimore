library(tidyverse)
library(sf)
library(tmap)
library(RColorBrewer)

#### About ----

# This scipt prepares data for mapping COVID vaccines delivered to Maryland counties, as of February 5. 

#### Load and clean data ----

# Read in vaccine data
vaxDeliv <- read.csv("data_raw/md_vaxDeliv.csv")

# Merge with county geometry
vaxDeliv_geom <- merge(md_counties, vaxDeliv, by = "name")

vaxDeliv_geom <- st_transform(vaxDeliv_geom, 26985)

#### Map ----

# First dose
vax1 <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDeliv1",
          id = "name", 
          title = "Vaccines Delivered") +
  tm_layout(frame=FALSE)

# First dose, adjusted per 100K
vax1_adj <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDeliv1_adj",
          style = "jenks", 
          palette = "Blues",
          title = "Vaccines Delivered per 100K") +
  tm_borders(lwd = 0.2) +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
vax1_adj

# Second dose
vax2 <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDeliv2",
          id = "name", 
          title = "Vaccines Delivered") +
  tm_layout(frame=FALSE)

# Second dose, adjusted per 100K
vax2_adj <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDeliv2_adj",
          style = "jenks",
          palette = "Blues",
          title = "Vaccines Delivered per 100K") +
  tm_borders(lwd = 0.2) +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
vax2_adj

# Total doses
vaxTotal <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDelivT",
          id = "name", 
          title = "Vaccines Delivered") +
  tm_layout(frame=FALSE)

# Total doses, adjusted per 100K
vaxTotal_adj <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDelivT_adj",
          style = "jenks",
          palette = "Blues",
          title = "Vaccines Delivered per 100K") +
  tm_borders(lwd = 0.2) +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
vaxTotal_adj

# Total doses, adjusted per 100K, with county labels
vaxTotal_adj_names <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDelivT_adj",
          style = "jenks",
          palette = "Blues",
          title = "Vaccines Delivered per 100K") +
  tm_borders(lwd = 0.2) +
  tm_text("name", size = "AREA", ymod = .4, col = "black") +
  tm_layout(frame = FALSE, 
            legend.text.size = 1, legend.title.size = 1.5)
vaxTotal_adj_names

#### Save ----

# Save maps
tmap_save(vax1_adj, "maps/vax1_adj.png")
tmap_save(vax2_adj, "maps/vax2_adj.png")
tmap_save(vaxTotal_adj, "maps/vaxTotal_adj.png")
tmap_save(vaxTotal_adj_names, "maps/vaxTotal_adj_names.png")

# Save data
st_write(vaxDeliv_geom, "data_final/vaxDeliv.shp")


