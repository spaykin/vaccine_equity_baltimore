#### Map 2: MD County Vaccines Delivered as of Feb 5 ----

library(RColorBrewer)

# read in data
vaxDeliv <- read.csv("data_raw/md_vaxDeliv.csv")

# merge with geom
vaxDeliv_geom <- merge(md_counties, vaxDeliv, by = "name")

vaxDeliv_geom <- st_transform(vaxDeliv_geom, 26985)

# map

# first dose
vax1 <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDeliv1",
          id = "name", 
          title = "Vaccines Delivered") +
  tm_layout(frame=FALSE)

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

# second dose
vax2 <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDeliv2",
          id = "name", 
          title = "Vaccines Delivered") +
  tm_layout(frame=FALSE)

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

# total doses
vaxTotal <- 
  tm_shape(vaxDeliv_geom) +
  tm_fill("vaxDelivT",
          id = "name", 
          title = "Vaccines Delivered") +
  tm_layout(frame=FALSE)

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

tmap_arrange(vax1, vax2, vaxTotal)
tmap_arrange(vax1_adj, vax2_adj, vaxTotal_adj)

# save maps
tmap_save(vax1_adj, "maps/vax1_adj.png")
tmap_save(vax2_adj, "maps/vax2_adj.png")
tmap_save(vaxTotal_adj, "maps/vaxTotal_adj.png")

# save data
st_write(vaxDeliv_geom, "data_final/vaxDeliv.shp")


