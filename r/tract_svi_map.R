

#### Map 3: Baltimore City, Social Vulnerability by Census Tracts, overlaid with zip boundaries ----

# read in tract data
md_tracts <- st_read("data_raw/Maryland_Census_Data_-_Census_Tracts/Maryland_Census_Data_-_Census_Tracts.shp")
# filter for baltimore
bmore_tracts <- md_tracts %>% filter(CNTY2010 == "24510")

# zip code data
zips <- st_read("data_raw/Maryland_Census_Data_-_ZIP_Code_Tabulation_Areas_(ZCTAs)/")

bmore_zips <- zips %>% filter(str_detect(ZCTA5CE10, "^2120|^2121|^2122"))

bmore_tracts <- st_transform(bmore_tracts, 26985)
bmore_zips<- st_transform(bmore_zips, 26985)

# map
tm_shape(bmore_tracts) +
  tm_borders(col = "blue", alpha = 0.8) +
  tm_shape(bmore_zips) +
  tm_borders()

# Load SVI data
md_svi <- st_read("data_raw/Maryland SVI/SVI2018_MARYLAND_tract.shp")

# filter for baltimore
bmore_svi <- md_svi %>% filter(STCNTY == "24510") %>% select(ST, STATE, ST_ABBR, STCNTY, COUNTY, FIPS, LOCATION, E_TOTPOP, E_POV,
                                                             RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES) %>%
  st_transform(26985)

# code -999s as NA
bmore_svi <- bmore_svi %>% mutate_if(is.numeric, ~replace(., . == -999.000, NA))

# map
svi_map <- # Overall Tract Summary
  tm_shape(bmore_svi) +
  tm_fill(col = "RPL_THEMES", alpha = 0.8, 
          title = "Social Vulnerability",
          palette = "BuPu") +
  tm_shape(bmore_zips, bbox = bmore_bbox) +
  tm_borders(col = "black", lwd = 0.7) +
  tm_text("ZCTA5CE10", size = 0.8) +
  tm_layout(frame = FALSE, legend.title.size = 1.5, legend.text.size = 1,
            legend.bg.color = "white")
svi_map

svi_minority_map <- # Minority Status and Language
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

tmap_save(svi_map, "maps/svi_map.png")
tmap_save(svi_minority_map, "maps/svi_minority_map.png")
