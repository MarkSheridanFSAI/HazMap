for (file in list.files("script", pattern = "\\.R$", full.names = TRUE)) {source(file)}

# ----- Region
ireland <- ireland_shapefile()
ireland_bbox <- st_bbox(ireland) 
mapper(ireland,"Ireland")

# ----- Rivers
#https://www.hydrosheds.org/products/hydrorivers#downloads
link_to_river_shapefile <- "C:/Users/Admin/OneDrive/Documents/GitHub/HazMap/www/HydroRIVERS_v10_eu_shp/HydroRIVERS_v10_eu_shp/HydroRIVERS_v10_eu.shp"
rivers <- st_read(link_to_river_shapefile)
rivers_ireland <- st_crop(rivers, ireland_bbox) 
main_rivers_ireland <- rivers_ireland %>%
  filter(ORD_STRA > 3)

# ----- River Danger Zone
danger_zone <- st_buffer(main_rivers_ireland, dist = 3000) # Distance in meters (1 km)
danger_zone <- st_union(danger_zone) # Combine overlapping buffers into one polygon
danger_zone <- st_make_valid(danger_zone) # Ensure the geometry is valid

# ----- Coastal Danger Zone
ireland_dissolved <- st_union(ireland)
ireland_dissolved <- st_make_valid(ireland_dissolved)
coastline <- st_boundary(ireland_dissolved) 
coastal_danger_zone <- st_buffer(coastline, dist = 1000) # Negative buffer goes inland
coastal_danger_zone <- st_union(coastal_danger_zone) # Merge all coastal buffers
coastal_danger_zone <- st_make_valid(coastal_danger_zone)


# ----- Map
map <- mapper(ireland,"Ireland") + 
  geom_sf(data = main_rivers_ireland, color = "blue", size = 0.5) +
  geom_sf(data = danger_zone, fill = "orange", alpha = 0.4, color = NA) +
  geom_sf(data = coastal_danger_zone, fill = "yellow", alpha = 0.4, color = NA) 

map


# ----- ZOOM
county_name <- "Dublin"
county <- ireland %>% filter(county == county_name)
county_bbox <- st_bbox(county)
county_map <- map + coord_sf(
  xlim = c(county_bbox["xmin"], county_bbox["xmax"]),
  ylim = c(county_bbox["ymin"], county_bbox["ymax"]),
  expand = FALSE # Prevent automatic expansion of limits
) +
  ggtitle(county_name)

county_map
