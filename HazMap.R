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



# -----------------------------------------------------------------------------


file_path <- "C:/Users/Admin/OneDrive/Documents/GitHub/HazMap/www/sample_dataframe_ireland.csv"
sample_data <- read.csv(file_path, stringsAsFactors = FALSE)



# -----------what points fall in the dangerzone
sample_data_sf <- sample_data %>%
  mutate(longitude_orig = longitude,
         latitude_orig = latitude) %>%
  st_as_sf(coords = c("longitude_orig", "latitude_orig"), crs = 4326)

#points_in_danger <- sample_data_sf %>%  filter(st_within(geometry, danger_zone, sparse = FALSE))

points_in_flood_danger <- sample_data_sf %>%
  filter(
    lengths(st_within(geometry, danger_zone)) > 0 |
      lengths(st_within(geometry, coastal_danger_zone)) > 0)


# ---------------- show points on map

sample_map <- points_in_flood_danger %>%  # sample_data , points_in_danger
    leaflet() %>%
      addTiles() %>%  
      addCircleMarkers(
        lng = ~longitude, 
        lat = ~latitude,  
        popup = ~paste("Policy Id:", policy_id, "<br>",
                       "Category 1:", category_1, "<br>",
                       "Category 2:", category_2, "<br>",
                       "Premium:", premium, "<br>",
                       "Sum Insured:", sum_insured),
        radius = 5, 
        color = "blue",
        fillOpacity = 0.7
      ) %>%
      setView(lng = -8, lat = 53.5, zoom = 8)  

sample_map

# ------------------------

sample_map_2 <- map +
  geom_point(
    data = points_in_flood_danger,
    aes(x = longitude, y = latitude, color = category_1, shape = category_2),
    size = 2,  
    alpha = 0.7  
  ) +
  scale_color_manual(values = c("personal" = "blue", "commercial" = "red")) + 
  theme_minimal() +
  labs(
    title = "Sample Data Points on Ireland Map",
    subtitle = "Categorized by Category 1 and Category 2",
    x = "Longitude",
    y = "Latitude",
    color = "Category 1",
    shape = "Category 2"
  )

sample_map_2



