for (file in list.files("script", pattern = "\\.R$", full.names = TRUE)) {source(file)}


# -----------------------------------------------------------------------------
# Nature
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
pluvial_danger_zone <- st_buffer(main_rivers_ireland, dist = 3000) # Distance in meters (1 km)
pluvial_danger_zone <- st_union(pluvial_danger_zone) # Combine overlapping buffers into one polygon
pluvial_danger_zone <- st_make_valid(pluvial_danger_zone) # Ensure the geometry is valid

# ----- Coastal Danger Zone
ireland_dissolved <- st_union(ireland)
ireland_dissolved <- st_make_valid(ireland_dissolved)
coastline <- st_boundary(ireland_dissolved) 
fluvial_danger_zone <- st_buffer(coastline, dist = 1000) # Negative buffer goes inland
fluvial_danger_zone <- st_union(fluvial_danger_zone) # Merge all coastal buffers
fluvial_danger_zone <- st_make_valid(fluvial_danger_zone)


# ----- Map
map <- mapper(ireland,"Ireland") + 
  geom_sf(data = main_rivers_ireland, color = "blue", size = 0.5) +
  geom_sf(data = pluvial_danger_zone, fill = "orange", alpha = 0.4, color = NA) +
  geom_sf(data = fluvial_danger_zone, fill = "yellow", alpha = 0.4, color = NA) 

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
# POLICIES

file_path <- "C:/Users/Admin/OneDrive/Documents/GitHub/HazMap/www/sample_dataframe_ireland.csv"
sample_data <- read.csv(file_path, stringsAsFactors = FALSE)
sample_data_sf <- sample_data %>%
  mutate(longitude_orig = longitude,
         latitude_orig = latitude) %>%
  st_as_sf(coords = c("longitude_orig", "latitude_orig"), crs = 4326)


# -----------what points fall in the dangerzone
#points_in_danger <- sample_data_sf %>%  filter(st_within(geometry, pluvial_danger_zone, sparse = FALSE))
points_in_flood_danger <- sample_data_sf %>%
  filter(lengths(st_within(geometry, pluvial_danger_zone)) > 0 | 
      lengths(st_within(geometry, fluvial_danger_zone)) > 0)


# ---------------- show points on map

policy_map <- map +
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
  #geom_sf(data = sample_data_sf, aes(color = "All Policies"), size = 1)

policy_map




# -------------------

storm_center <- st_sfc(st_point(c(-8.5, 53.5)), crs = 4326)
windstorm_area <- st_buffer(storm_center, dist = units::set_units(50, "km"))

policies_hit <- sample_data_sf %>%
  filter(st_within(geometry, windstorm_area, sparse = FALSE))

storm_map <- policy_map +
  geom_sf(data = windstorm_area, fill = "red", alpha = 0.3) +  # Windstorm area
  #geom_sf(data = sample_data_sf, aes(color = "All Policies"), size = 1) +
  geom_sf(data = policies_hit, color = "blue", size = 2) +  # Affected policies
  labs(title = "Simulated Windstorm and Affected Policies",
       subtitle = "Affected policies shown in blue",
       color = "Legend") +
  theme_minimal()

storm_map

# ---



# ---- STANDARD FORMULA WORKINGS

buffered_circles <- st_buffer(sample_data_sf, dist = units::set_units(5, "km"))
sample_data_sf2 <- sample_data_sf %>%
  mutate(
    circle_prem = sapply(seq_len(nrow(buffered_circles)), function(i) {
      sum(sample_data_sf$premium[st_within(sample_data_sf, buffered_circles[i,], sparse = FALSE)], na.rm = TRUE)
    }),
    circle_si = sapply(seq_len(nrow(buffered_circles)), function(i) {
      sum(sample_data_sf$sum_insured[st_within(sample_data_sf, buffered_circles[i,], sparse = FALSE)], na.rm = TRUE)
    }))

max_index <- which.max(sample_data_sf2$circle_si)
circle_full <- st_buffer(st_sfc(st_point(st_coordinates(sample_data_sf[max_index,])), crs = 4326), dist = units::set_units(5, "km"))
circle_policies <- sample_data_sf %>% filter(st_within(geometry, circle_full, sparse = FALSE))

circle_map <- map +
  geom_sf(data = circle_full, fill = "red", alpha = 0.3) +  
  geom_sf(data = circle_policies, color = "blue", size = 2)


circle_map




































# -------------------------

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


# ---


circle_prem <- c()
circle_si <- c()

for(i in 1:nrow(sample_data_sf)){
  coords <- st_coordinates(sample_data_sf[i,])
  circle_center <- st_sfc(st_point(coords), crs = 4326)
  circle_full <- st_buffer(circle_center, dist = units::set_units(5, "km"))
  circle_policies <- sample_data_sf %>% filter(st_within(geometry, circle_full, sparse = FALSE))
  
  circle_prem[i] <- sum(circle_policies$premium)
  circle_si[i] <- sum(circle_policies$sum_insured)
}

sample_data_sf2 <- sample_data_sf %>%
  mutate(circle_prem = circle_prem,
         circle_si = circle_si)

circle_map <- map +
  geom_sf(data = circle_full, fill = "red", alpha = 0.3) +  
  geom_sf(data = circle_policies, color = "blue", size = 2)


circle_map
# ---
