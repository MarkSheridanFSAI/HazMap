ireland_shapefile <- function(){
  
  
  roi <- ne_states(country = "Ireland", returnclass = "sf")
  uk <- ne_states(country = "United Kingdom", returnclass = "sf")
  northern_ireland <- uk[uk$region == "Northern Ireland", ]
  all_ireland <- rbind(roi, northern_ireland)
  
  mapping_table <- data.frame(
    region = c(
      "Donegal", "Leitrim", "Cavan", "Monaghan", "Louth",
      "Dublin", "Dún Laoghaire–Rathdown", "Wicklow", "Wexford",
      "Kilkenny", "Waterford", "Cork", "Cork", "Kerry",
      "Limerick", "Clare", "Galway", "Galway", "Mayo", "Sligo",
      "Meath", "Fingal", "Kildare", "Carlow", "Laoighis",
      "South Dublin", "Offaly", "Westmeath", "Longford", "Roscommon",
      "North Tipperary", "South Tipperary", "Waterford", "Limerick",
      "Derry", "Strabane", "Fermanagh", "Dungannon", "Armagh",
      "Newry and Mourne", "Limavady", "Coleraine", "Moyle", "Larne",
      "Carrickfergus", "Newtownabbey", "Belfast", "North Down", "Ards",
      "Down", "Magherafelt", "Omagh", "Mid Ulster", "Craigavon",
      "Banbridge", "Antrim", "Lisburn", "Ballymoney", "Ballymena",
      "Castlereagh"
    ),
    county = c(
      "Donegal", "Leitrim", "Cavan", "Monaghan", "Louth",
      "Dublin", "Dublin", "Wicklow", "Wexford",
      "Kilkenny", "Waterford", "Cork", "Cork", "Kerry",
      "Limerick", "Clare", "Galway", "Galway", "Mayo", "Sligo",
      "Meath", "Dublin", "Kildare", "Carlow", "Laois",
      "Dublin", "Offaly", "Westmeath", "Longford", "Roscommon",
      "Tipperary", "Tipperary", "Waterford", "Limerick",
      "Derry", "Derry", "Fermanagh", "Tyrone", "Armagh",
      "Armagh", "Derry", "Derry", "Antrim", "Antrim",
      "Antrim", "Antrim", "Down", "Down", "Down",
      "Down", "Tyrone", "Tyrone", "Tyrone", "Armagh",
      "Down", "Antrim", "Antrim", "Antrim", "Antrim",
      "Antrim"
    ))
  
  all_ireland$county <- mapping_table$county[match(all_ireland$name, mapping_table$region)]
  
  ireland_32_counties <- all_ireland %>%
    group_by(county) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_as_sf()
  
  return(ireland_32_counties)
}