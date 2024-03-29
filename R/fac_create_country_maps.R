
library(leaflet)
library(leaflet.extras)
library(readr)
library(here)
library(dplyr)
library(stringr)
library(htmltools)
library(mapview)
library(gt)
library(geonames)
library(countrycode)

options(geonamesUsername = "spatc")
Sys.setenv("APPDATA"=paste0("C:\\Users\\", Sys.getenv("USERNAME"), "\\dev"))

# Phantom JS is needed to save the map figure with the mapshot command
# only needed the first time you run this script on a new machine
# webshot::install_phantomjs(force=TRUE)

apt <- APT_DF %>%
  dplyr::select(
    icao    = ICAO,
    iata    = IATA,
    lon     = LON,
    lat     = LAT,
    Country = STATE) %>% 
  mutate(Country_code=countrycode(Country, origin = 'country.name', destination = 'iso2c', 
                                  custom_match = c("SERBIA AND MONTENEGRO" = "CS")))

for (Airport in apt$icao) {
  
  APT_data=filter(apt, icao==Airport)
  if (APT_data$Country_code=="CS") {
    bounds <- GNcountryInfo(country = "ME") %>% 
      select(south, north, east, west) %>% 
      rbind(GNcountryInfo(country = "RS") %>% 
      select(south, north, east, west)) %>% 
      mutate(south=min(south), north=max(north), east=max(east), west=min(west)) %>% 
      unique
  } else {
    bounds <- GNcountryInfo(country = APT_data$Country_code) %>% 
      select(south, north, east, west)
  }
  m=APT_data %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(lng = ~lon, lat = ~lat) %>% 
    fitBounds(lng1 = bounds$west, lat1 = bounds$north, lng2 = bounds$east, lat2 = bounds$south)
  
  mapshot(m, file = here("media", "airport_maps", paste0("APT_map_", Airport, ".png")))
  
}


