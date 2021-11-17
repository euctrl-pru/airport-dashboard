
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
source(here("R", "Factsheet", "Scripts", "Install_phantomjs.R"))

APT_DF <- read_csv2(here("data","APT_DSHBD_AIRPORT.csv"))

apt <- APT_DF %>%
  dplyr::select(
    icao    = ICAO_CODE,
    iata    = IATA_CODE,
    lon     = LON,
    lat     = LAT,
    Country = CTRY_NAME) %>% 
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
  
  mapshot(m, file = here("R", "Factsheet", "Figures", "Airport_maps", paste0("APT_map_", Airport, ".png")))
  
}


