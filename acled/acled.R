library(readr)
library(tidyverse)
library(sf)
library(leaflet)

acled <- read_csv("acled/2021-01-01-2021-06-23-Western_Africa.csv")

glimpse(acled)

acled_fat <- acled %>% filter(fatalities > 0 )

leaflet(acled_fat) %>% addTiles() %>% addCircleMarkers( lng = acled_fat$longitude, lat = acled_fat$latitude,
                                              color = "red",   fillOpacity = 0, radius = sqrt(acled_fat$fatalities))
