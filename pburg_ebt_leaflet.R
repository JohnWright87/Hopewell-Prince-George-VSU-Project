library(leaflet)
library(leaflet.extras)

pburg_ebt_stores <- read_csv("pburg_ebt_stores.csv")
pburg_ebt_stores %>%
  leaflet(options=leafletOptions(dragging=T, minZoom=10, maxZoom=20)) %>%
  addProviderTiles("Esri") %>%
  setMaxBounds(lng1=-77.49,lng2=-77.28, lat1=37.13, lat2=37.29)%>%
  addCircleMarkers(radius = 1, label = ~Store_Name) %>%
  addResetMapButton()

