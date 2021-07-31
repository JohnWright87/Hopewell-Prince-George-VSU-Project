

library(leaflet)
library(sp)
library(tidyverse)
library(shiny)
library(tidycensus)
library(tigris)


library(leaflet.extras)

pburg_ebt_stores <- read_csv("pburg_ebt_stores.csv")
pburg_ebt_stores %>% 
  
  leaflet(options=leafletOptions(dragging=T, minZoom=10, maxZoom=20)) %>%
  addProviderTiles("Esri") %>%
  setMaxBounds(lng1=-77.49,lng2=-77.28, lat1=37.13, lat2=37.29)%>%
  addCircleMarkers(radius = 1, label = ~Store_Name) %>%
  addResetMapButton()


leaflet(boundary) %>%
  addTiles() %>%
  addPolygons()

# finding the unique values for each field
industries <- unique(pburg_ebt_stores$NaicsTitle1)
counties <- unique(pburg_ebt_stores$State)
zipcodes <- unique(pburg_ebt_stores$Zip5)

# map with industry groups

  addCircles(group = industries) %>%
  addLayersControl(
    overlayGroups = industries)


  addPolygons(data = boundary, color = c("red","blue","green","orange","violet")) %>%
  addCircles(group = industries) %>%
  addLayersControl(
    overlayGroups = industries)
