
################## All intended libraries for this plan #################
######################################################################################
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sp)
library(rgdal)
library(tigris)
library(raster) 
library(maptools)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(tmap)
library(tmaptools)
library(maps)
library(leaflet)
library(viridis)
library(mapview)
library(RColorBrewer)
library(stringr)
library(osmdata)
library(osrm)
library(sf)
library(ggpubr)
library(ggmap)
library(gridExtra)
library(xlsx)
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

census_api_key("2b01f33110fb262986d0d9e502503154b6fccdff")

# Interactive map of the tri-city counties population

va_pop <- get_acs(geography = "county", county = c(053, 149, 570, 670, 730),
                  variables = "B01003_001",
                  state = "VA",
                  cache = TRUE,
                  geometry = TRUE)

mapview(va_pop, zcol = "estimate")

view(va_pop)

##################################################################


population <- get_acs(geography = "county",county = c(053, 149, 570, 670, 730),
                      variables = "B01003_001",
                      geometry = TRUE,
                      shift_geo = TRUE)

mapviewOptions(legend.pos = "bottomright")
mapviewOptions(leafletWidth = 800)
mapview(population, zcol = "estimate", native.crs = TRUE, crs = 5070)

view(population)

###################################


################## All intended libraries for this plan #################
######################################################################################
library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sp)
library(rgdal)
library(tigris)
library(raster) 
library(maptools)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(tmap)
library(tmaptools)
library(maps)
library(leaflet)
library(viridis)
library(mapview)
library(RColorBrewer)
library(stringr)
library(osmdata)
library(osrm)
library(sf)
library(ggpubr)
library(ggmap)
library(gridExtra)
library(xlsx)
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

census_api_key("2b01f33110fb262986d0d9e502503154b6fccdff")

# Interactive map of the tri-city counties population

va_jobs <- get_acs(geography = "county", county = c(053, 149, 570, 670, 730),
                  variables = "B01003_001",
                  state = "VA",
                  cache = TRUE,
                  geometry = TRUE)

mapview(va_jobs, zcol = "estimate")

view(va_jobs)



##############################

library(leaflet)
library(sp)

Jobs <- read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Crater Area Job Map Location.csv")
Jobs <- Crater_Area_Job_Map_Location[complete.cases(Crater_Area_Job_Map_Location ),]

#Crater_Area_Job_Map_Location$Latitude <- as.numeric(Crater.Area.Job.Map.Location$Latitude)
#Crater_Area_Job_Map_Location$Longitude <- as.numeric(Crater.Area.Job.Map.Location$Longitude)

#Crater_Area_Job_Map_Location.sp <- SpatialPointsDataFrame(Crater_Area_Job_Map_Location[,-c(9,10)], Crater_Area_Job_Map_Location[,-c(9,10)])

MAP <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(lat = 37.30872, lng  = -77.29399, popup = "Boathouse") %>%
  addMarkers(lat = 37.23191, lng  = -77.40595, popup = "Alexander's") %>%
  addMarkers(lat = 37.22760, lng  = -77.40354, popup = "Andrew's Grill")%>%

  setView(lat = 37.30872, lng = -77.29399, zoom = 11) %>%
    addMiniMap(toggleDisplay = TRUE,
      tiles = providers$Stamen.TonerLite)

MAP

View(Crater_Area_Job_Map_Location )
head(Crater_Area_Job_Map_Location )


#####################

# Load the leaflet library
library(leaflet)

# Create a leaflet map with default map tile using addTiles()
leaflet(option = leafletOptions(dragging = FALSE, minZoom = 14, maxZoom = 18)) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.PositronNoLabels")
 
  
  #fitBounds(lng1 = -73.910, lat1 =40.773, lng2 = -74.060, lat2 = 40.723)

 
 
  
  
  
  library(leaflet)
  library(sp)
  library(tidyverse)
  
  Jobs <- read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Crater Area Job Map Location.csv")
  
  Jobs %>%
    #filter(NaicsTitle1 == "Accommodation and Food Services") %>%
    leaflet() %>%
    addTiles() %>%
    addCircles() %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
    addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  
    setView(lat = 37.30872, lng = -77.29399, zoom = 10) %>% 
  addMiniMap(toggleDisplay = TRUE,
             tiles = providers$Stamen.TonerLite)
  
  head(Jobs)
  
  
  ########
  
  
  # My suggested code attempts
  library(leaflet)
  library(sp)
  library(tidyverse)
  library(shiny)
  
  # read in data file
  Jobs <- read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Crater Area Job Map Location.csv")
  
  # base leaflet map
  leaflet(Jobs) %>%
    addTiles() %>%
    addCircles()
  
  # finding the unique values for each field
  industries <- unique(Jobs$NaicsTitle1)
  counties <- unique(Jobs$County)
  zipcodes <- unique(Jobs$Zipcode)
  
  # map with industry groups
  Jobs %>%
    leaflet() %>%
    addTiles() %>%
    addCircles(group = industries) %>%
    addLayersControl(
      overlayGroups = industries)
  


  # My suggested code attempts
  library(leaflet)
  library(sp)
  library(tidyverse)
  library(shiny)
  library(tidycensus)
  library(tigris)
  
  # read in data file
  pburg_ebt_stores <- read_csv("pburg_ebt_stores.csv")
  
  county_fp <- c("053", "149", "570", "670", "730")
  
  boundary <- counties("VA", cb = TRUE) %>%
    filter(COUNTYFP %in% county_fp)
  
  library(leaflet)
  library(leaflet.extras)
  
 
  pburg_ebt_stores %>%
    leaflet(options=leafletOptions(dragging=T, minZoom=10, maxZoom=20)) %>%
    addProviderTiles("Esri") %>%
    setMaxBounds(lng1=-77.49,lng2=-77.28, lat1=37.13, lat2=37.29)%>%
    addCircleMarkers(radius = 1, label = ~Store_Name) %>%
    addResetMapButton()
    addTiles() %>%
    addCircles() %>%
  
  leaflet(boundary) %>%
    addTiles() %>%
    addPolygons()
  
  
  # map with industry groups
  Jobs %>%
    leaflet() %>%
    addTiles() %>%
    addCircles(group = industries) %>%
    addLayersControl(
      overlayGroups = industries)
  
  # map with industry groups and counties colored
  Jobs %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(data = boundary, color = c("red","blue","green","orange","violet")) %>%
    addCircles(group = industries) %>%
    addLayersControl(
      overlayGroups = industries)
  
  
  
  ###########
  # My suggested code attempts
  library(leaflet)
  library(sp)
  library(tidyverse)
  library(shiny)
  library(tidycensus)
  library(tigris)
  library(colorspace)
  
  # Define your Census API key and set it with census_api_key()
  api_key <- "xxx"
  census_api_key(api_key)
  
  # Set tigris envirnment codes
  options(tigris_use_cache=T)
  options(tigris_class="sf")
  
  # read in data file
  Jobs <- read_csv("Crater Area Job Map Location.csv")
  
  county_fp <- c("053", "149", "570", "670", "730")
  
  boundary <- counties("VA", cb = TRUE) %>%
    filter(COUNTYFP %in% county_fp)
  
  # base leaflet maps
  leaflet(Jobs) %>%
    addTiles() %>%
    addCircles()
  
  leaflet(boundary) %>%
    addTiles() %>%
    addPolygons()
  
  # finding the unique values for each field
  industries <- unique(Jobs$NaicsTitle1)
  
  # color palettes from colorspace
  county_colors  <- qualitative_hcl(5, palette = "Set 2")
  industry_colors <- sequential_hcl(18, palette = "Viridis")
  
  # map with industry groups
  Jobs %>%
    leaflet() %>%
    addTiles() %>%
    addCircles(group = industries) %>%
    addLayersControl(
      overlayGroups = industries)
  
  # map with industry groups and counties colored
  Jobs %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(data = boundary, color = county_colors) %>%
    addCircles(group = industries, color = industry_colors) %>%
    addLayersControl(
      overlayGroups = industries)