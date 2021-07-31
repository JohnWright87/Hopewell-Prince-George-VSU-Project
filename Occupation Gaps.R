

library(tidyverse)
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
library(scales)
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")

census_api_key("2b01f33110fb262986d0d9e502503154b6fccdff")


Hopewell_Occupation_Gaps <- read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Hopewell Occupation Gaps.csv")
  
  # Diverging Barcharts
ggplot(Hopewell_Occupation_Gaps, aes(x= reorder(Occupation, -Gaps), y= Gaps, label= Occupation)) +
  geom_bar(stat='identity') +
  geom_col (fill = "blue")+
  coord_flip() +
ggtitle("Occupation Gaps in Hopewell County") +
  xlab("Occupation")
     
     
head(Hopewell_Occupation_Gaps)
View(Hopewell_Occupation_Gaps)
###################
     
## Crater Area Occupation Gaps

Crater_Area_Occupation_Gaps <- read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Crater Area Occupation Gaps.csv")
     
#  Create Diverging Barcharts
ggplot(Crater_Area_Occupation_Gaps, aes(x= reorder(Occupation, -Gaps), y= Gaps, label= Occupation)) +
  geom_bar(stat='identity') +
  geom_col (fill = "blue")+
  coord_flip() +
ggtitle("Occupation Gaps in the Crater Area") +
  xlab("Occupation")

###########################

# Prince George Occupation Gaps

Prince_George_Occupation_Gaps <- read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Prince George Occupation Gaps.csv")

ggplot(Prince_George_Occupation_Gaps, aes(x= reorder(Occupation, -Gaps), y= Gaps, label= Occupation)) +
  geom_bar(stat='identity') +
  geom_col (fill = "blue")+
  coord_flip() +
  ggtitle("Occupation Gaps in Prince George County") +
  xlab("Occupation")
     

