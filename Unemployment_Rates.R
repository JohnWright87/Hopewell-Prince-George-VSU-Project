
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

#### Average Unemployment Rates

Unemployment_Rates <- data.frame(read_csv("C:/Users/John Wright/Desktop/Unemployment_Rates.csv"))
Unemployment_Rates <- Unemployment_Rates %>% pivot_longer(cols = c("X2015", "X2016", "X2017", "X2018", "X2019", "X2020"), names_to = "Year", values_to = "Percentage")
Unemployment_Rates$Year <- str_replace(Unemployment_Rates$Year, "X", "")

ggplot(Unemployment_Rates, aes(x = Year, y = Percentage, fill = Name)) +
  geom_col(position = "dodge") +
  ylab("Percentage") +
  ggtitle("Average Unemployment Rates") +
  arrange()

ggplot(Unemployment_Rates, aes(x = Name, y = Percentage, fill = Year)) +
  geom_col(position = "dodge") +
  ylab("Percentage") +
  xlab("Counties")
  ggtitle("Average Unemployment Rate by city 2015-2020") 

view(Unemployment_Rates)
head(Unemployment_Rates)

############# Unemployment Rate by years

ggplot(Unemployment_Rates, aes(x = Year, y = Percentage, group = Name, colour = Name)) +
  geom_line()+
  ggtitle("Average Unemployment Rates Progression 2015-2020") +
  ylab("Pecentage")

####################################################################################

Completing_College <- data.frame(read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Completing College.csv"))
Completing_College <- Completing_College %>% pivot_longer(cols = c("X1970", "X1980", "X1990", "X2000", "X2015.2019"), names_to = "Year", values_to = "Percentage")
Completing_College$Year <- str_replace(Completing_College$Year, "X", "")

ggplot(Completing_College, aes(x = Year, y = Percentage, fill = County)) +
  geom_col(position = "dodge") +
  ylab("Percentage") +
  ggtitle("Completing College")

head(Completing_College)
View(Completing_College)

####################################

##### Completing Some College

Completing_Some_College <- data.frame(read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Completing Some College.csv"))
Completing_Some_College <- Completing_Some_College %>% pivot_longer(cols = c("X1970", "X1980", "X1990", "X2000", "X2015.2019"), names_to = "Year", values_to = "Percentage")
Completing_Some_College$Year <- str_replace(Completing_Some_College$Year, "X", "")

ggplot(Completing_Some_College, aes(x = Year, y = Percentage, fill = County)) +
  geom_col(position = "dodge") +
  ylab("Percentage") +
  ggtitle("Completing Some College")

####################################

###### Completing High School

Completing_High_School <- data.frame(read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Completing High School.csv"))
Completing_High_School <- Completing_High_School %>% pivot_longer(cols = c("X1970", "X1980", "X1990", "X2000", "X2015.2019"), names_to = "Year", values_to = "Percentage")
Completing_High_School$Year <- str_replace(Completing_High_School$Year, "X", "")

ggplot(Completing_High_School, aes(x = Year, y = Percentage, fill = County)) +
  geom_col(position = "dodge") +
  ylab("Percentage") +
  ggtitle("Completing High School")


##############################################

######## Not Completing High School

Not_Completing_High_School <- data.frame(read_csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Not Completing High School.csv"))
Not_Completing_High_School <- Not_Completing_High_School %>% pivot_longer(cols = c("X1970", "X1980", "X1990", "X2000", "X2015.2019"), names_to = "Year", values_to = "Percentage")
Not_Completing_High_School$Year <- str_replace(Not_Completing_High_School$Year, "X", "")

ggplot(Not_Completing_High_School, aes(x = Year, y = Percentage, fill = County)) +
  geom_col(position = "dodge") +
  ylab("Percentage") +
  ggtitle("Not Completing High School")


