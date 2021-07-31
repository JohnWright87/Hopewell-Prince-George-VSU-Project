

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




######### Unemployed by occupation

Unemployed.by.Occupation <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployed by Occupation.csv")

Unemployed.by.Occupation_long <- Unemployed.by.Occupation %>%
  gather(key = "County", value = "numUnemployedbyOccupation", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Unemployed.by.Occupation_long %>%
  group_by(Occupation, County) %>%
  ggplot(aes(x = reorder(Occupation, numUnemployedbyOccupation), y = numUnemployedbyOccupation, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Occupation in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Industry",
       y = "Number of Unemployed Industry") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))


####################

##### Percentage of Unemployed by occupation

Unemployed.by.Occupation <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployed by Occupation.csv")

Unemployed.by.Occupation_long <- Unemployed.by.Occupation %>%
  gather(key = "County", value = "numUnemployedbyOccupation", Crater.Area.County:Hopewell.County) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater Region")))

Unemployed.by.Occupation_long %>%
  group_by(Occupation, County) %>%
  ggplot(aes(x = reorder(Occupation, numUnemployedbyOccupation), y = numUnemployedbyOccupation, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Occupation in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Industry",
       y = "Percentage of Unemployed") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

################################################################3


###### Unemployed by Industry

Unemployment.by.Industry <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Industry.csv")

Unemployment.by.Industry_long <- Unemployment.by.Industry %>%
  gather(key = "County", value = "numUnemployedbyIndustry", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Unemployment.by.Industry_long %>%
  group_by(Industry, County) %>%
  ggplot(aes(x = reorder(Industry, numUnemployedbyIndustry), y = numUnemployedbyIndustry, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Industry in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Industry",
       y = "Number of Unemployed by Industry") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))


####################

##### Percentage of Unemployed by Industry

Unemployment.by.Industry <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Industry.csv")

Unemployment.by.Industry_long <- Unemployment.by.Industry %>%
  gather(key = "County", value = "numUnemployedbyIndustry", Crater.Area.County:Hopewell.County) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater Region")))

Unemployment.by.Industry_long %>%
  group_by(Industry, County) %>%
  ggplot(aes(x = reorder(Industry, numUnemployedbyIndustry), y = numUnemployedbyIndustry, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Industry in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Industry",
       y = "Percentage of Unemployed by Industry") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

####################################################

####### Unemployment by Education Attained

Unemployment.by.Education.Attained <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Education Attained.csv")

Unemployment.by.Education.Attained_long <- Unemployment.by.Education.Attained %>%
  gather(key = "County", value = "numUnemployedbyEducationAttained", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Unemployment.by.Education.Attained_long %>%
  group_by(Education.Level, County) %>%
  ggplot(aes(x = reorder(Education.Level, numUnemployedbyEducationAttained), y = numUnemployedbyEducationAttained, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Educaton Level Attained in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Education Level",
       y = "Number of Unemployed by Educaton Level Attained") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))


####################

##### Percentage of Unemployed by Industry

Unemployment.by.Education.Attained <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Education Attained.csv")

Unemployment.by.Education.Attained_long <- Unemployment.by.Education.Attained %>%
  gather(key = "County", value = "numUnemployedbyEducationAttained", Crater.Area.County:Hopewell.County) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater Region")))

Unemployment.by.Education.Attained_long %>%
  group_by(Education.Level, County) %>%
  ggplot(aes(x = reorder(Education.Level, numUnemployedbyEducationAttained), y = numUnemployedbyEducationAttained, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Education Level in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Education Level",
       y = "Percentage of Unemployed by Education Level Attained") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

################################################

##### Unemployment by Age

Unemployment.by.Age <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Age.csv")

Unemployment.by.Age_long <- Unemployment.by.Age %>%
  gather(key = "County", value = "numUnemployedbyAge", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Unemployment.by.Age_long %>%
  group_by(Age, County) %>%
  ggplot(aes(x = reorder(Age, numUnemployedbyAge), y = numUnemployedbyAge, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Age in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Age",
       y = "Number of Unemployed by Age") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))


####################

##### Percentage of Unemployed by Age

Unemployment.by.Age <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Age.csv")

Unemployment.by.Age_long <- Unemployment.by.Age %>%
  gather(key = "County", value = "numUnemployedbyAge", Crater.Area.County:Hopewell.County) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater Region")))

Unemployment.by.Age_long %>%
  group_by(Age, County) %>%
  ggplot(aes(x = reorder(Age, numUnemployedbyAge), y = numUnemployedbyAge, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Age in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Age",
       y = "Percentage of Unemployed by Age") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))


################################################

######### Unemployment by Race

Unemployment.by.Race <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Race.csv")

Unemployment.by.Race_long <- Unemployment.by.Race%>%
  gather(key = "County", value = "numUnemployedbyRace", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Unemployment.by.Race_long %>%
  group_by(Race, County) %>%
  ggplot(aes(x = reorder(Race, numUnemployedbyRace), y = numUnemployedbyRace, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Race in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Race",
       y = "Number of Unemployed by Race") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))


####################

##### Percentage of Unemployed by Race

Unemployment.by.Race <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Race.csv")

Unemployment.by.Race_long <- Unemployment.by.Race %>%
  gather(key = "County", value = "numUnemployedbyRace", Crater.Area.County:Hopewell.County) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater Region")))

Unemployment.by.Race_long %>%
  group_by(Race, County) %>%
  ggplot(aes(x = reorder(Race, numUnemployedbyRace), y = numUnemployedbyRace, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Unemployment by Age in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Race",
       y = "Percentage of Unemployed by Age") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

######################################

######## Unemployment by Gender

Unemployment.by.Gender <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Gender.csv")

Unemployment.by.Gender_long <- Unemployment.by.Gender %>%
  gather(key = "County", value = "numUnemployedbyGender", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Unemployment.by.Gender_long %>%
  group_by(Gender, County) %>%
  ggplot(aes(x = reorder(Gender, numUnemployedbyGender), y = numUnemployedbyGender, fill = County)) +
  geom_col(position = "dodge") +
  labs(title = "Unemployment by Gender in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Gender",
       y = "Number of Unemployed by Gender") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))


####################

##### Percentage of Unemployed by Gender

Unemployment.by.Gender <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Unemployment by Gender.csv")

Unemployment.by.Gender_long <- Unemployment.by.Gender %>%
  gather(key = "County", value = "numUnemployedbyGender", Crater.Area.County:Hopewell.County) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater Region")))

Unemployment.by.Gender_long %>%
  group_by(Gender, County) %>%
  ggplot(aes(x = reorder(Gender, numUnemployedbyGender), y = numUnemployedbyGender, fill = County)) +
  geom_col(position = "dodge") +
  labs(title = "Unemployment by Gender in Hopewell, Prince George and Surrounding Crater Area by Location",
       x = "Gender",
       y = "Percentage of Unemployed by Gender") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))