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
library(dplyr)
library(ggpubr)
library(forcats)
library(ggmap)
library(gridExtra)
library(xlsx)
devtools::install_github("rCarto/osrm")
options(tigris_use_cache = TRUE)
options(tigris_class="sf")


#053 is the FIPS code for Dinwiddie
#149 is Prince George County
#570 is Colonial heights
#670 is Hopewell
#730 is petersburg

census_api_key("2b01f33110fb262986d0d9e502503154b6fccdff")


######## Travel time to work (Transportation)

get_county <- function(varcode, summary_var){
  data.frame(get_acs(geography = "county",
                     state = 51,
                     county = (c(730, 149, 053, 670, 570)),
                     variables = varcode,
                     summary_var = summary_var,
                     cache = TRUE)) %>%
    mutate(percent = (estimate/summary_est)*100) }

traveltime_vars <- c(
  travel_less_5_min =  "B08303_002",
  travel_5_9_min ="B08303_003",
  travel_10_14_min = "B08303_004",
  travel_15_19_min = "B08303_005",
  travel_20_24_min = "B08303_006",
  travel_25_29_min = "B08303_007",
  travel_30_34_min = "B08303_008",
  travel_35_39_min = "B08303_009",
  travel_40_44_min = "B08303_010",
  travel_45_59_min = "B08303_011",
  travel_60_89_min = "B08303_012",
  travel_90_plus_min = "B08303_013")

travel_all <- c(travel_all = "B08303_001")

traveltime <- get_county(traveltime_vars, travel_all)
traveltime <- mutate(traveltime, NAME = str_replace_all(traveltime$NAME, ", Virginia", ""))

ggplot(traveltime, aes(x = variable, y = estimate, fill = variable)) +
  geom_col(position = "dodge") +
  facet_wrap(~NAME) +
  coord_flip() +
  labs(title = "Travel Time to work in Hopewell, Prince George and Surrounding Crater Area by County",
       x = "Travel Time",
       y = "Estimated amount of People") +
  scale_fill_discrete(labels = c("Less than 5 minutes","5 to 9 minutes","10 to 14 minutes","15 to 19 minutes","20 to 24 minutes",
                                 "25 to 29 minutes","30 to 34 minutes", "35 to 39 minutes", "40 to 44 minutes","45 to 59 minutes",
                                 "60 to 89 minutes", "90 or more minutes"))

head(traveltime)
View(traveltime)
# traveltime_vars %>% arrange(desc(travel_all))


###########################################################

# Means of Travel

get_county <- function(varcode, summary_var){
  data.frame(get_acs(geography = "county",
                     state = 51,
                     county = (c(730, 149, 053, 670, 570)),
                     variables = varcode,
                     summary_var = summary_var,
                     cache = TRUE)) %>%
    mutate(percent = (estimate/summary_est)*100) }

traveltime_vars <- c(
  travelmeans_alone = "B08101_009",
  travelmeans_carpool = "B08101_017",
  travelmeans_public_taxi = "B08101_025",
  travelmeans_walked = "B08101_033",
  travelmeans_other = "B08101_041",
  travelmeans_at_home = "B08101_049")

travelmeans <- c(travelmeans_all = "B08101_001")

traveltime <- get_county(traveltime_vars, travel_all)
traveltime <- mutate(traveltime, NAME = str_replace_all(traveltime$NAME, ", Virginia", ""))

ggplot(traveltime, aes(x = variable, y = estimate, fill = variable)) +
  geom_col(position = "dodge") +
  facet_wrap(~NAME) +
  coord_flip() +
  labs(title = "Means Of Travel to work in Hopewell, Prince George and Surrounding Crater Area by County",
       x = "Types of Transportation",
       y = "Estimated amount of People") +
  scale_fill_discrete(labels = c("Drive to Work","Work from Home", "Carpool", "Other means of Transportation","Public Taxi", "Walk to Work"))


################################################################
get_county <- function(varcode, summary_var){
  data.frame(get_acs(geography = "county",
                     state = 51,
                     county = (c(730, 149, 053, 670, 570)),
                     variables = varcode,
                     summary_var = summary_var,
                     cache = TRUE)) %>%
    mutate(percent = (estimate/summary_est)*100) }

education_vars <- c(
  No_High_School_diploma = "B06009_002",
  High_School_diploma = "B06009_003",
  Associate_degree= "B06009_004",
  Bachelor_degree = "B06009_005",
  Masters_degree = "B06009_006")

pop_edu_total <- c(pop_edu_total = "B06009_001")

education <- get_county(education_vars, pop_edu_total)
education <- mutate(education, NAME = str_replace_all(education$NAME, ", Virginia", ""))

ggplot(education, aes(x = NAME, y = estimate, fill = variable)) +
  geom_col(position = "dodge") +
  labs(title = "Education Levels in Hopewell, Prince George and Surrounding Crater Area by County",
       x = "Counties",
       y = "Estimated Levels of People") +
  scale_fill_discrete(labels = c("High school diploma or equivalent","Associate's degree", "Bachelor's degree", "Master's degree","Did Not Finish High School"))

head(education)

##########################################################
