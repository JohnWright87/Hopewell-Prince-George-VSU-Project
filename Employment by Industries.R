
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(tmap)
library(tmaptools)
library(maps)
library(leaflet)
library(viridis)
library(RColorBrewer)
library(stringr)

census_api_key("2b01f33110fb262986d0d9e502503154b6fccdff")


Info <- get_acs(geography = "county", 
                county = c(053, 149, 570, 670, 730),
                variables = c(medincome = "B19326_001"), 
                state = "51", 
                year = 2019,
                cache = TRUE)
Info %>% head()


get_county <- get_acs(geography = "county", state = 51,
                      variables = c(medincome = "B19326_001"),
                      year = 2018)
get_county %>% head()



##########################3
  
Info <- get_acs(geography = "county", 
                  county = c(053, 149, 570, 670, 730),
                  table = "B15002", 
                  state = "51", 
                  year = 2019,
                  cache = TRUE)
Info %>% head()
view(Info)

#################
  
industry_varspt1 <- paste0("C24050_00", 2:9)
industry_varspt2 <- paste0("C24050_0", 10:14)
industry_vars <- append(industry_varspt1, industry_varspt2)


counties_data <- get_acs(geography = "tract", state="51", county = (c("730", "149", "053", "670", "570")),
                         variables = (c(county_vars)))
                         
###############################################


industry_varspt1 <- paste0("C24050_00", 1:9)
industry_varspt2 <- paste0("C24050_0", 10:14)
industry_vars <- append(industry_varspt1, industry_varspt2)

# Get ACS data - Median Household Income
#     concept = HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 201
industry <- get_acs(geography = "tract",
                    county = c(053, 149, 570, 670, 730),
                    variables = c(industry_vars),
                    state = "VA",
                    year = 2019,
                    cache = TRUE,
                    geometry = TRUE)

# Convert the collected ACS data into a wide format without the margin of error (MOE) column
industry_wide <- industry %>%
  select(-moe) %>%
  spread(variable, estimate) %>%
  separate(NAME, c("TRACT","COUNTY","STATE"), ",") %>%
  mutate(TRACT = str_replace(TRACT, "Census Tract ", ""))

head(industry_wide)
view(industry_wide)

filter(industry_wide, str_detect(TRACT, "C24050_001"))

