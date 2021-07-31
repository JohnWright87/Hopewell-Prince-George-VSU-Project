
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

industry_varspt1 <- paste0("C24050_00", 1:9)
industry_varspt2 <- paste0("C24050_0", 10:14)
industry_vars <- append(industry_varspt1, industry_varspt2)

industry_vars <- c("Agriculture" = "C24050_002",
                   "Construction" = "C24050_003",
                   "Manufacturing" = "C24050_004",
                   "Wholesale" = "C24050_005",
                   "Retail trade" = "C24050_006",
                   "Transportation" = "C24050_007",
                   "Information"  = "C24050_008",
                   "Finance" = "C24050_009",
                   "Professional" = "C24050_010",
                   "Education" = "C24050_011",
                   "Entertainment" = "C24050_012",
                   "Other services" = "C24050_013",
                   "Public administration" = "C24050_014")

# Get ACS data - Median Household Income
#     concept = HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 201
industry2019 <- get_acs(geography = "county",
                        county = c(053, 149, 570, 670, 730),
                        variables = c(industry_vars),
                        state = "VA",
                        year = 2019,
                        cache = TRUE,
                        geometry = TRUE)

industry2018 <- get_acs(geography = "county",
                        county = c(053, 149, 570, 670, 730),
                        variables = c(industry_vars),
                        state = "VA",
                        year = 2018,
                        cache = TRUE,
                        geometry = TRUE)

industry2017 <- get_acs(geography = "county",
                        county = c(053, 149, 570, 670, 730),
                        variables = c(industry_vars),
                        state = "VA",
                        year = 2017,
                        cache = TRUE,
                        geometry = TRUE)

industry2016 <- get_acs(geography = "county",
                        county = c(053, 149, 570, 670, 730),
                        variables = c(industry_vars),
                        state = "VA",
                        year = 2016,
                        cache = TRUE,
                        geometry = TRUE)

industry2015 <- get_acs(geography = "county",
                        county = c(053, 149, 570, 670, 730),
                        variables = c(industry_vars),
                        state = "VA",
                        year = 2015,
                        cache = TRUE,
                        geometry = TRUE)

industry2014 <- get_acs(geography = "county",
                        county = c(053, 149, 570, 670, 730),
                        variables = c(industry_vars),
                        state = "VA",
                        year = 2014,
                        cache = TRUE,
                        geometry = TRUE)


industry_all_years <- get_acs(geography = "county",
                              county = c(053, 149, 570, 670, 730),
                              variables = c(industry_vars),
                              state = "VA",
                              cache = TRUE,
                              geometry = TRUE)

industry2014 <- mutate(industry2014, year = "2014")
industry2015 <- mutate(industry2015, year = "2015")
industry2016 <- mutate(industry2016, year = "2016")
industry2017 <- mutate(industry2017, year = "2017")
industry2018 <- mutate(industry2018, year = "2018")
industry2019 <- mutate(industry2019, year = "2019")


industry_2014_2019 <- industry2014 %>% 
  rbind(industry2015) %>% 
  rbind(industry2016) %>% 
  rbind(industry2017) %>% 
  rbind(industry2018) %>% 
  rbind(industry2019)

industry_2014_2019 <- mutate(industry_2014_2019, NAME = str_replace_all(industry_2014_2019$NAME, "Virginia", "")) %>% 
  mutate(industry_2014_2019, NAME = str_replace_all(industry_2014_2019$NAME, TRACT, "Census Tract", ""))

ggplot(industry_2014_2019, aes(x = year, y = estimate, fill = variable)) +
  geom_col(position ="dodge") +
  facet_wrap(~NAME)


head(industry_2014_2019)

view(industry_2014_2019)
