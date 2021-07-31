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


#053 is the FIPS code for Dinwiddie
#149 is Prince George County
#570 is Colonial heights
#670 is Hopewell
#730 is petersburg

census_api_key("2b01f33110fb262986d0d9e502503154b6fccdff")

selected_counties <- get_acs(geography = "county",
                             county = c(053, 149, 570, 670, 730),
                             variables = c(hh_income_total_pop ="B19001_001",
                                           med_hh_income_less10k = "B19001_002",
                                           med_hh_income_10_14k = "B19001_003",
                                           med_hh_income_15_19k = "B19001_004",
                                           med_hh_income_20_24k = "B19001_005",
                                           med_hh_income_25_29k = "B19001_006",
                                           med_hh_income_30_34k = "B19001_007",
                                           med_hh_income_35_39k = "B19001_008",
                                           med_hh_income_40_44k = "B19001_009",
                                           med_hh_income_45_49k = "B19001_010",
                                           med_hh_income_50_59k = "B19001_011",
                                           med_hh_income_60_74k = "B19001_012",
                                           med_hh_income_75_99k = "B19001_013",
                                           med_hh_income_100_124k = "B19001_014",
                                           med_hh_income_125_149k = "B19001_015",
                                           med_hh_income_150_199k = "B19001_016",
                                           med_hh_income_200kmore = "B19001_017"),
                             state = "VA",
                             year = 2019,
                             cache = TRUE)


###############################################################
selected_counties %>%
  mutate(NAME = gsub(" County, Virginia", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "orange", size = 1) +
  labs(title = "Household income by county in The Tri-city Area of Virginia",
       subtitle = "2018-2019 Census Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")