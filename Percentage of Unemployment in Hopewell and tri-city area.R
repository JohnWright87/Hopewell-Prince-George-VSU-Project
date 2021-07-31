# Load the tidycensus package into your R session

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
# Define your Census API key and set it with census_api_key()
census_api_key("2b01f33110fb262986d0d9e502503154b6fccdff")

# Check your API key
Sys.getenv("CENSUS_API_KEY")

############## UEMPLOYMENT PERCENTAGE##########################

# Get employment data for Hopewell and surrounding counties

county_unemploment <- get_acs(geography = "tract", state ="VA", county = c(053, 149, 570, 670, 730),
                              variables = c("B23025_001",
                                            "B23025_002",
                                            "B23025_003",
                                            "B23025_005"), 
                              cache_table = TRUE)

head(county_unemploment)

# Calculate Hopewell unemployment percentages
pct_ue_county <- county_unemploment %>%
  separate(NAME, c("TRACT","COUNTY","STATE"), ",") %>%
  mutate(TRACT = str_replace(TRACT, "Census Tract ", "")) %>%
  group_by(COUNTY, TRACT) %>%
  summarize(pct_ue = B23025_005E/B23025_003E)
pct_ue_county

view(pct_ue_county)
county_ue_means <- pct_ue_county %>%
  group_by(COUNTY) %>%
  summarize(pct_ue_mean = mean(pct_ue, na.rm = TRUE),
            pct_ue_se = sd(pct_ue, na.rm = TRUE)/sqrt(n()))
county_ue_means

# Plot the unemployment data for Hopewell census tracts
ggplot(pct_ue_county, aes(COUNTY, pct_ue, fill=COUNTY)) +
  geom_col()

ggplot(county_ue_means, aes(pct_ue_mean, reorder(COUNTY, pct_ue_mean))) +
  geom_errorbarh(aes(xmin = pct_ue_mean - pct_ue_se, xmax = pct_ue_mean + pct_ue_se)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 14) + 
  labs(title = "Percentage of Unempolyment", 
       subtitle = "Hopewell & Surrounding Counties", 
       x = "Percent Unemployment (bars represent standard error)", 
       y = "")
         