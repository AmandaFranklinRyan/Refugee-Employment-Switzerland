library(tidyverse)
library(rio)

# Data downloaded from:https://www.bfs.admin.ch/asset/en/ts-x-40.02.03.02.03 
# Taken from Structural Survey
# Population taken from 15-64 years
# Import Data

swiss_employment <- rio::import("C:\\Users\\amand\\Downloads\\ts-x-40.02.03.02.03.csv")
canton_links <- rio::import("Canton Links.xlsx")
boundaries_json <- rjson::fromJSON(file="Geographic Data/Switzerland boundaries.geojson")

unemployment_data <- swiss_employment %>% 
  filter(ERWL==1 & UNIT_MEA=="pers in %") %>% 
  select(TIME_PERIOD,GEO,OBS_VALUE,OBS_STATUS)

unemployment_canton_df <- left_join(unemployment_data, canton_links, by=c("GEO"="CODE")) %>% 
  filter(LABEL_EN!="Switzerland")

#Plot data on choropleth map

g <- list(
  fitbounds = "locations",
  visible = FALSE)

swiss_map <- plot_ly(hoverinfo = "text",
                     text = ~paste("Canton:", unemployment_canton_df$LABEL_EN, "<br>",
                                   "Employed:", unemployment_canton_df$OBS_VALUE,"%", "<br>")) %>% 
  add_trace(
    type="choropleth",
    geojson=boundaries_json,
    locations=unemployment_canton_df$LABEL_EN,
    z=unemployment_canton_df$OBS_VALUE,
    frame=~unemployment_canton_df$TIME_PERIOD,
    colorscale="Blues",
    reversescale =T,
    zmin=0,
    zmax=15,
    featureidkey="properties.name") %>% 
  layout(geo = g) %>%  
  colorbar(thickess=20,
           #orientation="h",
           len=0.5) %>% 
  layout(title = "Unemployment in Switzerland")

# 3. Create Subplots for each nationality ---------------------------------

  