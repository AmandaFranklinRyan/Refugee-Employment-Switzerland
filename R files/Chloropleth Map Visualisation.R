library(sf)
library(tidyverse)
library(plotly)
library(rio)
library(rjson)

#Swiss map data downloaded from:https://cartographyvectors.com/map/1522-switzerland-with-regions

#Import Data
employment_data <- rio::import("Processed Data/Nationality and Employment 2022.csv")
cantons_abbreviations <- rio::import("Geographic Data/Cantons and Abbreviations.xlsx") #Matches cantons and abbreviations
#boundaries_df <- sf::st_read("Geographic Data/Switzerland boundaries.geojson")
boundaries_json <- rjson::fromJSON(file="Geographic Data/Switzerland boundaries.geojson")

#Join geographic and employment Data
all_geo_data <- left_join(boundaries_df,cantons_abbreviations,by=c("name"="Canton")) #Join cantons and abbreviations
geo_employment <- left_join(employment_data,all_geo_data,by=c("Canton"="Abbreviation")) #Merge geographic and Employment Data

#Plot employment percentage of all refugees in 2022
employment_all_refugees_df <- geo_employment %>% 
  select(Country,Employed_percent,name,geometry) %>% 
  group_by(name,geometry) %>% 
  mutate(Employed_percent=ifelse(Employed_percent=="-",NA,Employed_percent)) %>% 
  mutate(Employed_percent=as.numeric(Employed_percent)) %>%
  summarise(Employment_average=mean(Employed_percent, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(Employment_average=round(Employment_average*100,2))

#Create choropleth map from geojson


g <- list(
  fitbounds = "locations",
  visible = FALSE)

swiss_map <- plot_ly(hoverinfo = "text",
                     text = ~paste("Canton:", employment_all_refugees_df$name, "<br>",
                                   "Employed:", employment_all_refugees_df$Employment_average,"%", "<br>")) %>%  
  add_trace(
    type="choropleth",
    geojson=boundaries_json,
    locations=employment_all_refugees_df$name,
    z=employment_all_refugees_df$Employment_average,
    colorscale="Blues",
    reversescale =T,
    zmin=20,
    zmax=70,
    featureidkey="properties.name") %>% 
  layout(geo = g) %>%  
  colorbar(thickess=20,
           orientation="h",
           len=0.5) %>% 
  layout(title = "2022 Refugee Employment in Switzerland")


#For mapbox plots
#mapbox_api_key <- rstudioapi::askForPassword()


