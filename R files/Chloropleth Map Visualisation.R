library(sf)
library(tidyverse)
library(plotly)
library(rio)
library(geojsonio)

#Swiss map data downloaded from:https://cartographyvectors.com/map/1522-switzerland-with-regions

#Import Data
employment_data <- rio::import("Processed Data/Nationality and Employment 2022.csv")
#swiss_boundaries <- st_read("Geographic Data/swissBOUNDARIES3D_1_4_TLM_KANTONSGEBIET.shp")
#boundaries_json <- geojsonio::geojson_read("Geographic Data/Switzerland boundaries.geojson", what="sp")
cantons_abbreviations <- rio::import("Geographic Data/Cantons and Abbreviations.xlsx")
boundaries_df <- sf::st_read("Geographic Data/Switzerland boundaries.geojson")

#Join geographic and employment Data
all_geo_data <- left_join(boundaries_df,cantons_abbreviations,by=c("name"="Canton"))
geo_employment <- left_join(employment_data,all_geo_data,by=c("Canton"="Abbreviation"))

#Plot employment percentage of all refugees in 2022

employment_all_refugees_df <- geo_employment %>% 
  select(Country,Employed_percent,name,geometry) %>% 
  group_by(name,geometry) %>% 
  mutate(Employed_percent=as.numeric(Employed_percent)) %>% 
  summarise(Employment_average=mean(Employed_percent, na.rm=TRUE)) %>% 
  ungroup()

str(geo_employment)




boundaries_df2 <- as.data.frame(boundaries_df)
mapbox_api_key <- rstudioapi::askForPassword()

write_csv(boundaries_df2, "test.csv")
class(boundaries_df2)

boundaries_df %>% 
  select(geometry)

print(boundaries_df[5, ])