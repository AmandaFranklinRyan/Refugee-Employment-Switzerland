library(sf)
library(tidyverse)
library(plotly)
library(rio)
library(rjson)
library(crosstalk)

# 1. Import Employment and Geo Data ---------------------------------------

#Swiss map data downloaded from:https://cartographyvectors.com/map/1522-switzerland-with-regions

#Import Data
employment_data <- rio::import("Processed Data/Nationality and Employment3.csv")
cantons_abbreviations <- rio::import("Geographic Data/Cantons and Abbreviations.xlsx") #Matches cantons and abbreviations
boundaries_df <- sf::st_read("Geographic Data/Switzerland boundaries.geojson")
boundaries_json <- rjson::fromJSON(file="Geographic Data/Switzerland boundaries.geojson")

#Clean data and correct types
cleaned_data <- employment_data %>% 
  mutate(Date=(paste(Date,"-01",sep=""))) %>%  #Add day
  mutate(Date=as.POSIXct(Date, format = "%Y-%m-%d")) %>%  #Convert to datetime
  mutate(Employed_percent=as.numeric(Employed_percent)) 

#Join geographic and employment Data
all_geo_data <- left_join(boundaries_df,cantons_abbreviations,by=c("name"="Canton")) #Join cantons and abbreviations
geo_employment <- left_join(cleaned_data,all_geo_data,by=c("Canton"="Abbreviation")) #Merge geographic and Employment Data


# 2. Plot number of refugees per canton over time -------------------------

#Plot number of refugees per canton over time with employed percentage
employment_all_refugees_df <- geo_employment %>% 
  select(Date,Country,Employed_percent,name,Total) %>% 
  filter(Country=="Gesamttotal") %>% 
  mutate(Employed_percent=ifelse(Employed_percent=="-",NA,Employed_percent)) %>% 
  mutate(Employed_percent=round(Employed_percent*100,2)) %>% 
  group_by(year(Date),name) %>% 
  summarise(Employed_percent=round(mean(Employed_percent,na.rm=TRUE),1),
            Total=round(mean(Total,na.rm=TRUE),0))

g <- list(
  fitbounds = "locations",
  visible = FALSE)

swiss_map <- plot_ly(hoverinfo = "text",
                     text = ~paste("Canton:", employment_all_refugees_df$name, "<br>",
                                   "Employed:", employment_all_refugees_df$Employed_percent,"%", "<br>",
                                   "Total:", employment_all_refugees_df$Total, "<br>")) %>%  
  add_trace(
    type="choropleth",
    geojson=boundaries_json,
    locations=employment_all_refugees_df$name,
    z=employment_all_refugees_df$Total,
    colorscale="Blues",
    frame=employment_all_refugees_df$`year(Date)`,
    reversescale =T,
    zmin=35,
    zmax=9590,
    featureidkey="properties.name") %>% 
  layout(geo = g) %>%  
  colorbar(thickess=20,
           #orientation="h",
           len=0.5) %>% 
  layout(title = "Number of Refugees with B Permits (2013-2023)")

# 2. Plot employment of refugees per canton over time -------------------------

g <- list(
  fitbounds = "locations",
  visible = FALSE)

swiss_map_employment <- plot_ly(hoverinfo = "text",
                     text = ~paste("Canton:", employment_all_refugees_df$name, "<br>",
                                   "Employed:", employment_all_refugees_df$Employed_percent,"%", "<br>",
                                   "Total:", employment_all_refugees_df$Total, "<br>")) %>%  
  add_trace(
    type="choropleth",
    geojson=boundaries_json,
    locations=employment_all_refugees_df$name,
    z=employment_all_refugees_df$Employed_percent,
    colorscale="Blues",
    frame=employment_all_refugees_df$`year(Date)`,
    reversescale =T,
    zmin=20,
    zmax=70,
    featureidkey="properties.name") %>% 
  layout(geo = g) %>%  
  colorbar(thickess=20,
           #orientation="h",
           len=0.5) %>% 
  layout(title = "Percentage of employed refugees with B permits (2013-2023)")

