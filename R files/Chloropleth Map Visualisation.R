library(sf)
library(tidyverse)
library(plotly)
library(rio)
library(rjson)
library(crosstalk)

#Swiss map data downloaded from:https://cartographyvectors.com/map/1522-switzerland-with-regions

#Import Data
employment_data <- rio::import("Processed Data/Nationality and Employment 2022.csv")
cantons_abbreviations <- rio::import("Geographic Data/Cantons and Abbreviations.xlsx") #Matches cantons and abbreviations
boundaries_df <- sf::st_read("Geographic Data/Switzerland boundaries.geojson")
boundaries_json <- rjson::fromJSON(file="Geographic Data/Switzerland boundaries.geojson")

#Join geographic and employment Data
all_geo_data <- left_join(boundaries_df,cantons_abbreviations,by=c("name"="Canton")) #Join cantons and abbreviations
geo_employment <- left_join(employment_data,all_geo_data,by=c("Canton"="Abbreviation")) #Merge geographic and Employment Data

write_csv(geo_employment,"Processed Data/Employment and Geography.csv")

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
           #orientation="h",
           len=0.5) %>% 
  layout(title = "2022 Refugee Employment in Switzerland")

#Create a bar chart of nationalities and employment rate by canton
employment_barchart_data <- geo_employment %>% 
  select(Country,Employed_percent,name) %>% 
  mutate(Employed_percent=ifelse(Employed_percent=="-",NA,Employed_percent)) %>% 
  mutate(Employed_percent=as.numeric(Employed_percent)) %>%
  group_by(name,Country) %>% 
  summarise(Employment_average=mean(Employed_percent, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(Employment_average=round(Employment_average*100,2)) %>% 
  filter(name=="Luzern") %>% 
  mutate(Country=fct_reorder(Country,-Employment_average, .desc=TRUE))

employment_bar_chart <- employment_barchart_data %>% 
  plot_ly(y = ~Country, x = ~Employment_average, hoverinfo="text",
          text=~paste("Country",Country, "<br>","Total:",Employment_average)) %>% 
  add_bars(color=I("blue"), opacity=0.5) %>% 
  layout(title="Refugee Employment by Nationality in 2022",
         yaxis=list(title=FALSE,showgrid=FALSE),
         xaxis=list(showgrid=FALSE, title= "Employment (%)"))

#Create Interactive Map choosing refugee country of origin
#For mapbox plots
#mapbox_api_key <- rstudioapi::askForPassword()

# 3. Create Interactive Map choosing refugee country of origin ------------

#Plot employment percentage of all refugees in 2022
interactive_df <- geo_employment %>% 
  select(Country,Total,Employed_percent,name,Date,Employment_Age) %>%
  mutate(Employed_percent=ifelse(Employed_percent=="-",NA,Employed_percent)) %>% 
  mutate(Employed_percent=as.numeric(Employed_percent)) %>% 
  group_by(name,Country,Date) %>% 
  summarise(Employed_percent=mean(Employed_percent, na.rm=TRUE),
            Total=sum(Total),
            Employment_Age=sum(Employment_Age)) %>% 
  mutate(Date=(paste("01-", Date, sep=""))) %>%  #Add day
  mutate(Date=as.POSIXct(Date, format = "%d-%m-%Y"))   #Convert to datetime


#Create choropleth map from geojson
g <- list(
  fitbounds = "locations",
  visible = FALSE)

swiss_map <- plot_ly(hoverinfo = "text",
                     text = ~paste("Canton:", interactive_df$name, "<br>",
                                   "Employed:", interactive_df$Employed_percent,"%", "<br>",
                                   "Employment Age",interactive_df$Employment_Age,"<br>",
                                   "Total:", interactive_df$Total, "refugees")) %>%  
  add_trace(
    type="choropleth",
    geojson=boundaries_json,
    locations=interactive_df$name,
    z=interactive_df$Employed_percent,
    #frame=~interactive_df$Country,
    colorscale="Blues",
    reversescale =T,
    #zmin=20,
    #zmax=70,
    featureidkey="properties.name") %>% 
  layout(geo = g) %>%  
  colorbar(thickess=20,
           #orientation="h",
           len=0.5) %>% 
  layout(title = "2022 Refugee Employment in Switzerland")

# 3. Create Subplots for each nationality ---------------------------------

nationality_list <- c("Syrien","Türkei","Eritrea", "Afghanistan")
plot_list <- list()
g <- list(
  fitbounds = "locations",
  visible = FALSE)

for (nationality in nationality_list){
  current_df <- interactive_df %>% 
    filter(Country==nationality)

  swiss_plot <- plot_ly(hoverinfo = "text",
                        geo = 'geo',
                        showlegend = F,
                        legendgroup=current_df$Country,
                       text = ~paste("Canton:", current_df$name, "<br>",
                                     "Employed:", current_df$Employed_percent,"%", "<br>",
                                     "Employment Age",current_df$Employment_Age,"<br>",
                                     "Total:", current_df$Total, "refugees")) %>%  
    add_trace(
      type="choropleth",
      geojson=boundaries_json,
      locations=current_df$name,
      z=current_df$Employed_percent,
      frame=current_df$Date,
      colorscale="Blues",
      reversescale =T,
      zmin=20,
      zmax=70,
      featureidkey="properties.name") %>%
    colorbar(thickess=20,
      len=0.5) %>%
  layout(geo = g, annotations = list(x = 1 , y = 1.05, text = nationality, showarrow = F,
                                     xref='paper', yref='paper'))
  
  plot_list <- append(plot_list,list(swiss_plot))
}

names(plot_list) <- nationality_list

subplot(plot_list, nrows=4)  %>% layout(title = "2022 Refugee Employment in Switzerland")

#%>% layout(geo = g, geo2 = g, geo3=g)





  