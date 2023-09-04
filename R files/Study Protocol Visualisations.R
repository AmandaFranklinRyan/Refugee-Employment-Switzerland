library(tidyverse)
library(rio)
library(plotly)

#Import permit data
permit_data <- rio::import("Processed Data//All permits.csv")

#Clean data
data_clean <- permit_data %>% 
  filter(!is.na(Country) & !is.na(Employment_Age) & !is.na(Total) & !is.na(Employed)) %>%  #Remove rows containing only NAs
  mutate(Date=(paste(Date,"-01",sep=""))) %>%  #Add day
  mutate(Date=as.POSIXct(Date, format = "%Y-%m-%d")) %>%  #Convert to datetime
  mutate(Employed_percent=ifelse(Employed_percent=="-",NA,Employed_percent)) %>% # Convert "-" to NA
  mutate(Employed_percent=as.numeric(Employed_percent)) 

#Remove group totals, Europe, Oceania etc.

#Create a vector containing rows to remove
range_interest <- c(2:9,44,47,69, 81:85)
vector_remove <- data_clean$Country[range_interest]

data_groups_removed <- data_clean %>% 
  filter(!Country %in% vector_remove)

#Filter data to only include nationalities with more than 100 migrants a month
# at least once during the entire time period

#Create list of countries with total of more than 100 at least once
df_countries <- data_groups_removed %>% 
  filter(Total>100) %>% 
  select(Country) %>% 
  distinct() 

list_countries <- df_countries$Country

#Filter data to include only these 59 countries
data_summary <- data_groups_removed %>% 
  filter(Country %in% list_countries) %>% 
  select(Country, Total,Permit,Date)

# 2. Visualisations of Different Nationalities by permit ------------------

#Plot time series showing employment from 2013-2023 with B Permit Holders
number_plot <- data_summary %>% 
  filter(Permit=="B") %>% 
  group_by(Country) %>% 
  plot_ly(x=~Date, y=~Total) %>% 
  add_lines(color = ~Country) %>% 
  layout(title="Number of Refugees from most common host countries (2013-2023)",
         yaxis=list(title="Number of refugees"))

#There seems to be a large increase in refugees granted B permits from 2014 onwards?
# Did more refugees come or did the rule change?
#Russians with B permits also increased in 2014
#There were also a few refugees with B permits from Columbia, Tunisia and Serbia
#But why are their numbers decreasing over time? Did people leave the country or become resident?

#Create plot with most common B permit holding nationalities and Ukrainians (combine B and S permits)

#Create dataframe without Ukrainian data
countries_without_ukraine <- data_summary %>% 
  filter(Country!="Ukraine")

#Add S and B permit holding Ukrainians together
ukraine_only <- data_summary %>% 
  filter(Country=="Ukraine") %>% 
  filter(Permit=="B"|Permit=="S") %>% 
  filter(!Date=="2022-11-01" & !Date=="2023-03-01") %>% #These are missing on the BFS website
  group_by(Country,Date) %>% 
  summarise(Total=sum(Total)) %>% 
  ungroup() %>% 
  mutate(Permit="T") %>%  # T here means S+B permits
  select(Country, Total, Permit, Date)

#Combine dataframes
data_with_ukraine <- rbind(countries_without_ukraine,ukraine_only)


#Define countries with the most refugees for plotting
most_refugees <- c("Ukraine","Syrien","Eritrea","Türkei","Sri Lanka","Irak")

#Customise ticks on x-axis
custom_ticks <- as.Date(c("2008-12-01","2011-12-01","2014-12-01","2017-12-01","2020-12-01","2023-07-01"))
custom_labels <- c("2008","2011","2014","2017","2020","2023")

#Translate country names for plotting
plot_data <- data_with_ukraine %>% 
  filter(Country %in% most_refugees) %>% 
  mutate(Country=case_when(Country=="Syrien"~ "Syria",
                           Country=="Irak"~ "Iraq",
                           Country=="Türkei"~ "Turkey",
                           TRUE~Country))

colour_scheme <- c("#003f5c","#58508d","#bc5090","#ff6361","#ffa600")

t <- list(
  family = "Arial",
  size = 15)

number_plot <- plot_data %>% 
  filter(Permit=="B" | Permit=="T") %>% 
  filter(!Country=="Gesamttotal") %>% 
  group_by(Country) %>% 
  plot_ly(x=~Date, y=~Total) %>% 
  add_lines(color = ~Country,line = list(width = 2),colors = colour_scheme) %>% 
  layout(title = list(text = "<b>Figure 1: Most Common Nationalities of Recognised Refugees in Switzerland</b>",
                    x = 0,
                    font=t,
                    xanchor ="left",
                    xref = "paper"),
         annotations = list(x = 0, y = -0.3, 
                text = "Source: Data downloaded from BFS Website [3]", 
                showarrow = F, 
                xref='paper', 
                yref='paper'),
         xaxis = list(title = "",
                      tickvals = custom_ticks,   # Set custom tick positions
                      ticktext = custom_labels),  
         yaxis = list(title = "")) 

layout(annotations = 
         list(x = 0, y = -0.3, 
              text = "Source: Data downloaded from BFS Website [3]", 
              showarrow = F, 
              xref='paper', 
              yref='paper')
)

footer_text <- list(
  x = 0,                       # X-coordinate for centering the text
  y = -0.1,                      # Y-coordinate for the footer position
  xref = "paper",                # Use paper coordinates for X-axis
  yref = "paper",                # Use paper coordinates for Y-axis
  text = "Source: All Statistics downloaded from SEM website [20]",     # Footer text
  showarrow = FALSE,             # Don't show arrow
  font = list(size = 10, color = "black")  # Customize font size and color
)

footer_line2 <- list(
  x = 0,                       # X-coordinate for centering the text
  y = -0.15,                     # Y-coordinate for the second line of the footer
  xref = "paper",                # Use paper coordinates for X-axis
  yref = "paper",                # Use paper coordinates for Y-axis
  text = "Totals show number of S and B Permits for Ukrainians and B Permits for all other nationalities ",        # Text for the second line
  showarrow = FALSE,             # Don't show arrow
  font = list(size = 10, color = "black")  # Customize font size and color
)

# Add the footer text annotation
line_plot <- number_plot %>%
  layout(annotations = list(footer_text,footer_line2))

#Plot number of asylum seekers (N permits)

#Plot time series showing employment from 2013-2023 with N Permit Holders

asylum_nationalities <- c("Sri Lanka","Eritrea","Bosnien u. Herzegowina","Afghanistan",
                          "Syrien","Albanien","Äthiopien","Iran")

custom_ticks_2 <- as.Date(c("1995-12-01","2000-12-01","2005-12-01","2010-12-01","2015-12-01","2020-12-01"))
custom_labels_2 <- c("1995","2000","2005","2010","2015","2020")

colour_scheme_2 <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf")
  

asylum_plot <- data_summary %>% 
  filter(Permit=="N") %>% 
  filter(Country %in% asylum_nationalities) %>% 
  mutate(Country=case_when(Country=="Syrien"~ "Syria",
                           Country=="Albanien"~ "Albania",
                           Country=="Türkei"~ "Turkey",
                           Country=="Äthiopien"~ "Ethiopia",
                           Country=="Bosnien u. Herzegowina"~ "Bosnia and Herzegovina",
                           TRUE~Country)) %>% 
  group_by(Country) %>% 
  plot_ly(x=~Date, y=~Total) %>% 
  add_lines(color = ~Country, colors=colour_scheme_2) %>% 
  layout(title = list(text = "<b>Figure 2: Most Common Nationalities of Asylum Seekers (1994-2023)</b>",
                      x = 0,
                      font=t,
                      xanchor ="left",
                      xref = "paper"),
         yaxis=list(title=""),
         xaxis=list(title="",
                    tickvals = custom_ticks_2,   # Set custom tick positions
                    ticktext = custom_labels_2))

asylum_plot_final <- asylum_plot %>%
  layout(annotations = list(footer_text))


  
  