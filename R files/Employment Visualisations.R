library(plotly)
library(rio)


employment_data <- rio::import("Processed Data/Nationality and Employment 2022.csv")

#Clean data and correct types
cleaned_data <- employment_data %>% 
  mutate(Date=(paste("01-", Date, sep=""))) %>%  #Add day
  mutate(Date=as.POSIXct(Date, format = "%d-%m-%Y")) %>%  #Convert to datetime
  mutate(Employed_percent=as.numeric(Employed_percent))  #Convert percent to numeric not character

#Plot distribution of nationalities in January 2022 for Switzerland
nationality_january_2023 <-cleaned_data %>% 
  filter(Date=="2022-01-01") %>% 
  select(Country,Total) %>% 
  group_by(Country) %>% 
  summarise(Total=sum(Total)) %>% 
  mutate(Country=fct_reorder(Country,Total, .desc=TRUE)) #Convert to factor for plotting
  

nationality_plot_2023 <- nationality_january_2023 %>%
  plot_ly(x = ~Country, y = ~Total, hoverinfo="text",
          text=~paste("Country",Country, "<br>","Total:",Total)) %>%
  add_bars(color=I("blue"), opacity=0.5) %>% 
  layout(title="Most common Refugee Nationalities with B Permits in Switzerland in January 2022",
         xaxis=list(title=FALSE,showgrid=FALSE),
         yaxis=list(showgrid=FALSE))

#Plot line chart showing employment by nationality in Switzerland over time in 2022

#Create dataframe containing plot data
employment_time_data <- cleaned_data %>% 
  filter(year(Date)=="2022") %>% 
  group_by(month(Date), Country, Date) %>% 
  summarise(Employment_average=mean(Employed_percent)) %>% 
  ungroup()

#Plot time series showing employment for Syrians over 2022
employment_plot <- employment_time_data %>% 
  group_by(Country) %>% 
  plot_ly(x=~Date, y=~Employment_average) %>% 
  add_lines(color = ~Country) %>% 
  layout(title="Employment by Most Common Refugee Nationalities",
    yaxis=list(title="Average employment",tickformat = ".1%"))

#Create subplots to see differences between cantons
canton_data <- cleaned_data %>% 
  filter(year(Date)=="2022") %>%
  select(Country,Employed_percent,Canton,Date)


