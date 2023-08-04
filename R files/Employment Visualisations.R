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

x_values <- list("2022-01-01","2022-06-01","2022-12-01")
x_labels <- list("Jan","June","Dec")

canton_data <- cleaned_data %>% 
  filter(year(Date)=="2022") %>%
  select(Country,Employed_percent,Canton,Date) %>% 
  group_by(Country)

# Calculate average employment value across all cantons in a single month
average_employment_syria <- cleaned_data %>% 
  filter(year(Date)=="2022" & Country=="Syrien") %>%
  select(Employed_percent, Canton,Date) %>% 
  group_by(Date) %>% 
  summarise(average_employment=mean(Employed_percent))

#Create faceted plot of Syrian employment for all cantons
canton_plot <- canton_data %>% 
  filter(Country=="Eritrea") %>% 
  group_by(Canton) %>% 
  nest() %>% 
  mutate(plot=map2(data,Canton,\(data, Canton)
                   plot_ly(data = data, x = ~Date, y = ~Employed_percent) %>%
                   add_lines(color=I("blue")) %>% 
                   layout(title= "Employment Rate for Syrian refugees across cantons in 2022",
                     xaxis=list(tickvals=x_values, ticktext=x_labels,
                                     title=FALSE,
                                     showline= T, linewidth=1, linecolor='black'),
                          yaxis=list(title=FALSE,
                                     range = list(0, 0.75),
                                     tickformat = "1%",
                                     tickfont = list(size = 10),
                                     showline= F),
                          showlegend=FALSE) %>% 
                     add_annotations(
                       text = ~unique(Canton),
                       x = 0.5,
                       y = 1,
                       yref = "paper",
                       xref = "paper",
                       xanchor = "middle",
                       yanchor = "top",
                       showarrow = FALSE,
                       font = list(size = 12)
                     )
                                     )) %>% 
  subplot(nrows = 6, shareY = TRUE, shareX = FALSE)

#Create dot plot of average annual employment of Syrians by Canton in 2022

dot_data <- cleaned_data %>% 
  filter(year(Date)=="2022" & Country=="Syrien") %>% 
  select(Date,Employed_percent,Canton) %>% 
  group_by(Canton) %>% 
  summarise(Average_employment=mean(Employed_percent))

dot_plot <- dot_data %>% 
  plot_ly(x=~Average_employment, y=~fct_reorder(Canton,Average_employment)) %>% 
  add_markers() %>% 
  layout(xaxis=list(title="Average Employment",tickformat = "1%"),
         yaxis=list(title=FALSE),
         title="Employment Rate amongst Syrians in 2022")
  
#Create chloropleth map of employment

    


