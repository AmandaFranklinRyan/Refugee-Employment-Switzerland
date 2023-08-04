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

ticktext=list("End of Q1", "End of Q2", "End of Q3", "End of Q4"),
tickvals=list("2016-04-01", "2016-07-01", "2016-10-01", "2016-12-30")

x_values <- list("2022-01-01","2022-06-0","2022-12-01")
x_labels <- list("January","June","December")


canton_data <- cleaned_data %>% 
  filter(year(Date)=="2022") %>%
  select(Country,Employed_percent,Canton,Date) %>% 
  group_by(Country)

canton_plot <- canton_data %>% 
  filter(Country=="Syrien") %>% 
  group_by(Canton) %>% 
  nest() %>% 
  mutate(plot=map2(data,Canton,\(data, Canton)
                   plot_ly(data = data, x = ~Date, y = ~Employed_percent) %>%
                    add_markers(name = ~Canton) %>% 
                   layout(xaxis=list(tickvals=x_values, ticktext=x_labels,
                                     title="2022",
                                     showline= T, linewidth=1, linecolor='black'),
                          yaxis=list(title="Employed",tickformat = ".1%")))) %>% 
  subplot(nrows = 5, shareY = TRUE, shareX = FALSE)
    


