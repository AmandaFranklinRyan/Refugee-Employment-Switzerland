library(plotly)
library(rio)


employment_data <- rio::import("Processed Data/Nationality and Employment 2022.csv")

#Clean data and correct types
cleaned_data <- employment_data %>% 
  mutate(Date=(paste("01-", Date, sep=""))) %>%  #Add day
  mutate(Date=as.POSIXct(Date, format = "%d-%m-%Y")) #Convert to datetime

#Plot distribution of nationalities in January 2023 for Switzerland
nationality_january_2023 <-cleaned_data %>% 
  filter(Date=="2022-01-01") %>% 
  select(Country,Total) %>% 
  group_by(Country) %>% 
  summarise(Total=sum(Total))

nationality_plot_2023 <- nationality_january_2023 %>%
  plot_ly(x = ~Country, y = ~Total) %>%
  add_bars()
