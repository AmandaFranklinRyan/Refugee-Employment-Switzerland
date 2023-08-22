library(tidyverse)
library(readxl)
library(rio)

# 1. Import Refugee Employment Data by Canton ---------------------------------------

#Data from January 2023
file_name <- "Refugee Employment Data.xlsx"
sheet_name <- "CH-Kt"

employment_data <- read_excel(file_name, sheet = sheet_name, range="A5:E32")

renamed_data <- employment_data %>% 
  rename(Canton=...1,
         Total=...2,
         Employment_Age=...3,
         Employed=...4,
         Employed_percent=...5)

employment_summarised <- renamed_data %>% 
  select(Canton, Employed_percent, Employment_Age) %>% 
  filter(Canton!="Total") %>% 
  arrange(desc(Employed_percent))

#Why does Geneva have such a high unemployment rate alongside Tessin and Waadt?
#Do the cantons generally have low employment rates across all sectors?

out_of_employment_demographic <- renamed_data %>% 
  filter(Canton!="Total") %>% 
  mutate(not_employment_age=(Total-Employment_Age)/Total) %>% 
  arrange(desc(not_employment_age))

#Between 27% and 42% of refugees with B permits are under 15 or over 64, are these
# mostly children or older people?

# 2. Import Refugee Employment Data by Nationality ---------------------------------------

sheet_nationality <- "CH-Nati"
nationality_data <- read_excel(file_name, sheet = sheet_nationality, range="A5:E129")

renamed_data_nationality <- nationality_data %>% 
  rename(Country=...1,
         Total=...2,
         Employment_Age=...3,
         Employed=...4,
         Employed_percent=...5)

#Clean to remove totals from dataframe etc.
remove <- c("Staat unbekannt","Ohne Nationalität","Afrika","Nordafrika",
            "Subsahara","Amerika","Asien","Europa","Herkunft unbekannt")
nationality_cleaned <- renamed_data_nationality  %>%
  filter(!str_detect(Country, "Total")) %>% 
  filter(!Country %in% remove)

numbers_nationality <- nationality_cleaned %>% 
  select(Country, Total) %>% 
  arrange(desc(Total)) %>% 
  head(11)

#Most refugees with B permits are from Eritrea, Syria or Turkey

#Considering only top 10 most common nationalities
not_working_age_nationality <- nationality_cleaned %>% 
  filter(Country %in% numbers_nationality$Country) %>% 
  mutate(not_employment_age=(Total-Employment_Age)/Total) %>% 
  arrange(desc(not_employment_age))

#Somalia has the largest population not of working age followed by Syria and Afghanistan

employment_nationality <- nationality_cleaned %>% 
  filter(Country %in% numbers_nationality$Country) %>% 
  arrange(desc(Employed_percent)) %>% 
  select(Country,Employed_percent)

#China and Sri Lanka have the highest employment rate with Syria, Iraq, Afghanistan and Turkey at the bottom
# Surprising that Somalia is above Syria and Iran is below Eritrea and Ethiopia, assuming education is an important factor
# Could it be because they have been in the country for longer?


# 3. Extract Cantonal employment Data for the 10 most common refugee nationalities --------

canton_list <- c("AG","AI","AR","BE","BL","BS","FR","GE","GL","GR","JU","LU",
                 "NE","NW","OW","SG","SH","SO","SZ","TG","TI","UR","VD","VS","ZH","ZG")


file_list <- list.files(path = "Raw Data/", full.names = TRUE)

canton_employment_df <- data.frame()

for (file in file_list) {
  
  #Extract date from filename
  date_data <- str_extract(file, "\\d{4}-\\d{2}")
  
  #Select top 10 nationalities for the individual file
  sheet_nationality <- "CH-Nati"
  nationality_data <- read_excel(file, sheet = sheet_nationality, range="A5:E129")
  
  renamed_data_nationality <- nationality_data %>% 
    rename(Country=...1,
           Total=...2,
           Employment_Age=...3,
           Employed=...4,
           Employed_percent=...5)
  
  #Remove all regional totals keeping only total number of refugees
  remove <- c("Staat unbekannt","Ohne Nationalität","Afrika","Nordafrika",
              "Subsahara","Amerika","Asien","Europa","Herkunft unbekannt")
  nationality_cleaned <- renamed_data_nationality  %>%
    filter(!str_detect(Country, "Total")) %>% 
    filter(!Country %in% remove)
  
  numbers_nationality <- nationality_cleaned %>% 
    select(Country, Total) %>% 
    arrange(desc(Total)) %>% 
    head(11)
  
  country_list <- numbers_nationality$Country
  
  country_list

  for (canton in canton_list) {
    canton_data <- read_excel(file, sheet = canton) %>%
      filter(`6-23` %in% country_list)
    
    canton_summary <- canton_data[1:5] %>%
      mutate(Canton = canton)
    
    renamed_canton <- canton_summary %>%
      rename(
        Country = `6-23`,
        Total = ...2,
        Employment_Age = ...3,
        Employed = ...4,
        Employed_percent = ...5
      )
    
    renamed_canton_date <- renamed_canton %>% 
      mutate(Date=date_data)
    
    canton_employment_df <- rbind(canton_employment_df, renamed_canton_date)
  }
  
}

table(canton_employment_df$Country)

rio::export(canton_employment_df, "Nationality and Employment2.csv")


  
