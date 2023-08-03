library(tidyverse)
library(readxl)
library(rio)

"C:\Users\amand\OneDrive\Bureau\Refugee Data\Refugee Employment Data Files\6-23-Best-Fluechtlinge-B-Erwerb-d-2022-01.xlsx"

# 1. Import Refugee Employment Data by Canton ---------------------------------------

#Daata from January 2023
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

#Why does Geneva have such a high unemployment rate alongisde Tessin and Waadt?
#Do the cantons generally have low employment rates across all sectors?

out_of_employment_demographic <- renamed_data %>% 
  filter(Canton!="Total") %>% 
  mutate(not_employment_age=(Total-Employment_Age)/Total) %>% 
  arrange(desc(not_employment_age))

#Between 27% and 42% of refugees with B permits are under 15 or over 64, are these
# mostly children or older people?

# 2. Import Refugee Employment Data by Nationality ---------------------------------------

sheet_nationality <- "CH-Nati"
nationality_data <- read_excel(file_name, sheet = sheet_nationality, range="A16:E129")

renamed_data_nationality <- nationality_data %>% 
  rename(Country=...1,
         Total=...2,
         Employment_Age=...3,
         Employed=...4,
         Employed_percent=...5)

#Clean to remove totals from dataframe etc.
remove <- c("Staat unbekannt","Ohne Nationalität")
nationality_cleaned <- renamed_data_nationality  %>%
  filter(!str_detect(Country, "Total")) %>% 
  filter(!Country %in% remove)

numbers_nationality <- nationality_cleaned %>% 
  select(Country, Total) %>% 
  arrange(desc(Total)) %>% 
  head(10)

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
                 "NE","NW","OW","SG","SH","SO","SZ","TG","TI","UR","VD","VS")
country_list <- employment_nationality$Country

canton_employment_df <- data.frame()

for (canton in canton_list){
  canton_data <- read_excel(file_name, sheet = canton) %>% 
    filter(`6-23` %in% country_list)
  
  canton_summary <- canton_data[1:5] %>% 
    mutate(Canton=canton)
  
  renamed_canton <- canton_summary %>% 
    rename(Country=`6-23`,
           Total=...2,
           Employment_Age=...3,
           Employed=...4,
           Employed_percent=...5)
  
  canton_employment_df <-rbind(canton_employment_df,renamed_canton)
}

rio::export(canton_employment_df, "Nationality and Employment.csv")


  
