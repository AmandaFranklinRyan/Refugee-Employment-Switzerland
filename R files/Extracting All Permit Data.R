library(tidyverse)
library(readxl)
library(rio)

#Extract data on refugee nationalities and work participation rate for files
# containing S,N,B and F permit data

permit_data_df <- data.frame()

#List all subfolders in raw data files
folder_list <- list.dirs("Raw Data\\", full.names = FALSE, recursive = FALSE)

for (folder in folder_list){
  
  #Loop through all files in each folder
  current_folder <- paste("Raw Data\\",folder,sep="")
  file_list <- list.files(current_folder, full.names = TRUE)
  
    for (file in file_list){

sheet_nationality <- "CH-Nati" #Look only at national level data not cantonal
nationality_data <- read_excel(file, sheet = sheet_nationality, range="A5:E129")

#Extract permit type and data data to add to dataframe
permit_type <- str_match(file, "(.) Permit")[, 2]
date <- str_extract(file, "\\d{4}-\\d{2}")

renamed_data_nationality <- nationality_data %>% 
  rename(Country=...1,
         Total=...2,
         Employment_Age=...3,
         Employed=...4,
         Employed_percent=...5)

#Only take the first 5 columns of data from Excel sheet
nationality_summary <- renamed_data_nationality[1:5] %>% 
  mutate(Permit=permit_type) %>% 
  mutate(Date=date)

permit_data_df <- rbind(permit_data_df, nationality_summary)

rio::export(permit_data_df,"Processed Data//All permits.csv")

}
  }

