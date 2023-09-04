library(tidyverse)
library(readxl)
library(rio)

# 1. Download all files on S Permit holders ------------------------------------
# Almost all immigrants granted 3 permits are Ukrainian, but there are a few hundred from Belarus
# Russia etc. 

# Data only available from June 2022 onwards
# Ran loop twice to download all files
#Two months had inconsistent link format so were downloaded manually

# Specify dataset parameters
n_dataset <- "6-24"
permit_category <- "S"
years <- seq(from=2022, to= 2023)
months <- c("01","02","03","04","05","06")

for (year in years){
  for (month in months){
    
    #Build url for every month
    url <- paste(base_url,year,"/",month,"/",n_dataset,"-Best-",permit_category,
                 "-Erwerb-d-",year,"-",month,".xlsx.download.xlsx/",n_dataset,
                 "-Best-",permit_category,"-Erwerb-d-",
                 year,"-",month,".xlsx", sep="")
    
    file_name <- paste("Raw Data\\S Permit\\",year,"-",month,".xlsx", sep="")
    
    #Use "try" to download as skips to next file if 404 error rather than stops
    try(download.file(url, destfile = file_name, mode="wb"))
    
    # Pause to reduce number fo requests
    Sys.sleep(time=2)
  }
}

# 2. Download all files on N Permit holders ------------------------------------
# These are asylum seekers 

# Data available from 1994 onwards (annually 1994-2011 and monthly 2012-2023)

n_dataset <- "6-21"
permit_category <- "N"
years <- seq(from=1994, to= 2011)
months <- c("12")

for (year in years){
  for (month in months){
    
    url <- paste(base_url,year,"/",month,"/",n_dataset,"-Best-",permit_category,
                 "-Erwerb-d-",year,"-",month,".xlsx.download.xlsx/",n_dataset,
                 "-Best-",permit_category,"-Erwerb-d-",
                 year,"-",month,".xlsx", sep="")
    
    file_name <- paste("Raw Data\\N Permit\\",year,"-",month,".xlsx", sep="")
    
    try(download.file(url, destfile = file_name, mode="wb"))
    
    Sys.sleep(time=2)
  }
}

# 3. Download all files on F Permit holders ------------------------------------
# These are asylum seekers who have been given temporary protection

# Data available from 1994 onwards (annually 1994-2011 and monthly 2012-2023)

n_dataset <- "6-22"
permit_category <- "VA"
years <- seq(from=1994, to=2012)
months <- c("12")

for (year in years){
  for (month in months){
    
    url <- paste(base_url,year,"/",month,"/",n_dataset,"-Best-",permit_category,
                 "-Erwerb-d-",year,"-",month,".xlsx.download.xlsx/",n_dataset,
                 "-Best-",permit_category,"-Erwerb-d-",
                 year,"-",month,".xlsx", sep="")
    
    file_name <- paste("Raw Data\\F Permit\\",year,"-",month,".xlsx", sep="")
    
    try(download.file(url, destfile = file_name, mode="wb"))
    
    Sys.sleep(time=2)
  }
}

# 4. Download all files on B Permit holders ------------------------------------
# These are asylum seekers 

# Data available from 1994 onwards (annually 1994-2011 and monthly 2012-2023)

n_dataset <- "6-23"
permit_category <- "B"
years <- seq(from=2008, to=2012)
months <- c("12")

for (year in years){
  for (month in months){
    
    url <- paste(base_url,year,"/",month,"/",n_dataset,"-Best-Fluechtlinge-",permit_category,
                 "-Erwerb-d-",year,"-",month,".xlsx.download.xlsx/",n_dataset,
                 "-Best-Fluechtlinge-",permit_category,"-Erwerb-d-",
                 year,"-",month,".xlsx", sep="")
    
    file_name <- paste("Raw Data\\B Permit\\",year,"-",month,".xlsx", sep="")
    
    try(download.file(url, destfile = file_name, mode="wb"))
    
    Sys.sleep(time=2)
  }
}
