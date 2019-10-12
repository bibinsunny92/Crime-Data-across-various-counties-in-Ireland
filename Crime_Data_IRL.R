setwd("E:/MSc BUSINESS ANALYTICS/Project/Datasets")
Garda <- read.csv("garda_stations.csv") 

# Loading the required packages
# Loading the Librarires
install.packages("caTools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")

library(caTools)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(stringr)
library(leaflet)
library(corrplot)

summary(Garda)
str(Garda)


#### Process of data cleaning

# 1. removing 2016 data and ID column
Garda <- Garda %>% select(-contains("2016"))
Garda <- Garda[,-1]

dataCrime = gather(data = Garda, key = Crime_Type, value = No_of_Incidents, -Station,-Divisions,-x,-y)


### Extracting year value
dataCrime$Year = substr(dataCrime$Crime_Type,(nchar(dataCrime$Crime_Type)+1)-4,nchar(dataCrime$Crime_Type))

dataCrime$Crime_Type = substr(dataCrime$Crime_Type,1,nchar(dataCrime$Crime_Type)-4)

### Replacing dots with spaces
dataCrime$Crime_Type = gsub("\\."," ",dataCrime$Crime_Type)

dataCrime$County = gsub("\\Division$"," ",dataCrime$Divisions)

dataCrime$County = gsub("\\D.M.R.*","Dublin",dataCrime$County)

dataCrime$County = gsub("Cork City","Cork",dataCrime$County)
dataCrime$County = gsub("Cork North","Cork",dataCrime$County)
dataCrime$County = gsub("Cork West","Cork",dataCrime$County)

### Finding which crime type had most number of reported incidents

dataCrime %>%
  select(Crime_Type,No_of_Incidents) %>%
  group_by(Crime_Type) %>%
  summarize(TotalIncidents = sum(No_of_Incidents, na.rm=TRUE)) %>% arrange(desc(TotalIncidents))

### Finding which year had most number of reported incidents

dataCrime %>%
  select(Year,No_of_Incidents) %>%
  group_by(Year) %>%
  summarize(TotalIncidents = sum(No_of_Incidents, na.rm=TRUE)) %>% arrange(desc(TotalIncidents))

### Finding which county had most number of reported incidents

dataCrime %>%
  select(County,No_of_Incidents) %>%
  group_by(County) %>%
  summarize(TotalIncidents = sum(No_of_Incidents, na.rm=TRUE)) %>% arrange(desc(TotalIncidents))

###  Finding within Dublin, Cork, Limerick and Galway which area/place reported highest number of incidents

##### Top 10 stations in Dublin where highest number of incidents were reported?

dataCrime %>%
  select(Station,No_of_Incidents) %>%
  filter(dataCrime$County == " Dublin") %>%
  group_by(Station) %>%
  summarize(TotalIncidents = sum(No_of_Incidents, na.rm=TRUE)) %>% arrange(desc(TotalIncidents))

##### Top 10 stations in Cork where highest number of incidents were reported?

dataCrime %>%
  select(Station,No_of_Incidents) %>%
  filter(dataCrime$County == " Cork  ") %>%
  group_by(Station) %>%
  summarize(TotalIncidents = sum(No_of_Incidents, na.rm=TRUE)) %>% arrange(desc(TotalIncidents))

##### Top 10 stations in Limerick where highest number of incidents were reported?

dataCrime %>%
  select(Station,No_of_Incidents) %>%
  filter(dataCrime$County == " Limerick  " ) %>%
  group_by(Station) %>%
  summarize(TotalIncidents = sum(No_of_Incidents, na.rm=TRUE)) %>% arrange(desc(TotalIncidents))

##### Top 10 stations in Galway where highest number of incidents were reported?

dataCrime %>%
  select(Station,No_of_Incidents) %>%
  filter(dataCrime$County == " Galway  ") %>%
  group_by(Station) %>%
  summarize(TotalIncidents = sum(No_of_Incidents, na.rm=TRUE)) %>% arrange(desc(TotalIncidents))


### Exporting data to csv
write.csv(dataCrime, file = "CrimeData.csv")