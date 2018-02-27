#install.packages("rjson")
library("rjson")
library(jsonlite)
library(magrittr)
library(dplyr)
#library(xlsx)

#json_data <- fromJSON(file="C:/users/suhasini/desktop/billionaires-data/forbes_2000.json")
#billion_csv_data <- read.csv("billionaires.csv")

# modified the json source data placing 83 observations at the end as their position/rank data elements was missing 
# and making the df start from names rather than position and rank

j <- read_json("Data/forbes_2000.json",simplifyVector = TRUE)

json_data <- read_json("Data/forbes1_2000.json",simplifyVector = TRUE)

#deleting the obs where rank,position and variables pay,managementAssets,salary are missing 

billionaires_data <- json_data[-c(2044:2126),-c(18:20,23:25)]

#deleted 183 obs and 3 colms


colnames(billionaires_data)


missing_age1 <- billionaires_data %>% dplyr::filter(is.na('age'))

missing_age1 <- is.na(billionaires_data$age)
View(missing_age1)
no_age <- billionaires_data[missing_age1,]


colSums(is.na(billionaires_data))

missing_age_csv <- read.csv(file="Data/missing_age.csv", header=TRUE, sep=",")
class(missing_age_csv)


merge_billionaire_missing_age <- merge(x = billionaires_data, y = missing_age_csv, by = "name",all.x = TRUE)

billionaires_data <- dplyr::left_join(billionaires_data, missing_age_csv, by = c("name")) %>% 
  mutate(age = ifelse(is.na(age.x), age.y, age.x)) 

colnames(billionaires_data)

# 11 missing gender names - but couldn't get their information gender info from google as they are either 
# names of 2 persons(couples) or persons who inherited from their father

missing_gender <- is.na(billionaires_data$gender)
no_gender <- billionaires_data[missing_gender,]

# deleting columns worthchange(121),title(1697),headquarters(1482),state(1482),govenment(1697),age.y

billionaires_data <- billionaires_data[-c(8,9,14,16:18,20)]

#replacing 'unknown' in null values in gender & age values with "0"

billionaires_data$gender[is.na(billionaires_data$gender)] <- "unknown"
billionaires_data$age[is.na(billionaires_data$age)] <- 0

# replaced 11 gender values to "unknown" and 56 age values to 0


saveRDS(billionaires_data, 'r-objects/billionaires_data.rds')
write.csv(billionaires_data, 'r-objects/billionaires_data.csv')







 
   
