library("rjson")
library(jsonlite)
library(magrittr)
library(dplyr)

data_2018 <- read_json("Data/data_2018.json",simplifyVector = TRUE)

colSums(is.na(data_2018))
names(data_2018)


data_2018 <- data_2018[-c(8,14:21,23:25)]


data_2018$gender[is.na(data_2018$gender)] <- "unknown"
data_2018$age[is.na(data_2018$age)] <- 0

saveRDS(data_2018, 'r-objects/data_2018.rds')
write.csv(data_2018, 'r-objects/data_2018.csv')


data_2018 <- data_2018 %>% mutate(worth_billions = worth/1000)

toMatch <- c("&", "family", "brothers","and","-")
data_2018$age = ifelse(data_2018$age==0 & grepl(paste(toMatch, collapse="|"), data_2018$name),500,data_2018$age) 
# updated 27 age to 500, 29 to 0 (pattern doesnt match) 

mydata_2018 <-   data_2018 %>% 
  filter(!age==0 & !age==500 & !gender=='unknown') 


mydata_2018$age_group <-
  case_when(
    mydata_2018$age < 20 ~ "less than 20",
    mydata_2018$age < 40 ~ "20-40",
    mydata_2018$age < 60 ~ "40-60",
    mydata_2018$age < 80 ~ "60-80",
    TRUE            ~ "80-110"
  )

get_image <- function(urlpart) {
  paste0("<img src =https:",urlpart," height= 52 ></img>")
}
mydata_2018$squareImage <- get_image(mydata_2018$squareImage)

#final dataset without 0 & 500 age grp - mydata-2018.rds
saveRDS(mydata_2018, 'r-objects/mydata_2018.rds')
write.csv(mydata_2018, 'r-objects/mydata_2018.rds')

saveRDS(data_2018, 'r-objects/data_2018.rds')
write.csv(data_2018, 'r-objects/data_2018.csv')

#dataset with age 0 & 500 is data-2018(family billionaires names)

#saving in shiny Data folder

saveRDS(mydata_2018, 'billionaires_2017/Data/mydata_2018.rds')
write.csv(mydata_2018, 'billionaires_2017/Data/mydata_2018.csv')

saveRDS(data_2018, 'billionaires_2017/Data/data_2018.rds')
write.csv(data_2018, 'billionaires_2017/Data/data_2018.csv')
