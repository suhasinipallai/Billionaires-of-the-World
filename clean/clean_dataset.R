#clean dataset

library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)
#library(maps)
library(rworldmap)

#reading the rds object

billionaires_data <- readRDS('r-objects/billionaires_data.rds')

billionaires_data <- billionaires_data %>% mutate(worth_billions = worth/1000)

no_age_data <- billionaires_data[billionaires_data$age==0,]
# returned 56 entries which had 0 as age - not known values

# write a regex for pattern matching for selected words &,family,brothers,and,- 

toMatch <- c("&", "family", "brothers","and","-")

# updating the age col to 500 for paired or family names based on patterns

billionaires_data$age = ifelse(billionaires_data$age==0 & grepl(paste(toMatch, collapse="|"), billionaires_data$name),500,billionaires_data$age) 
# updated 27 age to 500, 29 to 0 (pattern doesnt match) 

b_data <-   billionaires_data %>% 
  filter(!age==0 & !age==500 & !gender=='unknown') 


b_data$age_group <-
  case_when(
    b_data$age < 20 ~ "less than 20",
    b_data$age < 40 ~ "20-40",
    b_data$age < 60 ~ "40-60",
    b_data$age < 80 ~ "60-80",
    TRUE            ~ "80-110"
  )

# fn to make the col- img src

get_image <- function(urlpart) {
  paste0("<img src =https:",urlpart," height= 52 ></img>")
}

#adding img src https to the squareimage col of the dataframe


b_data$squareImage <- get_image(b_data$squareImage)

#final dataset without 0 & 500 age grp - b_data.rds
#dataset with age 0 & 500 is billionaires_data.rds (family billionaires names)

saveRDS(billionaires_data, 'r-objects/billionaires_data.rds')
write.csv(billionaires_data, 'r-objects/billionaires_data.csv')

saveRDS(b_data, 'r-objects/b_data.rds')
write.csv(b_data, 'r-objects/b_data.csv')

#saving in shiny data folder
saveRDS(billionaires_data, 'billionaires_2017/Data/billionaires_data.rds')
write.csv(billionaires_data, 'billionaires_2017/Data/billionaires_data.csv')

saveRDS(b_data, 'billionaires_2017/Data/b_data.rds')
write.csv(b_data, 'billionaires_2017/Data/b_data.csv')



