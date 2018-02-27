library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)
#library(maps)
library(rworldmap)
#reading the rds object

billionaires_data <- readRDS('r-objects/billionaires_data.rds')

billionaires_data <- billionaires_data %>% mutate(worth_billions = worth/1000)


age_65 <- billionaires_data[which(billionaires_data$age>65 & billionaires_data$gender=='F'), ]
age_65

age_m_40 <- subset(billionaires_data, gender=="M" & age < 40 & age > 0,
                  select=position:age)

age_f_40 <- subset(billionaires_data, gender=="F" & age < 40 & age > 0,
                   select=position:age)

age_40 <- subset(billionaires_data, age >0 & age < 40,
                 select=position:age)

plot(billionaires_data$age,billionaires_data$rank)

filter_age <- billionaires_data$age!=0

miss_age <- is.na(billionaires_data$age)
no_age1 <- billionaires_data$age==0

bb <- billionaires_data[no_age1,]



no_age_data <- billionaires_data[billionaires_data$age==0,]
# returned 56 entries which had 0 as age - not known values

# write a regex for pattern matching for selected words &,family,brothers,and,- 

toMatch <- c("&", "family", "brothers","and","-")

# updating the age col to 500 for paired or family names based on patterns

billionaires_data$age = ifelse(billionaires_data$age==0 & grepl(paste(toMatch, collapse="|"), billionaires_data$name),500,billionaires_data$age) 
# updated 27 age to 500, 29 to 0 (pattern doesnt match) 



ggplot(billionaires_data,aes(x=age,y= position )) +
  geom_col() 

ggplot(age_40,aes(x=age,y=rank)) + geom_col()

ggplot(age_f_40,aes(x=age,y=rank)) + geom_col()
ggplot(age_m_40,aes(x=age)) +geom_histogram(bins = 10)

ggplot(age_f_40,aes(x=industry,y=worth,group =source,color=source))+
  geom_boxplot() 

cor(billionaires_data)

# need to reverse the y axis ..placing the 1 on the top and postion 2000 at the bottom

billionaires_data %>% filter(!gender=='unknown'& !age==500 & !age == 0 ) %>% 
                ggplot(aes(x=age,y=position)) +
                geom_point(aes(col=gender,size = worth_billions)) +
                #geom_smooth(method="lm") +
                labs(y="Position", 
                x="Age", 
                title="Age Vs Position" 
                  ) 
gg    
gg1 <- billionaires_data %>% filter(!age==500 & !age==0) %>% 
  ggplot(aes(x=position,y=age)) +
  geom_point(aes(col=gender,size = worth/1000)) 
  
  
ggplotly(gg1)

# fortune inherited - family wealth 
gg3 <- billionaires_data %>%  filter(age == 500 | age ==0) %>% 
       ggplot(aes(x=position,y=age,group=country,color=country)) +
       geom_point()
gg3


#creating summary by industry

industry_summary <- billionaires_data %>% 
                      group_by(industry) %>% 
                       summarize(count = n()
                                 )

ggplot(industry_summary,aes(reorder(industry,count),count)) +
  geom_col(color="blue",fill="red")+
  theme(axis.text.x =element_text(angle=65,hjust = 1))
  




country_billionaires <- billionaires_data %>% 
                         filter(age >=60) %>% 
                         group_by(position,country,industry,name) %>% 
                         summarise(mean_worth = mean(worth)) 

ggplot(country_billionaires,aes(x=industry,y=position,label=name)) +
   geom_text(size=2.5,check_overlap = TRUE,aes(color="country"))
                    
#creating a age_category column in bill_data young,middle,old
#young >0 and < 40, middle >40 and < 60 , old >60 and  < 110


billionaires_age_category <- billionaires_data %>% 
                filter(!age==0 & !age == 500) %>% 
                mutate(age_category = ifelse(age>60,"Old",
                                     ifelse(age>40,"Middle","Young")
                       ))
# 1987 obs as the unknown age and 500 age grp are not included - 56 obs

#-------------------------------------
# age wise categorisation of billionaires
#input various age grps, get the no of billionaires in each industry wise

billionaires_data %>%
     filter(age >30 & age < 100) %>%
         group_by(industry) %>% 
           summarise(Count=n()) %>% 
            ggplot(aes(reorder(industry,Count),Count)) +
            geom_col(color="black",fill="purple")+
            theme(axis.text.x =element_text(angle=65,hjust = 1))
 
#-------------------------
# find the top 10 industry billionaires in the world

 billionaires_data %>% 
      filter(rank >=1 & rank <= 50 & !gender == 'unknown') %>% 
         group_by(industry,gender) %>% 
           summarise(
                     mean_worth = mean(worth)
                      ) %>% 
           ungroup() %>% 
           arrange(desc(mean_worth)) %>% View()
            ggplot(aes(x=industry,y=mean_worth,fill=gender)) +
             geom_col(width = 0.8, position=position_dodge()) +
              theme(axis.text.x = element_text(angle=65,hjust = 1)) 
             # geom_text(aes(label=count), vjust=1.6, color="black", size=3.5)+
             # coord_flip()


billionaires_data %>% 
             filter(rank >=1 & rank <=10) %>% 
               ggplot(aes(x=industry,y=worth)) +
                  geom_col()
 

##-------------------

#gender wise distribution 

dim(billionaires_data)

billionaires_data %>% 
                filter(!gender=='unknown') %>% 
                group_by(industry,gender,worth) %>% 
                summarise(
                    count = n(),
                    mean_worth= sum(worth)
                     ) %>% 
                    ungroup() %>% 
                    ggplot(aes(x=industry,y=worth,fill = gender)) +
                    geom_col() +
                    theme(axis.text.x = element_text(angle=65,hjust = 1)) 

#--------------------------

# country wise billionaires distribution

billionaires_data %>% 
       filter(!age==0 & !age==500  & !gender=='unknown' & country=='United States') %>% 
       group_by(country,industry) %>% 
       summarise(mean_worth = mean(worth)) %>% 
       ggplot(aes(reorder(x=industry,-mean_worth),mean_worth,fill=country)) +
       geom_col()+
       theme(axis.text.x = element_text(angle=65,hjus=1))

#-------------------
# gender wise billionaire's distribution all over the world 
mytable <- table(billionaires_data$gender)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Species\n (with sample sizes)")

# country wise average worth distribution of the billionaires

pie_ch <- billionaires_data %>% 
             filter(!age==0 | !age==500 | gender == 'unknown' ) %>% 
             group_by(country) %>% 
             summarise(
                      mean_worth = round(mean(worth)),
                      sum_worth = sum(worth),
                      pct_worth = round(mean(worth)/100)
             ) %>% 
             ungroup() %>% 
            arrange(desc(mean_worth))
 
            pie(pie_ch$pct_worth
                ,labels = pie_ch$country
                ,main="Pie Chart of Countries")
        
#--------------------

#female young billionaires
#Alexandra Andresen & Katharina Andresen - 2 outliers youngest billionaires but inherited from father

ggplot(age_f_40,aes(x=age,y=position,group=industry,size=worth,color=industry)) +
       geom_col()


map_world <- borders("world")
mp <- ggplot() + map_world


bb<-billionaires_data

data(countryExData)

#creating a spatial polygon

sp_data <- billionaires_data %>% 
              filter(!age==0 & !age== 500 & !gender=='unknown') %>% 
              group_by(country) %>% 
              summarise(
                 total_worth = sum(worth_billions)
              ) %>% 
              ungroup() %>% 
              arrange(desc(total_worth))



my_map <- joinCountryData2Map( sp_data
                             ,joinCode = "NAME"
                             ,nameJoinColumn = "country",
                             verbose=TRUE)
mapDevice() #create worldmap shaped window
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

library(classInt)

library(RColorBrewer)

#getting class intervals
classInt <- classIntervals( sp_data[["total_worth"]]
                            ,n=10, style = "jenks")


classInt[["data"]]
catMethod = classInt[["brks"]]
catMethod
#getting colours
colourPalette <- brewer.pal(9,'RdPu')
#plot map



mapParams <- mapCountryData(my_map
                            ,nameColumnToPlot="total_worth"
                            ,addLegend=FALSE
                            ,catMethod = catMethod
                            ,colourPalette=colourPalette 
                            )

#adding legend
do.call(addMapLegend
        ,c(mapParams
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))






mapDevice() #create world map shaped window
mapCountryData(sPDF[which(sPDF$LDC=='LDC'),]
               ,nameColumnToPlot="POP2005")


dev.off()

mapCountryData(my_map,nameColumnToPlot = 'total_worth')

identifyCountries(getMap()
                  ,nameColumnToPlot="worth_billions")


mapCountryData(my_map,nameColumnToPlot = 'position')
mapCountryData(my_map,nameColumnToPlot = 'gender',catMethod = 'categorical')
mapCountryData(my_map,nameColumnToPlot = 'age')

my_map

my_map <-sPDF
my_map1 <- my_map
sPDF <- getMap()

 #select countries from the map
my_map1 <-my_map[which(my_map$age>40 & my_map$age <= 102),]
mapDevice()
mapCountryData(my_map1
                   , nameColumnToPlot='worth'
                   , colourPalette='rainbow'
                   , mapTitle='Billionaires World Wide')

identifyCountries(getMap()
                  ,nameColumnToPlot="worth")
dim(my_map1)
dim(sPDF)






mapDevice()
mapBubbles(dF=map_data
           ,nameZSize="worth"
           ,nameZColour = "country"
           ,colourPalette="rainbow"
           ,oceanCol="lightblue"
           ,landCol="white")

View(sPDF)
dim(sPDF)
getMap()


#----------------------------------------

# grouping th industry sectors and retreiving other obs - rank,name,networth,age,source,country
#input industry,gender,country 
billionaires_data %>% 
                   filter(!age==0 & !age==500 & !gender =='unknown') %>% 
                   filter(industry=='Automotive' & gender =='M' & country=='United States') %>% 
                   select(rank,name,age,source,country) %>% 
                   arrange(rank) %>% View()
                   
#-----------------------------------------------

