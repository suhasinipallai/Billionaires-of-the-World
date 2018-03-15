library(tidyverse)
library(magrittr)
library(shiny)
library(DT)
library(plotly)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {


# top 25 countries of the world with max no.of billionaires
  
temp <- b_data %>%
    group_by(
      country,
      gender
    ) %>%
    summarise(
      count=n(),
      total_networth = sum(worth_billions)
    ) %>%
    ungroup()
  temp <- temp[order(-temp$count),][1:25,]
  
# rendering countrywise plot
  
output$countryPlot <- renderPlot({
    
    ggplot(data=temp,aes(reorder(country,count),count,fill=gender)) +
      geom_col(position='dodge') +
      
      geom_text(aes(label=count), vjust=-0.3, size=3.5)+
      scale_y_continuous(breaks=seq(0,600,50)) +
      labs(
        #title = "\n    Billionaires count in different Countries   " ,
        x =" Country " ,y = "Count of Billionaires " , fill = "Gender") +
      
      theme(
        plot.title = element_text(color="black", size=14, face="bold.italic"),
        plot.background  = element_rect(fill = "lightblue") ,
        
        panel.background = element_rect(color = "grey50"),
        panel.ontop = FALSE ,
        
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        
        axis.text.x = element_text( hjust = 1,colour = "black",size=12,face="bold" ),
        axis.text.y = element_text( hjust = 1,colour = "black",size=12,face="bold" ),
        
        legend.box.background = element_rect(),
        legend.text = element_text(face = "bold",size=12,colour = "black"),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.title =  element_text(face="bold",colour="black") 
        
      )+
      
      coord_flip()
    
  })
  
# industry-wise plot  
output$industryPlot <- renderPlot({
    
      input_gender <- input$gender_var
      input_age <- input$age_var
      input_country <- input$country_var
    
#condition when  the inputs are not changed 
if (input_gender == 'All' & input_age == "All" & input_country=='All'){
      
          filter_select <- b_data %>%
                           group_by(
                             industry,
                             gender
                                  ) %>% 
                           summarise(
                             count=n()
                             )
      # condition when input gender changes 
      } else if(!input_gender == 'All' &  input_country=='All' & input_age == 'All') {
              
         filter_select <- b_data %>% 
                          filter(
                                gender == input_gender
                               ) %>% 
                          group_by(
                               industry,gender
                               ) %>% 
                          summarise(
                              count =n()
                                )
       
        # condition when the country and age factors are selected
        } else if(input_gender == 'All' &  !input_country=='All' & input_age=='All') {
                   
            filter_select <-  b_data %>% 
                              filter(
                                country == input_country
                                ) %>% 
                              group_by(
                                industry,
                                gender
                                #age_group
                                ) %>% 
                              summarise(
                                count =n()
                                )
          
         } else if(input_gender == 'All' &  !input_country=='All' & !input_age=='All') {
              
             filter_select <-  b_data %>% 
                               filter(
                                 country == input_country &
                                 age_group == input_age
                                 ) %>% 
                               group_by(
                                 industry,
                                 gender,
                                 age_group
                                 ) %>% 
                               summarise(
                                 count =n()
                                 )
          
        } else if(!input_gender == 'All' &  !input_country=='All' & input_age=='All') {
          
             filter_select <-  b_data %>% 
                               filter(
                                 country == input_country &
                                 gender == input_gender
                                 ) %>% 
                               group_by(
                                 industry,
                                 gender
                                 ) %>% 
                               summarise(
                                 count =n()
                                 )
          
        } else if(input_gender == 'All' &  input_country=='All' & !input_age=='All') {
              
              filter_select <-  b_data %>% 
                                filter(
                                  age_group == input_age
                                  ) %>% 
                                group_by(
                                  industry,
                                  gender,
                                  age_group
                                  ) %>% 
                                summarise(
                                  count =n()
                                  )
          
        } else if(!input_gender == 'All' &  input_country=='All' & !input_age=='All') {
          
              filter_select <-  b_data %>% 
                                filter(
                                  age_group == input_age &
                                  gender == input_gender   
                                    ) %>% 
                                group_by(
                                  industry,
                                  gender,
                                  age_group
                                  ) %>% 
                                summarise(
                                  count =n()
                                  )
          
          
      } else {
                filter_select <- b_data %>% 
                                 filter(
                                   gender==input_gender &
                                   country ==input_country &
                                   age_group == input_age
                                      ) %>% 
                                   group_by(
                                     industry,
                                     gender,
                                     age_group
                                     ) %>% 
                                   summarise(
                                     count =n()
                                     )
    }
  
# plot to show the number of billionaires in each industry with various factors - gender,age,country
ggplot(filter_select,
             aes(
               reorder(industry,count),count,fill = gender)) +
             geom_col(position = 'dodge')+ 
             geom_text(
                  aes(
                    label=count
                    ),
                   hjust=.2, 
                   size=3.5)  +
             labs(
              # title = "Billionaires count in each Industry   " ,
               x = " Industry " ,
               y = "Count of Billionaires " , 
               fill = "Gender"
               ) +
       
             theme(
         
             plot.title = element_text(color="black", size=14, face="bold"),
             plot.background  = element_rect(fill = "lightblue") ,
            
             panel.background = element_rect(color = "grey50"),
             panel.ontop = FALSE ,
     
             axis.title.x = element_text(color="black", size=14, face="bold"),
             axis.title.y = element_text(color="black", size=14, face="bold"),
         
             axis.text.x = element_text(angle=65,hjust = 1, colour = "black",size=12,face="bold" ),
             axis.text.y = element_text(hjust = 1, colour = "black",size=12,face="bold" ),
         
             legend.box.background = element_rect(),
             legend.text = element_text(face = "bold",size=12,colour = "black"),
             legend.box.margin = margin(6, 6, 6, 6),
             legend.title =  element_text(face="bold",colour="black")
          
             )
      
    })  
  

# source plot when UI changes

output$sourcePlot <- renderPlot({
                   
              input_gender <- input$gender_var
              input_age <- input$age_var
              input_country <- input$country_var
    
 if(input_gender=='All' & input_age == 'All' & input_country=='All' ){
            
        filter_select <- b_data %>% 
                           group_by(
                             source,
                             gender,
                             age_group
                             ) %>%
                           summarise(
                             count = n(),
                             sum_networth = sum(worth_billions)
                                    )
      
        filter_select <- filter_select %>%
                         filter(
                           sum_networth>=20
                           )
      
      
  } else if(!input_gender=='All' & input_country=='All' & input_age == 'All') {
      
           filter_select <- b_data %>% 
                            filter(
                              gender==input_gender
                              ) %>% 
                            group_by(
                              source,
                              gender,
                              age_group
                              ) %>% 
                            summarise(
                              count=n(),
                              sum_networth=sum(worth_billions)
                                   )
              
 if(filter_select$gender=='M' | filter_select$gender=='F'){
                 
               filter_select <- filter_select %>%
                                filter(
                                  sum_networth>=20
                                  )
           } 
           
    } else if(input_gender=='All' & !input_country == 'All' & input_age == 'All') {
      
             filter_select <- b_data %>% 
                              filter(
                                country == input_country 
                                   ) %>% 
                               group_by(
                                 source,
                                 gender,
                                 age_group
                                 ) %>% 
                               summarise(
                                 count=n(),
                                 sum_networth = sum(worth_billions)
                                  )
             
    } else if(input_gender=='All' & !input_country == 'All' & !input_age == 'All') {
              
             filter_select <- b_data %>% 
                              filter(
                                country == input_country & 
                                age_group == input_age
                                  ) %>% 
                              group_by(
                                source,
                                gender,
                                age_group
                                ) %>% 
                              summarise(
                                count=n(),
                                sum_networth = sum(worth_billions)
                                    )
      
  } else if(!input_gender =='All' & !input_country == 'All' & input_age == 'All') {
      
      
         filter_select <- b_data %>% 
                          filter(
                            gender == input_gender &
                            country == input_country    
                              ) %>% 
                          group_by(
                            source,
                            gender,
                            age_group
                            ) %>% 
                          summarise(
                            count=n(),
                            sum_networth = sum(worth_billions)
                            )
      
    } else if(input_gender=='All' & input_country == 'All' & !input_age == 'All') {
      
             filter_select <- b_data %>% 
                              filter(
                                age_group == input_age 
                                ) %>% 
                              group_by(
                                source,
                                gender,
                                age_group
                                ) %>% 
                             summarise(
                               count=n(),
                               sum_networth = sum(worth_billions)
                               )
      
   } else if(!input_gender=='All' & input_country == 'All' & !input_age == 'All') {
      
             filter_select <- b_data %>% 
                              filter(
                                age_group == input_age &
                                gender == input_gender   
                                 ) %>% 
                               group_by(
                                 source,
                                 gender,
                                 age_group
                                 ) %>% 
                              summarise(
                                count=n(),
                                sum_networth = sum(worth_billions)
                                )
             
     if(filter_select$gender=='M'){
               
               filter_select <- filter_select %>%
                 filter(
                   sum_networth>=50
                 )
             }
      
  } else {
               filter_select <- b_data %>% 
                        filter( country == input_country & 
                                gender == input_gender &
                                age_group == input_age   
                                ) %>% 
                       group_by(source,
                                gender,
                                age_group
                                ) %>%
                       summarise(count = n(),
                                 sum_networth = sum(worth_billions)
                       )
    }
# plot for count of billionaires source wise and their sum total net worth    
  ggplot(filter_select, 
             aes(source,count,size=sum_networth,colour=gender)) +
             geom_point()+
             theme(axis.text.x=element_text(angle=65,hjust=1)) +
             #facet_grid(~ age_group)+
             labs( 
               #title = "\n    Investment Area   " ,
             x =" Source " ,y = "Count of Billionaires " , colour = "Gender",size="Total\n Net Worth \n in Billions") +
       
       theme(plot.title = element_text(color="black", size=14, face="bold"),
             plot.background  = element_rect(fill = "lightblue") ,
         
             panel.background = element_rect(color = "grey50"),
             panel.ontop = FALSE ,
          
             axis.title.x = element_text(color="black", size=14, face="bold"),
             axis.title.y = element_text(color="black", size=14, face="bold"),
         
             axis.text.x = element_text(hjust = 1,colour = "black",size=12,face="bold" ),
             axis.text.y = element_text(hjust = 1,colour = "black",size=12,face="bold" ),
         
             legend.box.background = element_rect(),
             legend.text = element_text(face = "bold",size=12,colour = "black"),
             legend.box.margin = margin(6, 6, 6, 6),
             legend.title =  element_text(face="bold",colour="black") 
          )
    
    })
# selecting certain columns to get displayed in data table
b_data_selected <- b_data %>%
                       select(
                             rank,
                             name,
                             age,
                             gender,
                             country,
                             age_group,
                             worth_billions,
                             industry,
                             source,
                             squareImage
                             )
#rendering data table -for year 2017     
output$mytable1 <- renderDataTable({
     
           input_gender = input$gender_var
           input_age = input$age_var
           input_country = input$country_var
  
      if(input_gender=='All' & input_age=='All' & input_country=='All'){
       
         selected <- b_data_selected %>% 
                       select(-c(age_group)
                              )
         
     } else if(input_gender == 'All' & input_age == 'All' & !input_country=='All'){
       
          selected <- b_data_selected %>% 
                          filter(
                            country == input_country
                            ) %>% 
                            select(
                             -c(country,age_group)
                             )
          
     } else if(input_gender == 'All' & !input_age == 'All' & !input_country=='All') {
       
           selected <- b_data_selected %>% 
                          filter(
                            age_group == input_age &
                            country == input_country
                              ) %>% 
                           select(
                             -c(country,
                                age_group)
                             )
  
     } else if(!input_gender == 'All' & input_age == 'All' & input_country=='All') {
  
            selected <- b_data_selected %>% 
                           filter(
                               gender == input_gender
                               ) %>% 
                           select(
                             -c(gender,age_group)
                             )
            
     } else if(input_gender=='All' & !input_age=='All' & input_country=='All') {
       
             selected <- b_data_selected %>% 
                            filter(
                              age_group == input_age
                              ) %>% 
                            select(
                              -c(age_group)
                              )
             
     } else if(!input_gender == 'All' & !input_age == 'All' & input_country == 'All') {
       
            selected <- b_data_selected %>% 
                            filter(
                              gender == input_gender &
                              age_group == input_age
                              ) %>% 
                            select(
                              -c(gender,age_group)
                              )
            
     } else if(!input_gender == 'All' & input_age == 'All' & !input_country == 'All'){
        
             selected <- b_data_selected %>% 
                             filter(
                               gender == input_gender & 
                               country == input_country
                               ) %>% 
                             select(
                               -c(gender,country,age_group)
                               )
       
     } else {
               
             selected <- b_data_selected %>% 
                             filter(
                               gender == input_gender &
                               age_group == input_age &
                               country == input_country
                               ) %>% 
                             select(
                               -c(gender,country,age_group)
                               )
      }
     
    
DT::datatable(selected ,escape = FALSE,rownames= FALSE,
                      options = list(
                      scrollX = TRUE,  
                      pageLength = 5,
                      initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color':'green'});",
                             "$(this.api().table().body()).css({'color':'red'});",
                             
                            "}")
     
                      )
     ) %>% 
       formatStyle(
            'worth_billions',
            backgroundColor = styleInterval(50, c('gray', 'yellow'))
          )
  
   })
  
# 2018 dataset country plot
temp_2018 <- mydata_2018 %>%
       group_by(
         country,
         gender
       ) %>%
       summarise(
         count=n(),
         total_networth = sum(worth_billions)
       ) %>%
       ungroup()
temp_2018 <- temp_2018[order(-temp_2018$count),][1:25,]
     
#2018 data- countrywise- top25     
output$new2018 <- renderPlot({
       
       ggplot(data=temp_2018,aes(reorder(country,count),count,fill=gender)) +
         geom_col(position='dodge') +
         
         geom_text(aes(label=count), vjust=-0.3, size=3.5)+
         scale_y_continuous(breaks=seq(0,600,50)) +
         labs(
           #title = "\n    Billionaires count in different Countries   " ,
           x =" Country " ,y = "Count of Billionaires " , fill = "Gender") +
         
         theme(
           plot.title = element_text(color="black", size=14, face="bold.italic"),
           plot.background  = element_rect(fill = "lightblue") ,
           
           panel.background = element_rect(color = "grey50"),
           panel.ontop = FALSE ,
           
           axis.title.x = element_text(color="black", size=14, face="bold"),
           axis.title.y = element_text(color="black", size=14, face="bold"),
           
           axis.text.x = element_text(hjust = 1,colour = "black",size=12,face="bold" ),
           axis.text.y = element_text(hjust = 1,colour = "black",size=12,face="bold" ),
           
           legend.box.background = element_rect(),
           legend.text = element_text(face = "bold",size=12,colour = "black"),
           legend.box.margin = margin(6, 6, 6, 6),
           legend.title =  element_text(face="bold",colour="black") 
           
         )+
         coord_flip()
     })
     
})