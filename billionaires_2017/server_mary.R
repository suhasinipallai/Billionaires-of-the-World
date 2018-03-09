
library(tidyverse)
library(magrittr)
library(shiny)
library(DT)

shinyServer(function(input, output) {
  
  output$industryPlot <- renderPlot({
    
    # save the input values
    industry_value <- input$industry
    age_value <- input$age
    
    # if-else logic to handle one or more 'All' inputs and create 'data' for plot function
    if (industry_value == 'All' & age_value == "All"){
      data <-   b_data %>%
        group_by(industry) %>% 
        summarise(count=n())
    } else if(industry_value == 'All' &  !age_value == 'All') {
      data <-    b_data %>% 
        filter(age_group == age_value) %>% 
        group_by(industry, gender) %>% 
        summarise(count =n())
    }else {
      data <- b_data %>% 
        filter(age_group == age_value & industry == industry_value) %>% 
        group_by(gender, industry) %>% 
        summarise(count =n())
    }
    
    ggplot(data,aes(reorder(industry,count),count, fill=as.factor(gender))) +
      geom_col()+
      theme(axis.text.x =element_text(angle=65,hjust = 1))         
  }) 
  
})