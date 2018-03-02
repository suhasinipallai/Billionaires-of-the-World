#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
        
        
        if(g == 'All'){
           b_data %>% 
            filter(gender)
        }
        
        
        industry_summary_all <- b_data %>% 
                         # filter(!age==0 & !age==500 & !gender=='unknown') %>% 
                          group_by(industry) %>% 
                          summarize(count = n()
                           )
       
       country_summary_all <- b_data %>% 
                          # filter(!age==0 & !age==500 & !gender=='unknown') %>% 
                           group_by(country) %>% 
                           summarize(count = n())
       
       source_networth_summary_all <-b_data %>% 
                          #  filter(!age==0 & !age==500 & !gender=='unknown') %>% 
                            group_by(source) %>% 
                            summarise(count = n(),
                                      sum_networth = sum(worth_billions))
       
       selected_options <- reactive({
                req(input$checkbox)
                billionaires_data %>% 
                  filter(gender==input$gen,age==input$ag,country==input$country) 
                   })
       
  
#   output$barPlot <- renderPlot({
#          billionaires_data %>%  
#                    filter(.,rank >=input$rank) %>% 
#                     select(rank,name,source,industry,age,worth) %>% 
              # draw the histogram with the specified number of bins
#          ggplot(aes(x=rank,y=worth),col = 'darkgray', border = 'white') +
#             geom_col()
    #  })
       
  # output$industryPlot <- renderPlot({
  #   
  #               industry_summary_all %>% 
  #                filter(input$gen == "All" & input$ag == 'All') %>% 
  #     ggplot(aes(reorder(industry,count),count)) +
  #     geom_col(color="blue",fill="red")+
  #     theme(axis.text.x =element_text(angle=65,hjust = 1))
  #   
  #   
  # })  
       
 output$industryPlot <- renderPlot({
   
              gender_input <- input$gen
              age_input <- input$ag
              country_input <- input$country       
   
         if (gender_input == 'All' & age_input == "All" & country_input=='All'){
            
            fil_select <- b_data %>%
                       group_by(industry) %>% 
                       summarise(count=n())
          } else {
            
          fil_select <- b_data %>% 
                        filter(
                          gender==gender_input &
                          country ==country_input
                        ) %>% 
                        group_by(industry,gender) %>% 
                        summarise(count =n())
          }
              
          ggplot(fil_select,aes(reorder(industry,count),count,color=gender)) +
                geom_col(color="blue",fill="red")+
               theme(axis.text.x =element_text(angle=65,hjust = 1))     
          
 })  
  
  
  # 
  # output$countryPlot <- renderPlot({
  #                  country_summary_all %>% 
  #                     filter(input$gen == "All" & input$ag == 'All') %>% 
  #                     filter(count>=10) %>% 
  #                     ggplot(aes(reorder(country,count),count,label=count)) +
  #                     geom_col(fill="lightblue") +
  #                     geom_text(position=position_stack())+
  #                     theme(axis.text.x=element_text(angle=65,hjust=1))+
  #                     coord_flip() 
  #   
  # })
  
 output$countryPlot <- renderPlot({
                     
                       gender_input <- input$gen
                       age_input <- input$ag
                       country_input <- input$country       
                   
       if (gender_input == 'All' & age_input == "All" & country_input == 'All'){
      
                          fil_select <- b_data %>%
                          group_by(country) %>% 
                          summarise(count=n())
                        } else {
     
                       fil_select <- b_data %>% 
                       filter(
                              gender == gender_input &
                              country ==country_input
                             ) %>% 
                      group_by(country) %>% 
                      summarise(count =n())
                    }
   
             ggplot(fil_select,aes(reorder(country,count),count,label=count)) +
                     geom_col(fill="lightblue") +
                     geom_text(position=position_stack())+
                     theme(axis.text.x=element_text(angle=65,hjust=1))+
                     coord_flip() 
                       
 })
 
  
  output$sourcePlot <- renderPlot({
                     source_networth_summary_all %>% 
                       filter(input$gen == "All" & input$ag == 'All') %>%     
                       filter(sum_networth >=50) %>% 
                       ggplot(aes(source,count,size=sum_networth)) +
                       geom_point()+
                       theme(axis.text.x=element_text(angle=65,hjust=1))
    
  })
  
  output$table <- DT::renderDataTable({
    
              b_data %>% 
                  filter(input$gen == "All" & input$ag == 'All') %>% 
                  filter(!age==0 & !age==50 & !age=='unknown') %>% 
                #  filter(rank>= input$rank[1] & rank <= input$rank[2]) %>% 
                  select(rank,name,lastName,source,industry,worth_billions,age) 
    
  })
  
  
  output$industryPlot_q <- renderPlot({
    industry_summary_all %>% 
         indus()
      #filter(gender==input$gen,age==input$ag) %>% 
      #ggplot(aes(reorder(industry,count),count))
  })
    
  indus <- eventReactive(input$button ,{
        #  print(as.numeric(input$button))
          industry_summary_all %>% 
      filter(gen == input$gen) %>% 
      ggplot(aes(reorder(industry,count),count)) +
      geom_col(color="blue",fill="red")+
      theme(axis.text.x =element_text(angle=65,hjust = 1))
    
  })
   
 
})
