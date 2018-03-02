#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(" World Billionaires - By Forbes - 2017 "),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
       tags$h3("Total Billionaires - ",nrow(b_data)),
       tags$img(height=100,
                width=150,
                src="plot.png"),
       
       industries <- sort(unique(b_data$industry)),
       industries <- c("All",industries),
       gender <-unique(b_data$gender),
       gender <- c("All",gender),
       gender <- gender[-c(4)],
       countries <- sort(unique(b_data$country)),
       countries <- c("All",countries),
      # images <- billionaires_data$squareImage,
       
      selectInput(inputId = "gen",
                  label  = "Gender :",
                  choices = gender ,
                  selected = "All"
                  
      ), 
      
       selectInput(inputId="ag",
                   label = "Select the age grp:",
                   choices = c( "All",
                                "less than 20",
                                "20-40",
                                "40-60",
                                "60-80",
                                "80-110"
                                  ),
                   selected = "All"
                     ),
       
        #select if you want diff options 
       checkboxInput(inputId="checkbox",
                     label = "Diff Options",
                     value = FALSE
                     
                     ),
      
      #display only if checkbos is checked
      conditionalPanel(condition="input.checkbox == true",
                       selectInput(inputId = "country",
                                   label   = "Select the Country:",
                                   choices = countries,
                                   selected = "All"
                         
                       )
        
        
      ),
       actionButton(inputId = "button",
                    label   =  "show"
         
                    )
       
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(type="tabs",
                   tabPanel("Industry Wise Plot",plotOutput("industryPlot")),
                   tabPanel("Country Wise Plot",plotOutput("countryPlot")),
                   tabPanel("Source Wise Plot",plotOutput("sourcePlot")),
                   tabPanel("Data",DT::dataTableOutput("table"))
                   #tabPanel("Images",imageOutput("img"))
                    
         
             )
       
    )
  )
))
