library(shiny)
library(DT)
library(shinythemes)

#b_data <- readRDS('./Data/b_data.rds')

#saveRDS(b_data, './Data/b_data.rds')
# Adding All to the UI variables

industries <- sort(unique(b_data$industry))
industries <- c("All",industries)
gender <-unique(b_data$gender)
gender <- c("All",gender)
countries <- sort(unique(b_data$country))
countries <- c("All",countries)
ages <- sort(unique(b_data$age_group))
ages <- c("All",ages)

# Define UI for application 
shinyUI(fluidPage(theme = shinytheme("cyborg"),
  
  # Application title
  titlePanel(" World Billionaires - By Forbes - 2017 "),
  
  # Sidebar with a selectInput for various options 
  sidebarLayout(
    sidebarPanel(
      
       
       # adding the logo 
       
       tags$img(height=100,
                 width =100,
                 src="coin.png"),
       
       #HTML(paste0("The dataset has ",nrow(b_data)," observations.")),
       
       #adding UI
   
       selectInput(inputId = "gender_var",
                  label  = "Gender :",
                  choices = gender ,
                  selected = "All"
                   ), 
      
       selectInput(inputId="age_var",
                   label = "Age Group:",
                   choices = ages,
                   selected = "All"
                   ),
      
       selectInput(inputId = "country_var",
                   label = "Country:",
                   choices= countries,
                   selected = "All"
                   ) 
      , width = 3
       
    
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       
      tabsetPanel(type="tabs",
                  
                   tabPanel("Country-wise Plot",
                           plotOutput("countryPlot")),
                   
                   tabPanel("Industry-wise Plot",
                             plotOutput("industryPlot")),
  
                   tabPanel("Source-wise Plot",
                            plotOutput("sourcePlot")),
                  
                   tabPanel("Table",
                            DT::dataTableOutput("mytable1")),
                   
                   tabPanel("2018",
                            plotOutput("new2018"))
                   
                 )
       ,width=9
    )
  )
))
