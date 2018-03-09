library(shiny)

ui <- fluidPage(

    sliderInput(inputId = "slid",
                label = "Select number range",
                min = 1,
                max=10,
                value= 3,
                step = 1
              ),
    plotOutput("hist")
  
  
  )

server <- function(input,output){
  output$hist <- renderPlot({
       hist(rnorm(input$slid))
  })
  
}

shinyApp(ui= ui , server=server)