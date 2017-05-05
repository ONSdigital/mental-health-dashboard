#install.packages("shiny")
library(shiny)
source("src/r/model.R")

ui <- shinyUI(fluidPage(
  fluidRow(h1("This is the page layout a user would see after clicking on a map region")),
  fluidRow(column(width = 6, plotOutput("map", height = 800)), #need to: expand width one key moved
    column(width = 6, plotOutput("chart", height = 800))
    
  #fluidRow(column(style = "background-color:white;", div(style = "height:200px;"))), #creates white space
  #fluidRow(column(style = "background-color:blue;", div(style = "height:200px;"), h1("pull in metadata here"))
),
fluidRow(h1("narrative"))))

server <- function(input, output) {
  output$map <- renderPlot( {
    choropleth_map_prevalence_by_NHS_Region <- create_choropleth_map_by_prevalence(model_outputs[[1]])
    })
#  currently second output doesn't work
  output$chart <- renderPlot({
   create_barchart_of_prevalence_by_region(model_outputs[[2]], model_outputs[[3]], region)
  })
  }

shinyApp(ui = ui, server = server)

