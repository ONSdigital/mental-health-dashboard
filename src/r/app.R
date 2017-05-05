#install.packages("shiny")
library(shiny)
source("src/r/model.R")

ui <- shinyUI(fluidPage(
  fluidRow(h1("This is the page layout a user would see after clicking on a map region")),
  fluidRow(column(width = 4, style = div(style = "height:100px;"), plotOutput("map", width = 1000)),
    column(width = 5, offset = 1, div(style = "height:100px;"), plotOutput("chart", width = 1000)),
  fluidRow(column(width = 12, style = "background-color:white;", div(style = "height:200px;"))), #creates white space
  fluidRow(column(width = 12, style = "background-color:blue;", div(style = "height:200px;"), h1("pull in metadata here"))
))))

server <- function(input, output) {
  output$map <- renderPlot(height = 600, {
    choropleth_map_prevalence_by_NHS_Region <- create_choropleth_map_by_prevalence(model_outputs[[1]])
    })
#  currently second output doesn't work
  output$chart <- renderPlot(height = 1000, {
   create_barchart_of_prevalence_by_region(model_outputs[[2]], model_outputs[[3]], region)
  })
  }

shinyApp(ui = ui, server = server)
