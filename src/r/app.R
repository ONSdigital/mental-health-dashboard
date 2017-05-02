#install.packages("shiny")
library(shiny)
source("resources/r/model.r")

ui <- shinyUI(fluidPage(
  fluidRow(h1("This the page layout a user would see after clicking on a map region")),
  fluidRow(column(width = 4, style = div(style = "height:100px;"), plotOutput("map", width = 1000)),
    column(width = 5, offset = 1, div(style = "height:100px;"), plotOutput("chart", width = 1000)),
  fluidRow(column(width = 12, style = "background-color:white;", div(style = "height:200px;"))), #creates white space
  fluidRow(column(width = 12, style = "background-color:blue;", div(style = "height:200px;"), h1("pull in metadata here"))
))))

server <- function(input, output) {
  output$map <- renderPlot(height = 600, {
    create_choropleth_map_by_prevalence(region_shapefile_with_joined_prevalence_data)}) ### Needs to take 'joined' dataset
  output$chart <- renderPlot(height = 1000 {
    create_barchart_of_prevalence_by_region(regional_prevalence_with_ranks, england_prevalence)
  })
  }

shinyApp(ui = ui, server = server)



