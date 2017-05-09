#install.packages("shiny")
library(shiny)
source("src/r/model.R")

ui <- shinyUI(fluidPage(
  fluidRow(h2("Prevalence of Common Mental Health Disorders among people aged 16 to 74,\n in England, by NHS Region, 2014/15")),
  fluidRow(column(width = 6, plotOutput("map", height = 800)),
           column(width = 6, plotOutput("chart", height = 800))),
  fluidRow(h2(textOutput("narrative"))),
  fluidRow(h3("For more information on this dataset click", a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/Data/Metadata.md", target="_blank"), "."))))


server <- function(input, output) {
  output$map <- renderPlot( {
    choropleth_map_prevalence_by_NHS_Region <- create_choropleth_map_by_prevalence(model_outputs[[1]])
  })
  output$chart <- renderPlot({
    create_barchart_of_prevalence_by_region(model_outputs[[2]], model_outputs[[3]], region)
  })
  output$narrative <- renderText({narrative_by_region <- create_narrative(narrative, region)})
}

shinyApp(ui = ui, server = server)

