#install.packages("shiny")
#install.packages("backports")
#install.packages("caTools")
#install.packages("rmarkdown")
#install.packages("rprojroot")
library(shiny)
library(backports)
library(caTools)
library(rmarkdown)
library(rprojroot)
source("src/r/model.R")

ui <- shinyUI(fluidPage(
  fluidRow(column(12, offset = 0.5, h2("Prevalence of Common Mental Health Disorders among people aged 16 to 74,\n in England, by NHS Region, 2014/15"))),
  fluidRow(h2("")),
  fluidRow(column(4, plotOutput("map", height = "600")),
           column(8, plotOutput("chart", height = "500", width = "900"))),
  fluidRow(h2(textOutput("narrative"))),
  fluidRow(h3("For more information on this dataset click", a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata.md", target="_blank"), "."))))


server <- function(input, output) {
  output$map <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs[[1]])
  })
  output$chart <- renderPlot({
    create_barchart_of_prevalence_by_region(model_outputs[[2]], model_outputs[[3]], region)
  })
  output$narrative <- renderText({narrative_by_region <- create_narrative(narrative, region)})
}

shinyApp(ui = ui, server = server)

