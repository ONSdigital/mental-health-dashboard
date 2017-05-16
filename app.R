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
  fluidRow(column(1),
           column(10, h1("Prevalence of Common Mental Health Disorders among people aged 16 to 74,\n in England, by NHS Region, 2014/15")),
            column(1)),
  fluidRow(sidebarPanel(
    selectInput('region', 'Please select an NHS region', model_outputs[[2]]$Parent.Name))),
  fluidRow(column(5, plotOutput("map", height = "600")),
           column(7, plotOutput("chart", height = "500", width = "900"))),
  fluidRow(column(1),
           column(10,(h2(textOutput("narrative")))),
          column(1)),
  fluidRow(column(1),
           column(10, h3("For more information on this dataset click",
                         a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata.md", target="_blank"), ".")),
           column(1)))
)

server <- function(input, output) {
  output$map <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs[[1]], input$region)
  })
  output$chart <- renderPlot({
    create_barchart_of_prevalence_by_region(model_outputs[[2]], model_outputs[[3]], input$region)
  })
  output$narrative <- renderText({narrative_by_region <- create_narrative(narrative, input$region)})
}

shinyApp(ui = ui, server = server)
