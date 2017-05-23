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

ui <- shinyUI(
  fluidPage(
    titlePanel("Mental Health Dashboard"),
    
    mainPanel(
      column(12,
      tabsetPanel(
        
        tabPanel( "Mental Health Prevalence",
                  fluidRow(h1("Prevalence of Common Mental Health Disorders among people aged 16 to 74,\n in England, by NHS Region, 2014/15")),
                  fluidRow(sidebarPanel( 
                    tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
                  selectInput('region', label = h3('Please select an NHS region'), model_outputs[[2]]$Parent.Name))),
                  fluidRow(plotOutput("chart1")),
                  fluidRow(plotOutput("map1")),
                  fluidRow(h2(textOutput("narrative1"))),
                  fluidRow(h3("For more information on this dataset click",
                     a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata.md", target="_blank"), "."))
        ),
        
        tabPanel("Depression prevalence", 
                 fluidRow(h1(" Percentage of patients on GP practice register, aged 18+, recorded as having depression,\n in England, by NHS Region, 2014/15")),
                 fluidRow(sidebarPanel( 
                   tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
                   selectInput('region', label = h3('Please select an NHS region'), model_outputs[[2]]$Parent.Name))),
                 fluidRow(plotOutput("chart2")),
                 fluidRow(plotOutput("map2")),
                 fluidRow(h2(textOutput("narrative2"))),
                 fluidRow(h3("For more information on this dataset click",
                             a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata.md", target="_blank"), "."))
                 
        ),
        
        tabPanel("Depression review",
                 fluidRow(h1("Percentage of newly diagnosed patients with depression, aged 18+, who had a review 10-56 days after diagnosis,\n in England, by NHS Region, 2014/15")),
                 fluidRow(sidebarPanel( 
                   tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
                   selectInput('region', label = h3('Please select an NHS region'), model_outputs[[2]]$Parent.Name))),
                 fluidRow(plotOutput("chart3")),
                 fluidRow(plotOutput("map3")),
                 fluidRow(h2(textOutput("narrative3"))),
                 fluidRow(h3("For more information on this dataset click",
                             a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata.md", target="_blank"), "."))
                
           )
        
        )
      
      ),
    width = 12)
    )
  
  )
                  
 
  

  

server <- function(input, output) {
  output$map1 <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs1[[1]], input$region)
  })
  output$chart1 <- renderPlot({
    create_barchart_of_prevalence_by_region(model_outputs1[[2]], model_outputs1[[3]], input$region)
  })
  output$narrative1 <- renderText({create_narrative(model_outputs1, input$region)})
  
  output$map2 <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs2[[1]], input$region)
  })
  output$chart2 <- renderPlot({
    create_barchart_of_prevalence_by_region(model_outputs2[[2]], model_outputs2[[3]], input$region)
  })
  output$narrative2 <- renderText({create_narrative(model_outputs2, input$region)})
  
  output$map3 <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs3[[1]], input$region)
  })
  output$chart3 <- renderPlot({
    create_barchart_of_prevalence_by_region(model_outputs3[[2]], model_outputs3[[3]], input$region)
  })
  output$narrative3 <- renderText({create_narrative(model_outputs3, input$region)})
}

shinyApp(ui = ui, server = server)
