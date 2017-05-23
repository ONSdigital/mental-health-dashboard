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
                  fluidRow(plotOutput("chart")),
                  fluidRow(plotOutput("map")),
                  fluidRow(h2(textOutput("narrative"))),
                  fluidRow(h3("For more information on this dataset click",
                     a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata.md", target="_blank"), "."))
        ),
        
        tabPanel("Depression prevalence", 
                 fluidRow(h1(" Percentage of patients on GP practice register, aged 18+, recorded as having depression,\n in England, by NHS Region, 2014/15")),
                 fluidRow(sidebarPanel( 
                   tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
                   selectInput('region', label = h3('Please select an NHS region'), model_outputs[[2]]$Parent.Name)))
                 
        ),
        
        tabPanel("Depression review",
                 fluidRow(h1("Percentage of newly diagnosed patients with depression, aged 18+, who had a review 10-56 days after diagnosis,\n in England, by NHS Region, 2014/15")),
                 fluidRow(sidebarPanel( 
                   tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
                   selectInput('region', label = h3('Please select an NHS region'), model_outputs[[2]]$Parent.Name)))
                
           )
        
        )
      
      ),
    width = 12)
    )
  
  )
                  
 
  

  

server <- function(input, output) {
  output$map <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs[[1]], input$region)
  })
  output$chart <- renderPlot({
    create_barchart_of_prevalence_by_region(model_outputs[[2]], model_outputs[[3]], input$region)
  })
  output$narrative <- renderText({create_narrative(model_outputs, input$region)})
}

shinyApp(ui = ui, server = server)
