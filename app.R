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
                         fluidRow(column(1), column( 10,h1("Prevalence of Common Mental Health Disorders among people aged 16 to 74,\n in England, by NHS Region, 2014/15")),
                                  column(1)),
                         fluidRow(column(1), column( 10,sidebarPanel( 
                           tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
                           selectInput('region1', label = h3('Please select an NHS region'), model_outputs1[[2]]$Parent.Name)))),
                         fluidRow(column(5, plotOutput("map1",height = "700")),
                                  (column(7, plotOutput("chart1", height = "500", width = "900")))),
                         fluidRow(column(1), column(10, h2(textOutput("narrative1")))), column(1),
                         fluidRow(column(1), column(10, h3("For more information on this dataset click",
                                                           a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata1.md", target="_blank"), "."),column(1))
                         )),
               
               tabPanel("Depression prevalence", 
                        fluidRow(column(1), column( 10,h1("Percentage of patients on GP practice register, aged 18+, recorded as having depression,\n in England, by NHS Region, 2014/15")),
                                 column(1)),
                        
                        fluidRow(column(1), column( 10,sidebarPanel( 
                          tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
                          selectInput('region2', label = h3('Please select an NHS region'), model_outputs1[[2]]$Parent.Name)))),
                        fluidRow(column(5, plotOutput("map2",height = "700")),
                                 (column(7, plotOutput("chart2", height = "500", width = "900")))),
                        fluidRow(column(1), column(10, h2(textOutput("narrative2")))), column(1),
                        fluidRow(column(1), column(10, h3("For more information on this dataset click",
                                                          a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata2.md", target="_blank"), "."),column(1))
                                 
                        )),
               
               tabPanel("Depression review", 
                        fluidRow(column(1), column( 10,h1("Percentage of newly diagnosed patients with depression, aged 18+, who had a review 10-56 days after diagnosis,\n in England, by NHS Region, 2014/15")),
                                 column(1)),
                        fluidRow(column(1), column( 10,sidebarPanel( 
                          tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
                          selectInput('region3', label = h3('Please select an NHS region'), model_outputs1[[2]]$Parent.Name)))),
                        fluidRow(column(5, plotOutput("map3",height = "700")),
                                 (column(7, plotOutput("chart3", height = "500", width = "1000")))),
                        fluidRow(column(1), column(10, h2(textOutput("narrative3")))), column(1),
                        fluidRow(column(1), column(10, h3("For more information on this dataset click",
                                                          a("here", href= "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata3.md", target="_blank"), "."),column(1))
                                 
                        )
               )
             )
             
      ),
      width = 12)
  )
  
)






server <- function(input, output) {
  output$map1 <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs1[[1]], input$region1)
  })
  output$chart1 <- renderPlot({
    create_barchart_of_MH_prevalence_by_region(model_outputs1[[2]], model_outputs1[[3]], input$region1)
  })
  output$narrative1 <- renderText({create_narrative1(model_outputs1, input$region1)})
  
  output$map2 <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs2[[1]], input$region2)
  })
  output$chart2 <- renderPlot({
    create_barchart_of_depression_prevalence_by_region(model_outputs2[[2]], model_outputs2[[3]], input$region2)
  })
  output$narrative2 <- renderText({create_narrative2(model_outputs2, input$region2)})
  
  output$map3 <- renderPlot( {
    create_choropleth_map_by_prevalence(model_outputs3[[1]], input$region3)
  })
  output$chart3 <- renderPlot({
    create_barchart_of_depression_review_by_region(model_outputs3[[2]], model_outputs3[[3]], input$region3)
  })
  output$narrative3 <- renderText({create_narrative3(model_outputs3, input$region3)})
}

shinyApp(ui = ui, server = server)
