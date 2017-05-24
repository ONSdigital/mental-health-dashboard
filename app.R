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

format_tab <- function(title, header, region_no, map_no, chart_no, narrative_no, metadata_url_no) {
  tabPanel(title, (tags$style(type='text/css', 
                              ".nav-tabs {font-size: 20px} ")),
           fluidRow(column(1), column( 10,h1(header)),column(1)), 
           fluidRow(column(1), column( 10,sidebarPanel( 
             tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
             selectInput(region_no, label = h3('Please select an NHS region'), model_outputs1[[2]]$Parent.Name)))),
           fluidRow(column(5, plotOutput(map_no,height = "700")),
                    (column(7, plotOutput(chart_no, height = "500", width = "1100")))),
           fluidRow(column(1), column(10, h2(textOutput(narrative_no)))), column(1),
           fluidRow(column(1), column(10, h3("For more information on this dataset click",
                                             a("here", href= metadata_url_no, target="_blank"), "."),column(1))
           ))
}

comparison_tab <- function (title, header, region_no, chart1_no, chart2_no, chart3_no) {
  tabPanel(title,(tags$style(type='text/css', 
                                                ".nav-tabs {font-size: 20px} ")),
           fluidRow(column(1), column( 10,h1(header)),column(1)), 
           fluidRow(column(1), column( 10,sidebarPanel( 
             tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
             selectInput(region_no, label = h3('Please select an NHS region'), model_outputs1[[2]]$Parent.Name)))),
           fluidRow (column(6, plotOutput(chart1_no, width = "900")),
                     (column(6, plotOutput(chart2_no, width = "900")))),
           fluidRow(column(1)),
           fluidRow(column(1)),
           fluidRow(column(2), (column(10, plotOutput(chart3_no, width = "1100")))),
           fluidRow (column(10, h3("For more information on these datasets please see the metadata link in the relevant tabs."))
           ))
}

ui <- shinyUI(
  fluidPage(
    titlePanel("Mental Health Dashboard"),
    
    mainPanel(
      column(12,
             tabsetPanel(
               
               format_tab("Prevalence of Common Mental Health Disorders ", 
                          "Prevalence of Common Mental Health Disorders among people aged 16 to 74,\n in England, by NHS Region, 2014/15",
                          "region1",
                          "map1",
                          "chart1",
                          "narrative1",
                          "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata1.md"),
               
               format_tab("Prevalence of Depression",
                          "Percentage of patients on GP practice register, aged 18+, recorded as having depression,\n in England, by NHS Region, 2014/15",
                          "region2",
                          "map2",
                          "chart2",
                          "narrative2",
                          "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata2.md"),
               
               format_tab("Depression Follow-Up Reviews",
                          "Percentage of newly diagnosed patients with depression, aged 18+, who had a review 10-56 days after diagnosis,\n in England, by NHS Region, 2014/15",
                          "region3",
                          "map3",
                          "chart3",
                          "narrative3",
                          "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata3.md"),
               
              comparison_tab("Regional Comparisons",
                             "A comparison of different mental health indicators \n across NHS Regions in England, 2014/15",
                             "regioncompare",
                             "chartcompare1",
                             "chartcompare2",
                             "chartcompare3"),
              
              format_tab("Age-Standardised Suicide Rates",
                         "Age-standardised suicide rates per 100,000 population, by NHS Region \n in England, 2015 death registrations",
                         "region4",
                         "map4",
                         "chart4",
                         "narrative4",
                         "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata4.md")
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
  
  output$chartcompare1 <- renderPlot({
    create_barchart_of_MH_prevalence_by_region(model_outputs1[[2]], model_outputs1[[3]], input$regioncompare)
  })
  output$chartcompare2 <- renderPlot({
    create_barchart_of_depression_prevalence_by_region(model_outputs2[[2]], model_outputs2[[3]], input$regioncompare)
  })
  output$chartcompare3 <- renderPlot({
    create_barchart_of_depression_review_by_region(model_outputs3[[2]], model_outputs3[[3]], input$regioncompare)
  })
  
  output$map4 <- renderPlot( {
    create_choropleth_map_of_rate(model_outputs4[[1]], input$region4)
  })
  output$chart4 <- renderPlot({
    create_barchart_of_suicide_rates_by_region(model_outputs4[[2]], model_outputs4[[3]], input$region4)
  })
  output$narrative4 <- renderText({create_narrative4(model_outputs4, input$region4)})
}

shinyApp(ui = ui, server = server)
