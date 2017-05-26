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


home_tab <- function(title, header, narrativeline1, narrativeline2, narrativeline3, narrativeline4, narrativeline5) {
  tabPanel(title, (tags$style(type='text/css', 
                              ".nav-tabs {font-size: 20px} ")),
           fluidRow(column(1), column( 10,h1(header)),column(1)),
           fluidRow(column(1), column( 10,h2(narrativeline1)),column(1)),
           fluidRow(column(1), column( 10,h2(narrativeline2)),column(1)),
           fluidRow(column(1), column( 10,h2(narrativeline3)),column(1)),
           fluidRow(column(1), column( 10,h2(narrativeline4)),column(1)),
           fluidRow(column(1), column( 10,h2(narrativeline5)),column(1)))
  
}

format_tab <- function(title, header, region_no, map_no, chart_no, narrative_no, metadata_url_no) {
  tabPanel(title, (tags$style(type='text/css', 
                              ".nav-tabs {font-size: 20px} ")),
           fluidRow(column(1), column( 10,h1(header)),column(1)), 
           fluidRow(column(1), column( 10,sidebarPanel( 
             tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
             selectInput(region_no, label = h3('Please select an NHS region'), model_outputs1[[2]]$Parent.Name[order(model_outputs1[[2]]$Parent.Name)])))),
           fluidRow(column(5, plotOutput(map_no,height = "700")),
                    (column(7, plotOutput(chart_no, height = "500", width = "1100")))),
           fluidRow(column(1), column(10, h2(textOutput(narrative_no)))), column(1),
           fluidRow(column(1), column(10, h3("For more information on this dataset click",
                                             a("here", href= metadata_url_no, target="_blank"), "."),column(1))
           ))
}

CCG_tab <- function(title, header, region_no, ccgmap) {
  tabPanel(title, (tags$style(type='text/css', 
                              ".nav-tabs {font-size: 20px} ")),
           fluidRow(column(1), column( 10,h1(header)),column(1)), 
           fluidRow(column(1), column( 10,sidebarPanel( 
             tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
             selectInput(region_no, label = h3('Please select a CCG'), model_outputs6$Area.Name[order(model_outputs6$Area.Name)])))),
           fluidRow(plotOutput(ccgmap, height = 1000 )))
}

timeseries_tab <- function(title, header, region_no, timeseries) {
  tabPanel(title, (tags$style(type='text/css', 
                              ".nav-tabs {font-size: 20px} ")),
           fluidRow(column(1), column( 10,h1(header)),column(1)), 
           fluidRow(column(1), column( 10,sidebarPanel( 
             tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
             selectInput(region_no, label = h3('Please select an NHS region'), model_outputs1[[2]]$Parent.Name[order(model_outputs1[[2]]$Parent.Name)])))),
           fluidRow(column(1), plotOutput(timeseries, height=600, width = 1900 )))
}

comparison_tab <- function (title, header, region_no, chart1_no, chart2_no, chart3_no, chart4_no) {
  tabPanel(title,(tags$style(type='text/css', 
                             ".nav-tabs {font-size: 20px} ")),
           fluidRow(column(1), column( 10,h1(header)),column(1)), 
           fluidRow(column(1), column( 10,sidebarPanel( 
             tags$style(type='text/css', ".selectize-input { font-size: 20px;} .selectize-dropdown { font-size: 20px;}"),
             selectInput(region_no, label = h3('Please select an NHS region'), model_outputs1[[2]]$Parent.Name[order(model_outputs1[[2]]$Parent.Name)])))),
           fluidRow (column(6, plotOutput(chart4_no, width = "900")),
                     (column(6, plotOutput(chart1_no, width = "975")))),
           fluidRow (column(6, plotOutput(chart2_no, width = "900")),
                     (column(6, plotOutput(chart3_no, width = "975")))),
           fluidRow (column(10, h3("For more information on these datasets please see the metadata links in the relevant tabs."))
           ))
}

ui <- shinyUI(
  fluidPage(
    titlePanel("Mental Health Dashboard"),
    
    mainPanel(
      column(12,
             tabsetPanel(
               
               home_tab("Overview", 
                        "This dashboard presents mental health data taken from Public Health England (PHE) Fingertips tool and the Office for National Statistics (ONS). 
Read on to learn about England overall, or click the tabs above to explore regional breakdowns.",
                        "In 2013/14 the spending on mental health in England was £145.80 per 1,000 population.",
                        "In 2014/15 the overall prevalence of common mental disorders in England was 15.6%.",
                        "In 2014/15 the percentage of patients on GP practice register recorded as having depression in England was 7.3%.",
                        "In 2014/15 the percentage of newly diagnosed patients with depression who had a review 10-56 days after diagnosis in England was 63.8%.",
                        "In 2015 the age-standardised suicide-rate in England was 10.1 per 100,000 population."
                        ),
               
               
               format_tab("Spending on Mental Health 2013/14", 
                          "Spending on mental health per 1,000 population, by NHS Region in England, 2013/14",
                          "region5",
                          "map5",
                          "chart5",
                          "narrative5",
                          "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata5.md"),
               
               format_tab("Prevalence of Common Mental Health Disorders (NHS region)", 
                          "Prevalence of Common Mental Health Disorders among people aged 16 to 74,\n in England, by NHS Region, 2014/15",
                          "region1",
                          "map1",
                          "chart1",
                          "narrative1",
                          "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata1.md"),
               
               CCG_tab("Prevalence of Common Mental Health Disorders (CCG)", 
                       "Prevalence of Common Mental Health Disorders among people aged 16 to 74,\n in England, by Clinical Commissioning Group, 2014/15",
                       "region6",  
                       "ccgmap"),
               
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
                              "A comparison of spending on mental health in 2013/14 and mental health indicators in 2014/15, across NHS Regions in England ",
                              "regioncompare",
                              "chartcompare1",
                              "chartcompare2",
                              "chartcompare3",
                              "chartcompare4"),
               
               format_tab("Age-Standardised Suicide Rates",
                          "Age-standardised suicide rates per 100,000 population, by NHS Region \n in England, 2015 death registrations",
                          "region4",
                          "map4",
                          "chart4",
                          "narrative4",
                          "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata4.md"),
               
              
               timeseries_tab("Suicides time series",
                          "Age-Standardised Suicide rates per 100,000 population, by NHS Region \n in England 2006-2016 death registrations",
                          "region7",
                          "suicidestimeseries"
                          )

             )
             
      ),
      width = 12)
  )
  
)






server <- function(input, output) {
  output$ccgmap <- renderPlot( {
    create_choropleth_map_CCG(model_outputs6, input$region6)
  })
  output$map1 <- renderPlot( {
    create_choropleth_map_by_prevalence_purple(model_outputs1[[1]], input$region1)
  })
  output$chart1 <- renderPlot({
    create_barchart_of_MH_prevalence_by_region(model_outputs1[[2]], model_outputs1[[3]], input$region1)
  })
  output$narrative1 <- renderText({create_narrative1(model_outputs1, input$region1)})
  
  output$map2 <- renderPlot( {
    create_choropleth_map_by_prevalence_green(model_outputs2[[1]], input$region2)
  })
  output$chart2 <- renderPlot({
    create_barchart_of_depression_prevalence_by_region(model_outputs2[[2]], model_outputs2[[3]], input$region2)
  })
  output$narrative2 <- renderText({create_narrative2(model_outputs2, input$region2)})
  
  output$map3 <- renderPlot( {
    create_choropleth_map_by_prevalence_orange(model_outputs3[[1]], input$region3)
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
  
  output$chartcompare4 <- renderPlot({
    create_barchart_of_MH_spending_by_region(model_outputs5[[2]], model_outputs5[[3]], input$regioncompare)
  })
  
  output$map4 <- renderPlot( {
    create_choropleth_map_of_rate(model_outputs4[[1]], input$region4)
  })
  output$chart4 <- renderPlot({
    create_barchart_of_suicide_rates_by_region(model_outputs4[[2]], model_outputs4[[3]], input$region4)
  })
  output$narrative4 <- renderText({create_narrative4(model_outputs4, input$region4)})
  
  output$map5 <- renderPlot( {
    create_choropleth_map_of_spending(model_outputs5[[1]], input$region5)
  })
  output$chart5 <- renderPlot({
    create_barchart_of_MH_spending_by_region(model_outputs5[[2]], model_outputs5[[3]], input$region5)
  })
  output$narrative5 <- renderText({create_narrative5(model_outputs5, input$region5)})
  
  output$suicidestimeseries <- renderPlot( {
    create_suicide_time_series(reshaped_suicide_data, input$region7)})
}

shinyApp(ui = ui, server = server) 
