#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("backports")
#install.packages("caTools")
#install.packages("rmarkdown")
#install.packages("rprojroot")

library(shiny)
library(shinydashboard)
library(backports)
library(caTools)
library(rmarkdown)
library(rprojroot)
source("src/r/model.R")

format_tab <- function(title_no, region_no, map_no, chart_no, narrative_no, metadata_url_no) {
  fluidRow(
    box(h3(title_no), width=12),
    box(plotOutput(map_no, height = 400, width = 450), width=5, height=420),
    box(selectInput(region_no, h4("Please select a region"),
                    choices = model_outputs1[[2]]$Parent.Name[order(model_outputs1[[2]]$Parent.Name)])
        , width=4),
    box(h4(textOutput(narrative_no), width = 4, height=200)),
    box(plotOutput(chart_no, height = 400, width = 1100), width=12),
    box(h3("For more information on this dataset click",
                                      a("here", href= metadata_url_no, target="_blank"), "."))
             )
}

CCG_tab <- function(title_no,region_no, ccgmap) {
  fluidRow(
    box(h3(title_no), width=12),
    box(selectInput(region_no, h4("Please select a CCG"), 
                           choices = model_outputs6$Area.Name[order(model_outputs6$Area.Name)]),
               width = 4),
           box(plotOutput(ccgmap, height = 1000), width = 8))
}

timeseries_tab <- function(title_no, region_no, timeseries) {
  fluidRow(
    box(h3(title_no), width=12),
    box(selectInput(region_no, h4("Please select a region"),
                    choices = model_outputs1[[2]]$Parent.Name[order(model_outputs1[[2]]$Parent.Name)])
        , width=4),
    box(plotOutput(timeseries, height = 600, width = 1100), width = 12))
}

comparison_tab <-function(title1, title2, title3, title4, region_no, chart1_no, chart2_no, chart3_no, chart4_no, chart5_no, chart6_no, chart7_no, chart8_no) {
  fluidRow(
    box(selectInput(region_no, h4("Please select a region"),
                    choices = model_outputs1[[2]]$Parent.Name[order(model_outputs1[[2]]$Parent.Name)]),
        width=7),

    tabBox(
    side = "left",
    tabPanel(title1, plotOutput(chart1_no)),
    tabPanel(title2, plotOutput(chart2_no)),
    tabPanel(title3, plotOutput(chart3_no)),
    tabPanel(title4, plotOutput(chart4_no)), width = 11),

  tabBox(
    side = "left",
    tabPanel(title1, plotOutput(chart5_no)),
    tabPanel(title2, plotOutput(chart6_no)),
    tabPanel(title3, plotOutput(chart7_no)),
    tabPanel(title4, plotOutput(chart8_no)), width = 11)
  )
}

donut_tab <- function(title_no,region_no, donut1, donut2, narrative_no, metadata_url_no) {
  fluidRow(
    box(h3(title_no), width=12),
    box(plotOutput(donut1, height = 400, width = 520), width=6, height=420),
    box(plotOutput(donut2, height = 400, width = 520), width=6, height=420),
    box(h4(textOutput(narrative_no), width = 5, height=200)),
    box(selectInput(region_no, h4("Please select a region"),
                    choices = model_outputs1[[2]]$Parent.Name[order(model_outputs1[[2]]$Parent.Name)])
        , width=4),
    box(h3("For more information on this dataset click",
           a("here", href= metadata_url_no, target="_blank"), "."))
    
  )}

ethnicity_tab <- function(title_no, region_no, region_chart, England_chart, metadata_url_no){
   fluidRow(
     box(h3(title_no), width=12),
     box(selectInput(region_no, h4("Please select a region"),
                     choices = model_outputs1[[2]]$Parent.Name[order(model_outputs1[[2]]$Parent.Name)])
         , width=4),
            box(plotOutput(region_chart, width = "900"), width = 10),
            box(plotOutput(England_chart, width = "900"), width = 10),
            box(h3("For more information on this dataset click",
                                              a("here", href= metadata_url_no, target="_blank"), "."))
            
 )}

ui <- dashboardPage(
  dashboardHeader(title = "Mental Health Dashboard", titleWidth = 450),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Homepage", icon = icon("home"), tabName = "Home"),
    menuItem("CMHD Prevalence", icon = icon("dashboard"), tabName = "CMHD_Prevalence"),
    menuItem("CCG CMHD Prevalence", icon=icon("dashboard"), tabName="CCG_Prev"),
    menuItem("Depression Prevalence", icon = icon("dashboard"), tabName = "Depression_Prev"),
    menuItem("Depression Follow-up", icon=icon("dashboard"), tabName="Depression_Foll"),
    menuItem("Mental Health Spending", icon=icon("dashboard"), tabName="MH_Spend"),
    menuItem("CAMHS Reliable Improvement", icon=icon("dashboard"), tabName="CAMHS_Improv"),
    menuItem("CAMHS Spending", icon=icon("dashboard"), tabName="CAMHS_Spending"),
    menuItem("Suicide rate", icon=icon("dashboard"), tabName="Suicides"),
    menuItem("Suicide rate time series", icon=icon("dashboard"), tabName="Suicide_Time"),
    menuItem("Psychosis Waiting Times", icon=icon("dashboard"), tabName="Donut"),
    menuItem("Ethnicity", icon=icon("dashboard"), tabName="Ethnicity"),
    menuItem("Comparisons", icon=icon("window-restore"), tabName="Comparisons"))),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "Home", h3(
              
              fluidRow(box("This dashboard presents mental health data taken from Public Health England (PHE) Fingertips tool and the Office for National Statistics (ONS).
                           Read on to learn about England overall, or click the tabs on the left to explore regional breakdowns.")),
              fluidRow(box("In 2013/14 the spending on mental health in England was £145.80 per 1,000 population.")),               
              fluidRow(box("In 2014/15 the overall prevalence of common mental disorders in England was 15.6%.")),               
              fluidRow(box("In 2014/15 the percentage of patients on GP practice register recorded as having depression in England was 7.3%.")),
              fluidRow(box("In 2014/15 the percentage of newly diagnosed patients with depression who had a review 10-56 days after diagnosis in England was 63.8%.")),
              fluidRow(box("In 2015 the age-standardised suicide-rate in England was 10.1 per 100,000 population.")))),
      
      tabItem(tabName = "CMHD_Prevalence",
              format_tab("Prevalence of Common Mental Health Disorders among people aged 16 to 74, in England, by NHS Region, 2014/15", 
                         "region1", 
                         "map1", 
                         "chart1", 
                         "narrative1", 
                         "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata1.md")),
      
      tabItem(tabName = "CCG_Prev",
              CCG_tab("Prevalence of Common Mental Health Disorders among people aged 16 to 74, in England, by Clinical Commissioning Group, 2014/15", 
                      "region6", 
                      "ccgmap")),
      
      tabItem(tabName = "Depression_Prev",
              format_tab("Percentage of patients on GP practice register, aged 18+, recorded as having depression, in England, by NHS Region, 2014/15", 
                         "region2", 
                         "map2", 
                         "chart2", 
                         "narrative2", 
                         "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata2.md")),
      
      tabItem(tabName = "Depression_Foll",
              format_tab("Percentage of newly diagnosed patients with depression, aged 18+, who had a review 10-56 days after diagnosis, in England, by NHS Region, 2014/15",
                         "region3", 
                         "map3", 
                         "chart3", 
                         "narrative3", 
                         "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata3.md")),
      
      tabItem(tabName = "MH_Spend",
              format_tab("Spending on mental health per 1,000 population, by NHS Region in England, 2013/14",
                         "region5", 
                         "map5", 
                         "chart5", 
                         "narrative5", 
                         "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata5.md")),
      
      tabItem(tabName = "CAMHS_Spending",
              format_tab("Percentage of mental health spend categorised under CAMHS, by NHS Region \n in England, 2012/13",
                         "region8", 
                         "map8", 
                         "chart8", 
                         "narrative8", 
                         "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata8.md")),
      
      tabItem(tabName = "CAMHS_Improv",
              format_tab("Percentage of people who have completed IAPT treatment who achieved reliable improvement, by NHS Region \n in England, Q2 2016/17",
                         "region9", 
                         "map9", 
                         "chart9", 
                         "narrative9", 
                         "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata9.md")),
      
      tabItem(tabName = "Suicides",
              format_tab("Age-standardised suicide rates per 100,000 population, by NHS Region in England, 2015 death registrations",
                         "region4", 
                         "map4", 
                         "chart4", 
                         "narrative4", 
                         "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata4.md")),
      
      tabItem(tabName = "Suicide_Time",
              timeseries_tab("Age-Standardised suicide rates per 100,000 population, by NHS Region in England 2006-2015 death registrations",
                             "region7", 
                             "suicidestimeseries")),
      
      tabItem(tabName = "Donut",
              donut_tab("Waiting times for patients who have started treatment and those who are still waiting for treatment for Early Intervention Psychosis in England, April 2017",
                        "region10", 
                        "donut1", 
                        "donut2", 
                        "narrative10", 
                        "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata10.md")),
      
      tabItem(tabName = "Ethnicity",
              ethnicity_tab("Access to community mental health services by ethnicity, in England, by NHS regions 2014/15",
                           "region11",
                           "region_chart",
                           "England_chart",
                           "https://github.com/ONSdigital/mental-health-dashboard/blob/master/src/r/data/Metadata11.md"
                           )),
      
      tabItem(tabName = "Comparisons",

              comparison_tab("CMHD Prevalence","Depression Prevalence", "Depression Follow-up", "MH Spending", 
                             "regioncompare", "chartcompare1", "chartcompare2", "chartcompare3", "chartcompare4", 
                             "chartcompare5", "chartcompare6", "chartcompare7", "chartcompare8"))
      
    )
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
  
  output$chartcompare5 <- renderPlot({
    create_barchart_of_MH_prevalence_by_region(model_outputs1[[2]], model_outputs1[[3]], input$regioncompare)
  })
  output$chartcompare6 <- renderPlot({
    create_barchart_of_depression_prevalence_by_region(model_outputs2[[2]], model_outputs2[[3]], input$regioncompare)
  })
  output$chartcompare7 <- renderPlot({
    create_barchart_of_depression_review_by_region(model_outputs3[[2]], model_outputs3[[3]], input$regioncompare)
  })
  
  output$chartcompare8 <- renderPlot({
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
  
  output$map8 <- renderPlot( {
    create_choropleth_map_by_CAMHS_spending_GnBu(model_outputs8[[1]], input$region8)
  })
  output$chart8 <- renderPlot({
    create_barchart_of_CAMHS_spending_by_region(model_outputs8[[2]], model_outputs8[[3]], input$region8)
  })
  output$narrative8 <- renderText({create_narrative8(model_outputs8, input$region8)})
  
  output$map9 <- renderPlot( {
    create_choropleth_map_of_IAPT_improvement(model_outputs9[[1]], input$region9)
  })
  output$chart9 <- renderPlot({
    create_barchart_of_improvement(model_outputs9[[2]], model_outputs9[[3]], input$region9)
  })
  output$narrative9 <- renderText({create_narrative9(model_outputs9, input$region9)})

  output$narrative9 <- renderText({create_narrative9(model_outputs9, input$region9)
  })

  output$donut1 <- renderPlot({
    create_donut_started_treatment(psychosis_started, input$region10)
  })
  output$donut2 <- renderPlot({
    create_donut_not_started_treatment(psychosis_not_started, input$region10)
  })
  output$narrative10 <- renderText({create_narrative10(psychosis_started, psychosis_not_started, input$region10)
  })

  output$region_chart <- renderPlot({
    create_barchart_of_ethnicity_access_region(ethnicity_regions, input$region11)
  })
  output$England_chart <- renderPlot({
    create_barchart_of_ethnicity_access_England(ethnicity_England)
  })
}


shinyApp(ui, server)

