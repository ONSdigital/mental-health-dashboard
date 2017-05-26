#install.packages("data.table")
#install.packages("dplyr")
#install.packages("maptools")
#install.packages("ggplot2")
#install.packages("classInt")
#install.packages("RColorBrewer")
#install.packages("testthat")
library(data.table)
library(dplyr)
library(maptools)
library(ggplot2)
library(classInt)
library(RColorBrewer)
library(testthat)

####Data
#CCG Data
CCG_prevalence <- read.csv("src/r/data/Estimated_Prevalence_of_CMDs_2014-2015.csv")
#Depression prevalence
depression_prevalence <- read.csv("src/r/data/Depression_recorded_prevalence_QOF_percentage_or_practice_register_aged_18+_2014-15.csv")
#Depression prevalence
depression_review <- read.csv("src/r/data/%_of_newly_diagnosed_patients_with_depression_who_had_a_review_10-56_days_after_diagnosis_2014-15.csv")
#Suicide rates
suicide_rates <- read.csv("src/r/data/NHS_Region_Suicides.csv")
#Spending data
CCG_spending <- read.csv("src/r/data/CCG_MH_spending.csv")
# NHS region Shapefile data
region_shapefile <- readShapePoly("src/r/data/NHS_Regions/NHS_Regions_Geography_April_2015_Super_Generalised_Clipped_Boundaries_in_England.shp")
#CCG Shapefile data
CCG_shapefile <- readShapePoly("src/r/data/CCG_Shapefiles/Clinical_Commissioning_Groups_July_2015_Super_Generalised_Clipped_Boundaries_in_England.shp")
#Suicides Time series Data
Suicides_time_series_raw <- read.csv("src/r/data/REgion_Suicide_Time_Series.csv", check.names = F)

####Model
##Prevalence datasets
#Function to aggregate prevalence to England
aggregate_prevalence_to_England <- function(prevalence_data) {
  England_count <- sum(prevalence_data$Count)
  England_pop <- sum(prevalence_data$Denominator)
  England_prevalence <- round((England_count / England_pop)*100, digits = 1)
  
  return(England_prevalence)
}

#Function to aggregate to Region
aggregate_prevalence_to_region <- function(prevalence_data) {
  regional_level_prevalence <- prevalence_data %>%
    group_by(Parent.Code, Parent.Name) %>%
    summarise(Count = sum(Count),
              Population = sum(Denominator)) %>%
    mutate(prevalence=round((Count/Population)*100, digits =1))
  
  return(regional_level_prevalence)
}

#Function to manipulate regions to match shapefile
manipulate_regions_for_shapefile <- function(region_prevalence) {
  #Combining regions to match shapefile
  removed_regions <- region_prevalence %>%
    filter(Parent.Code != "E39000037") %>%
    filter(Parent.Code != "E39000038")
  
  #Sum regions
  summed_regions <- region_prevalence %>%
    filter(Parent.Code %in% c("E39000037","E39000038")) %>%
    group_by() %>%
    summarise(Parent.Code = "E39000028",
              Parent.Name = "Lancashire and Greater Manchester",
              Count = sum(Count),
              Population = sum(Population)) %>%
    mutate(prevalence = round((Count/Population)*100, digits =1))
  
  #Add row
  thirteen_level_NHS_regional_prevalence <- summed_regions %>%
    bind_rows(removed_regions)
  
  return(thirteen_level_NHS_regional_prevalence)
}

#Add rank column/variable to dataset - Prevalence
rank_prevalence_by_region <- function(thirteen_level_NHS_regional_prevalence){
  thirteen_level_NHS_regional_prevalence$rank <- NA
  thirteen_level_NHS_regional_prevalence$rank[order(-thirteen_level_NHS_regional_prevalence$prevalence)] <- 1:nrow(thirteen_level_NHS_regional_prevalence)
  
  return(thirteen_level_NHS_regional_prevalence)
}

#join shapefile to regional prevalence data
join_prevalence_data_to_shapefile <- function(regional_prevalence_with_ranks, region_shapefile){
  regional_prevalence_with_ranks <- setnames(regional_prevalence_with_ranks, "Parent.Code", "nhsrg15cd")
  region_shapefile@data <-  region_shapefile@data %>% 
    left_join(regional_prevalence_with_ranks, by='nhsrg15cd')
  
  return(region_shapefile)
}

#Create barchart 1- Mental health prevalence
create_barchart_of_MH_prevalence_by_region <- function(regional_prevalence_with_ranks, england_prevalence, nhs_region){
  
  #Order by rank
  regional_prevalence_with_ranks$Parent.Name <- factor(regional_prevalence_with_ranks$Parent.Name, 
                                                       levels = regional_prevalence_with_ranks$Parent.Name[order(regional_prevalence_with_ranks$prevalence)])
  #Create themes for formatting text size, colour etc
  axis_labels <- element_text(face = "bold", size = 20)
  region_labels <- element_text(size = 20, hjust = 1, colour = "black")
  prevalence_labels <- element_text(size = 20, vjust = 0.2, hjust = 0.5)
  
  #Create dataframe for England average line
  england_prev <- rep(england_prevalence, length(regional_prevalence_with_ranks$Parent.Name))
  region_names <- as.vector(regional_prevalence_with_ranks$Parent.Name)
  england_prevalence_line <- data.frame(england_prev, region_names)
  
  ColourSchemeBlue <- brewer.pal(2,"Blues")
  
  #Plot
  ggplot(regional_prevalence_with_ranks, aes(x=Parent.Name, y=prevalence)) +
    coord_flip() +
    theme(axis.title = axis_labels, axis.text.x = prevalence_labels, axis.text.y = region_labels) +
    labs(x = "NHS Region", y = "Prevalence of Common Mental Disorders (%)") +
    scale_fill_manual(values = ColourSchemeBlue) +
    geom_bar(stat = "identity", colour="black", aes(fill=Parent.Name==nhs_region), show.legend = FALSE) +

    geom_line(data = england_prevalence_line, aes(x=as.numeric(region_names), y=england_prev), color = "navyblue", size = 2) +
    annotate("text", x=0.75, y= 16.25, label = "England average", color = "navyblue", size  = 7)

}

#Create barchart 2- depression prevalence
create_barchart_of_depression_prevalence_by_region <- function(regional_prevalence_with_ranks, england_prevalence, nhs_region){

  #Order by rank
  regional_prevalence_with_ranks$Parent.Name <- factor(regional_prevalence_with_ranks$Parent.Name,
                                                       levels = regional_prevalence_with_ranks$Parent.Name[order(regional_prevalence_with_ranks$prevalence)])
  #Create themes for formatting text size, colour etc
  axis_labels <- element_text(face = "bold", size = 20)
  region_labels <- element_text(size = 20, hjust = 1, colour = "black")
  prevalence_labels <- element_text(size = 20, vjust = 0.2, hjust = 0.5)

  #Create dataframe for England average line
  england_prev <- rep(england_prevalence, length(regional_prevalence_with_ranks$Parent.Name))
  region_names <- as.vector(regional_prevalence_with_ranks$Parent.Name)
  england_prevalence_line <- data.frame(england_prev, region_names)

  ColourSchemeBlue <- brewer.pal(2,"Blues")

  #Plot
  ggplot(regional_prevalence_with_ranks, aes(x=Parent.Name, y=prevalence)) +
    coord_flip() +
    theme(axis.title = axis_labels, axis.text.x = prevalence_labels, axis.text.y = region_labels) +
    labs(x = "NHS Region", y = "Prevalence of depression (%)") +
    scale_fill_manual(values = ColourSchemeBlue) +
    geom_bar(stat = "identity", colour="black", aes(fill=Parent.Name==nhs_region), show.legend = FALSE) +

    geom_line(data = england_prevalence_line, aes(x=as.numeric(region_names), y=england_prev), color = "navyblue", size = 2) +
    annotate("text", x=0.75, y= 7.25, label = "England average", color = "navyblue", size  = 7)

}

#Create barchart3 - depression follow up
create_barchart_of_depression_review_by_region <- function(regional_prevalence_with_ranks, england_prevalence, nhs_region){

  #Order by rank
  regional_prevalence_with_ranks$Parent.Name <- factor(regional_prevalence_with_ranks$Parent.Name,
                                                       levels = regional_prevalence_with_ranks$Parent.Name[order(regional_prevalence_with_ranks$prevalence)])
  #Create themes for formatting text size, colour etc
  axis_labels <- element_text(face = "bold", size = 20)
  region_labels <- element_text(size = 20, hjust = 1, colour = "black")
  prevalence_labels <- element_text(size = 20, vjust = 0.2, hjust = 0.5)

  #Create dataframe for England average line
  england_prev <- rep(england_prevalence, length(regional_prevalence_with_ranks$Parent.Name))
  region_names <- as.vector(regional_prevalence_with_ranks$Parent.Name)
  england_prevalence_line <- data.frame(england_prev, region_names)

  ColourSchemeBlue <- brewer.pal(2,"Blues")

  #Plot
  ggplot(regional_prevalence_with_ranks, aes(x=Parent.Name, y=prevalence)) +
    coord_flip() +
    theme(axis.title = axis_labels, axis.text.x = prevalence_labels, axis.text.y = region_labels) +
    labs(x = "NHS Region", y = "Percentage who had a review 10-56 days after \n diagnosis of depression (%)") +
    scale_fill_manual(values = ColourSchemeBlue) +
    geom_bar(stat = "identity", colour="black", aes(fill=Parent.Name==nhs_region), show.legend = FALSE) +

    geom_line(data = england_prevalence_line, aes(x=as.numeric(region_names), y=england_prev), color = "navyblue", size = 2) +
    annotate("text", x=0.75, y= 63.80, label = "England average", color = "navyblue", size  = 7)

}

#subset shapefile by region
region_subset <- function(shapefile, nhs_region) {
  subset(shapefile, shapefile$Parent.Name == nhs_region)
}

# Create map 1 - purple
create_choropleth_map_by_prevalence_purple <- function(shapefile, nhs_region){

  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(shapefile@data$prevalence,
                        n=4, # set the number of ranges to create
                        style="jenks") # set the algorithm to use to create the ranges

  #get 4 Purple ColorBrewer Colours
  ColourSchemePurple <- brewer.pal(4,"Purples")

  # plot a map using the new class breaks and colours we created just now.
  plot(shapefile,
       col= ColourSchemePurple[findInterval(shapefile@data$prevalence, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))

  # overlay map with selected region, highlighted in black
  plot(region_subset(shapefile, nhs_region),
       border = rgb(0.0,0.0,0.0),
       add = TRUE)

# Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         inset=c(-0.07), # adds space to the right of legend so it doesn't overlap with map
         legend = leglabs(breaks$brks, reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourSchemePurple), # use the colour scheme created earlier
         bty = "n",
         cex = 1.8, #expansion factor - expands text to make larger
         title = "Percentage (%)"
  )
  par(xpd=FALSE)# disables clipping of the legend by the map extent
}

# Create map 2 - green
create_choropleth_map_by_prevalence_green <- function(shapefile, nhs_region){
  
  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(shapefile@data$prevalence,
                        n=4, # set the number of ranges to create
                        style="jenks") # set the algorithm to use to create the ranges
  
  #get 4 Purple ColorBrewer Colours
  ColourSchemeGreen <- brewer.pal(4,"Greens")
  
  # plot a map using the new class breaks and colours we created just now.
  plot(shapefile,
       col= ColourSchemeGreen[findInterval(shapefile@data$prevalence, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))
  
  # overlay map with selected region, highlighted in black
  plot(region_subset(shapefile, nhs_region),
       border = rgb(0.0,0.0,0.0),
       add = TRUE)
  
  # Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         inset=c(-0.07), # adds space to the right of legend so it doesn't overlap with map
         legend = leglabs(breaks$brks, reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourSchemeGreen), # use the colour scheme created earlier
         bty = "n",
         cex = 1.8, #expansion factor - expands text to make larger
         title = "Percentage (%)"
  )
  par(xpd=FALSE)# disables clipping of the legend by the map extent
}


# Create map 3 - Orange
create_choropleth_map_by_prevalence_orange <- function(shapefile, nhs_region){
  
  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(shapefile@data$prevalence,
                        n=4, # set the number of ranges to create
                        style="jenks") # set the algorithm to use to create the ranges
  
  #get 4 Purple ColorBrewer Colours
  ColourSchemeOrange <- brewer.pal(4,"Oranges")
  
  # plot a map using the new class breaks and colours we created just now.
  plot(shapefile,
       col= ColourSchemeOrange[findInterval(shapefile@data$prevalence, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))
  
  # overlay map with selected region, highlighted in black
  plot(region_subset(shapefile, nhs_region),
       border = rgb(0.0,0.0,0.0),
       add = TRUE)
  
  # Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         inset=c(-0.07), # adds space to the right of legend so it doesn't overlap with map
         legend = leglabs(breaks$brks, reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourSchemeOrange), # use the colour scheme created earlier
         bty = "n",
         cex = 1.8, #expansion factor - expands text to make larger
         title = "Percentage (%)"
  )
  par(xpd=FALSE)# disables clipping of the legend by the map extent
}

### CMD Prevalence data with CCG breakdown
#join CCG shapefile to regional prevalence data
join_prevalence_data_to_CCG_shapefile <- function(CCG_prevalence, CCG_shapefile){
  CCG_prevalence <- setnames(CCG_prevalence, "Area.Code", "ccg15cd")
  CCG_shapefile@data <-  CCG_shapefile@data %>% 
    left_join(CCG_prevalence, by='ccg15cd')
  
  return(CCG_shapefile)
}

#subset shapefile by region
CCG_subset <- function(CCG_shapefile, CCG_region) {
  subset(CCG_shapefile, CCG_shapefile$Area.Name == CCG_region)
}

# Create map 6 - CCG level CMDs
create_choropleth_map_CCG <- function(CCG_shapefile, CCG_region){
  
  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(CCG_shapefile@data$Value,
                        n=6, # set the number of ranges to create
                        style="jenks") # set the algorithm to use to create the ranges
  
  #get 4 Purple ColorBrewer Colours
  ColourSchemeYlGnBu <- brewer.pal(6,"YlGnBu")
  
  # plot a map using the new class breaks and colours we created just now.
  plot(CCG_shapefile,
       col= ColourSchemeYlGnBu[findInterval(CCG_shapefile@data$Value, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))
  
  # overlay map with selected region, highlighted in black
  plot(CCG_subset(CCG_shapefile, CCG_region),
       border = rgb(0.0,0.0,0.0),
       add = TRUE)
  
  # Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         legend = leglabs(breaks$brks, reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourSchemeYlGnBu), # use the colour scheme created earlier
         bty = "n",
         cex = 2.5, #expansion factor - expands text to make larger
         title = "Percentage (%)"
  )
  par(xpd=FALSE)# disables clipping of the legend by the map extent
  }



#Function to turn integers into ranks

int_to_ranking <- function(i){
  
  whereinf <- is.infinite(i)
  wherena <- is.na(i)
  whereneg <- sign(i) == -1L
  i <- suppressWarnings(as.integer(i))
  if(any(is.na(i) & (!whereinf) & (!wherena))) stop('could not convert some inputs to integer')
  
  last_digit <- as.numeric(substring(i, nchar(i)))
  ending <- sapply(last_digit + 1, switch, 'th', 'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th')
  second_last_digit <- as.numeric(substring(i, nchar(i) - 1, nchar(i) - 1))
  ending[second_last_digit == 1L] <- 'th'
  out <- paste(i, ending, sep = '')
  
  out[whereinf] <- 'infinitieth'
  out[whereinf & whereneg] <- '-infinitieth'
  out[wherena] <- 'missingith'
  
  return(out)
}

#Narrative function for dataset 1
create_narrative1 <- function(model_outputs, nhs_region){
  Eng_Prev <- model_outputs[[3]]
  
  Year <- "2014/15 "
  single_region <- subset(model_outputs[[1]]@data, Parent.Name == nhs_region)
   Region_Name<-single_region$Parent.Name

  a<-"In "
  b<-" the prevalence of common mental health disorders in the "
  c<-" NHS region was "
  d<-single_region$prevalence
  e<-"%. This was "
  f<-ifelse(single_region$prevalence < Eng_Prev,"lower than ",
            ifelse(single_region$prevalence > Eng_Prev, "higher than ",
                   ifelse(single_region$prevalence <- Eng_Prev, "equal to ")))
  g<-"the overall prevalence of "
  h<- "% in England. In comparison to other NHS regions, "
  i<-" was ranked "
  j<-int_to_ranking(single_region$rank)
  k<-" in England."

  narrative_text<-paste(a,Year,b,Region_Name,c,d,e,f,g,Eng_Prev,h,Region_Name,i,j,k, sep = "")

  return(narrative_text)
}

#Narrative function for dataset 2
create_narrative2 <- function(model_outputs, nhs_region){
  Eng_Prev <- model_outputs[[3]]
  
  Year <- "2014/15 "
  single_region <- subset(model_outputs[[1]]@data, Parent.Name == nhs_region)
  Region_Name<-single_region$Parent.Name
  
  a<-"In "
  b<-" the percentage of patients on GP practice register recorded as having depression in the "
  c<-" NHS region was "
  d<-single_region$prevalence
  e<-"%. This was "
  f<-ifelse(single_region$prevalence < Eng_Prev,"lower than ",
            ifelse(single_region$prevalence > Eng_Prev, "higher than ",
                   ifelse(single_region$prevalence <- Eng_Prev, "equal to ")))
  g<-"the overall prevalence of "
  h<- "% in England. In comparison to other NHS regions, "
  i<-" was ranked "
  j<-int_to_ranking(single_region$rank)
  k<-" in England."
  
  narrative_text<-paste(a,Year,b,Region_Name,c,d,e,f,g,Eng_Prev,h,Region_Name,i,j,k, sep = "")
  
  return(narrative_text)
}

#Narrative function for dataset 3
create_narrative3 <- function(model_outputs, nhs_region){
  Eng_Prev <- model_outputs[[3]]
  
  Year <- "2014/15 "
  single_region <- subset(model_outputs[[1]]@data, Parent.Name == nhs_region)
  Region_Name<-single_region$Parent.Name
  
  a<-"In "
  b<-" the percentage of newly diagnosed patients with depression who had a review 10-56 days after diagnosis in the "
  c<-" NHS region was "
  d<-single_region$prevalence
  e<-"%. This was "
  f<-ifelse(single_region$prevalence < Eng_Prev,"lower than ",
            ifelse(single_region$prevalence > Eng_Prev, "higher than ",
                   ifelse(single_region$prevalence <- Eng_Prev, "equal to ")))
  g<-"the overall proportion of "
  h<- "% in England. In comparison to other NHS regions, "
  i<-" was ranked "
  j<-int_to_ranking(single_region$rank)
  k<-" in England."
  
  narrative_text<-paste(a,Year,b,Region_Name,c,d,e,f,g,Eng_Prev,h,Region_Name,i,j,k, sep = "")
  
  return(narrative_text)
}

#Create run model function for dataset - prevalence
run_model <- function(prevalence_dataset, shapefile, metadata){
  england_prevalence <- aggregate_prevalence_to_England(prevalence_dataset)
  region_prevalence <- aggregate_prevalence_to_region(prevalence_dataset)
  thirteen_level_NHS_regional_prevalence <- manipulate_regions_for_shapefile(region_prevalence)
  regional_prevalence_with_ranks <- rank_prevalence_by_region(thirteen_level_NHS_regional_prevalence)
  region_shapefile_with_joined_prevalence_data <- join_prevalence_data_to_shapefile(regional_prevalence_with_ranks,
                                                                                    shapefile)
  return(list(region_shapefile_with_joined_prevalence_data, regional_prevalence_with_ranks, england_prevalence))
}


##Rates

#Function to aggregate rates to England
aggregate_rates_to_England <- function(rates_data) {
  England_rate <- round((sum(rates_data$Rate))/13, digits = 1) #could change 13 to have r count number of rows
  return(England_rate) 
  }

#Add rank column/variable to dataset - rates
rank_rates_by_region <- function(rates_data){
  rates_data$Rank <- NA
  rates_data$Rank[order(-rates_data$Rate)] <- 1:nrow(rates_data)
  
  return(rates_data)
}

rates_with_ranks <- rank_rates_by_region(suicide_rates)

#join shapefile to regional rate data
join_rate_data_to_shapefile <- function(rates_data_with_ranks, region_shapefile){
  rates_data_with_ranks <- setnames(rates_data_with_ranks, "Region.code", "nhsrg15cd")
  region_shapefile@data <-  region_shapefile@data %>% 
    left_join(rates_data_with_ranks, by='nhsrg15cd')
  
  return(region_shapefile)
}

#Create barchart4 - suicide rates
create_barchart_of_suicide_rates_by_region <- function(rates_data, England_rate, nhs_region){
  
  #Order by rank
  rates_data$Region.name <- factor(rates_data$Region.name,
                                                       levels = rates_data$Region.name[order(rates_data$Rate)])
  #Create themes for formatting text size, colour etc
  axis_labels <- element_text(face = "bold", size = 20)
  region_labels <- element_text(size = 20, hjust = 1, colour = "black")
  prevalence_labels <- element_text(size = 20, vjust = 0.2, hjust = 0.5)
  
  #Create dataframe for England average line
  england_rate_rep <- rep(England_rate, length(rates_data$Region.name))
  region_names <- as.vector(rates_data$Region.name)
  england_rate_line <- data.frame(england_rate_rep, region_names)
  
  ColourSchemeBlue <- brewer.pal(2,"Blues")
  
  #Plot
  ggplot(rates_data, aes(x=Region.name, y=Rate)) +
    coord_flip() +
    theme(axis.title = axis_labels, axis.text.x = prevalence_labels, axis.text.y = region_labels) +
    labs(x = "NHS Region", y = "Rate (per 100,000 population)") +
    scale_fill_manual(values = ColourSchemeBlue) +
    geom_bar(stat = "identity", colour="black", aes(fill=Region.name==nhs_region), show.legend = FALSE) +
    
    geom_line(data = england_rate_line, aes(x=as.numeric(region_names), y=england_rate_rep), color = "navyblue", size = 2) +
    annotate("text", x=0.75, y= 10, label = "England average", color = "navyblue", size  = 7)
  
}

#subset shapefile by region
region_subset_rate <- function(shapefile, nhs_region) {
  subset(shapefile, shapefile$Region.name == nhs_region)
}

# Create a map of rate by NHS Region - red
create_choropleth_map_of_rate <- function(shapefile, nhs_region){
  
  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(shapefile@data$Rate,
                        n=4, # set the number of ranges to create
                        style="jenks") # set the algorithm to use to create the ranges
  
  #get 4 Green ColorBrewer Colours
  ColourSchemeRed <- brewer.pal(4,"Reds")
  
  # plot a map using the new class breaks and colours we created just now.
  plot(shapefile,
       col= ColourSchemeRed[findInterval(shapefile@data$Rate, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))
  
  # overlay map with selected region, highlighted in black
  plot(region_subset_rate(shapefile, nhs_region),
       border = rgb(0.0,0.0,0.0),
       add = TRUE)
  
  # Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         inset=c(-0.07), # adds space to the right of legend so it doesn't overlap with map
         legend = leglabs(breaks$brks, reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourSchemeRed), # use the colour scheme created earlier
         bty = "n",
         cex = 1.8, #expansion factor - expands text to make larger
         title = "Rate (per 100,000 population)"
  )
  par(xpd=FALSE)# disables clipping of the legend by the map extent
}

#Narrative function for rates
create_narrative4 <- function(model_outputs, nhs_region){
  Eng_Average <- model_outputs[[3]]
  
  Year <- "2015"
  single_region <- subset(model_outputs[[1]]@data, Region.name == nhs_region)
  Region_Name<-single_region$Region.name
  
  a<-"In "
  b<-" the age-standardised suicide rate in the "
  c<-" NHS region was "
  d<-single_region$Rate
  e<-" per 100,000 population. This was "
  f<-ifelse(single_region$Rate < Eng_Average,"lower than ",
            ifelse(single_region$Rate > Eng_Average, "higher than ",
                   ifelse(single_region$Rate <- Eng_Average, "equal to ")))
  g<-"the average rate of "
  h<- " per 100,000 population in England. In comparison to other NHS regions, "
  i<-" was ranked "
  j<-int_to_ranking(single_region$Rank)
  k<-" in England."
  
  narrative_text<-paste(a,Year,b,Region_Name,c,d,e,f,g,Eng_Average,h,Region_Name,i,j,k, sep = "")
  
  return(narrative_text)
}


#Create run model function for dataset - rates
run_model_rates <- function(rates_data, shapefile, metadata){
  England_rate <- aggregate_rates_to_England(rates_data)
  rates_data_with_ranks <- rank_rates_by_region(rates_data)
  region_shapefile_with_joined_rate_data <- join_rate_data_to_shapefile(rates_data_with_ranks,
                                                                                    shapefile)
  return(list(region_shapefile_with_joined_rate_data, rates_data_with_ranks, England_rate))
}

##Spending data
#Function to aggregate spending to England
aggregate_spending_to_England <- function(spending_data) {
  England_count <- sum(spending_data$CCG.spending.on.mental.health)
  England_pop <- sum(spending_data$CCG.population)
  England_spending <- round((England_count / England_pop)*1000, digits = 1)
  
  return(England_spending)
}

#Function to aggregate spending to NHS region
aggregate_spending_to_region <- function(spending_data) {
  regional_level_spending <- spending_data %>%
    group_by(Parent.Code, Parent.Name) %>%
    summarise(Spend = sum(CCG.spending.on.mental.health),
              Population = sum(CCG.population)) %>%
    mutate(CCG.spending.on.mental.health.per.capita=round((Spend/Population)*1000, digits =1))
  
  return(regional_level_spending)
}

#Function to manipulate regions to match shapefile
manipulate_spending_regions_for_shapefile <- function(region_spending) {
  #Combining regions to match shapefile
  removed_regions <- region_spending %>%
    filter(Parent.Code != "E39000037") %>%
    filter(Parent.Code != "E39000038")
  
  #Sum regions
  summed_regions <- region_spending %>%
    filter(Parent.Code %in% c("E39000037","E39000038")) %>%
    group_by() %>%
    summarise(Parent.Code = "E39000028",
              Parent.Name = "Lancashire and Greater Manchester",
              Spend = sum(Spend),
              Population = sum(Population)) %>%
    mutate(CCG.spending.on.mental.health.per.capita = round((Spend/Population)*1000, digits =1))
  
  #Add row
  thirteen_level_NHS_regional_spending <- summed_regions %>%
    bind_rows(removed_regions)
  
  return(thirteen_level_NHS_regional_spending)
}

#Add rank column/variable to dataset - Spending
rank_spending_by_region <- function(thirteen_level_NHS_regional_spending){
  thirteen_level_NHS_regional_spending$rank <- NA
  thirteen_level_NHS_regional_spending$rank[order(-thirteen_level_NHS_regional_spending$CCG.spending.on.mental.health.per.capita)] <- 1:nrow(thirteen_level_NHS_regional_spending)
  
  return(thirteen_level_NHS_regional_spending)
}

#join shapefile to regional spending data
join_spending_data_to_shapefile <- function(regional_spending_with_ranks, region_shapefile){
  regional_spending_with_ranks <- setnames(regional_spending_with_ranks, "Parent.Code", "nhsrg15cd")
  region_shapefile@data <-  region_shapefile@data %>% 
    left_join(regional_spending_with_ranks, by='nhsrg15cd')
  
  return(region_shapefile)
}


#Create barchart5 - MH spending
create_barchart_of_MH_spending_by_region <- function(spending_data, England_spending, nhs_region){
  
  #Order by rank
  spending_data$Parent.Name <- factor(spending_data$Parent.Name,
                                   levels = spending_data$Parent.Name[order(spending_data$CCG.spending.on.mental.health.per.capita)])
  #Create themes for formatting text size, colour etc
  axis_labels <- element_text(face = "bold", size = 20)
  region_labels <- element_text(size = 20, hjust = 1, colour = "black")
  prevalence_labels <- element_text(size = 20, vjust = 0.2, hjust = 0.5)
  
  #Create dataframe for England average line
  england_spend_rep <- rep(England_spending, length(spending_data$Parent.Name))
  region_names <- as.vector(spending_data$Parent.Name)
  england_spend_line <- data.frame(england_spend_rep, region_names)
  
  ColourSchemeBlue <- brewer.pal(2,"Blues")
  
  #Plot
  ggplot(spending_data, aes(x=Parent.Name, y=CCG.spending.on.mental.health.per.capita)) +
    coord_flip() +
    theme(axis.title = axis_labels, axis.text.x = prevalence_labels, axis.text.y = region_labels) +
    labs(x = "NHS Region", y = "Spending on mental health per 1,000 population") +
    scale_fill_manual(values = ColourSchemeBlue) +
    geom_bar(stat = "identity", colour="black", aes(fill=Parent.Name==nhs_region), show.legend = FALSE) +
    
    geom_line(data = england_spend_line, aes(x=as.numeric(region_names), y=england_spend_rep), color = "navyblue", size = 2) +
    annotate("text", x=0.75, y= 155, label = "England average", color = "navyblue", size  = 7)
  
}


#Function for suicides time series chart
reshape_suicide_time_series <- function(Suicide_data) {
  #reformat dataframe for line chart 
  Suicides_time_series_reshaped <- melt(Suicide_data, id.vars="Region.name", value.name="Rate", variable.name="Year")
  return(Suicides_time_series_reshaped) 
}


Filter_suicide_time_series <- function (reshaped_sdata, nhs_region) {
#Filter data by single region
specificregion <-subset(reshaped_sdata, reshaped_sdata$Region.name == nhs_region)
return(specificregion)}


#Function for suicides time series chart
create_suicide_time_series <- function(suicide_data) {
  #Plot line chart
  suicides_chart <- ggplot(data = suicide_data, aes(x=Year, y=Rate, group = Region.name)) +
    geom_line() 
}

run_model_timeseriesdata <- function(Suicides_time_series_raw){
  reshaped_suicide_data <-reshape_suicide_time_series(Suicides_time_series_raw)
  specificregion <- Filter_suicide_time_series(reshaped_suicide_data, region)
  return(specificregion)
}


#subset shapefile by region
region_subset_spending <- function(shapefile, nhs_region) {
  subset(shapefile, shapefile$Parent.Name == nhs_region)
}

# Create a map of spending by NHS Region - Pink
create_choropleth_map_of_spending <- function(shapefile, nhs_region){
  
  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(shapefile@data$CCG.spending.on.mental.health.per.capita,
                        n=4, # set the number of ranges to create
                        style="jenks") # set the algorithm to use to create the ranges
  
  #get 4 Green ColorBrewer Colours
  ColourSchemePink <- brewer.pal(4,"RdPu")
  
  # plot a map using the new class breaks and colours we created just now.
  plot(shapefile,
       col= ColourSchemePink[findInterval(shapefile@data$CCG.spending.on.mental.health.per.capita, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))
  
  # overlay map with selected region, highlighted in black
  plot(region_subset_spending(shapefile, nhs_region),
       border = rgb(0.0,0.0,0.0),
       add = TRUE)
  
  # Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         inset=c(-0.07), # adds space to the right of legend so it doesn't overlap with map
         legend = leglabs(breaks$brks, reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourSchemePink), # use the colour scheme created earlier
         bty = "n",
         cex = 1.8, #expansion factor - expands text to make larger
         title = "Spending per 1,000 population"
  )
  par(xpd=FALSE)# disables clipping of the legend by the map extent
}

#Narrative function for spending
create_narrative5 <- function(model_outputs, nhs_region){
  Eng_Average <- model_outputs[[3]]
  
  Year <- "2013/14"
  single_region <- subset(model_outputs[[1]]@data, Parent.Name == nhs_region)
  Region_Name<-single_region$Parent.Name
  
  a<-"In "
  b<-" the spending on mental health in the "
  c<-" NHS region was £"
  d<-single_region$CCG.spending.on.mental.health.per.capita
  e<-" per 1,000 population. This was "
  f<-ifelse(single_region$CCG.spending.on.mental.health.per.capita < Eng_Average,"lower than ",
            ifelse(single_region$CCG.spending.on.mental.health.per.capita > Eng_Average, "higher than ",
                   ifelse(single_region$CCG.spending.on.mental.health.per.capita <- Eng_Average, "equal to ")))
  g<-"the average spending of £"
  h<- " per 1,000 population in England. In comparison to other NHS regions, "
  i<-" was ranked "
  j<-int_to_ranking(single_region$rank)
  k<-" in England."
  
  narrative_text<-paste(a,Year,b,Region_Name,c,d,e,f,g,Eng_Average,h,Region_Name,i,j,k, sep = "")
  
  return(narrative_text)
}


#Create run model function for dataset - spending
run_model_spending <- function(spending_data, shapefile, metadata){
  England_spending <- aggregate_spending_to_England(spending_data)
  region_spending <- aggregate_spending_to_region(spending_data)
  thirteen_level_NHS_regional_spending <- manipulate_spending_regions_for_shapefile(region_spending)
  spending_data_with_ranks <- rank_spending_by_region(thirteen_level_NHS_regional_spending)
  region_shapefile_with_joined_spending_data <- join_spending_data_to_shapefile(spending_data_with_ranks,
                                                                        shapefile)
  return(list(region_shapefile_with_joined_spending_data, spending_data_with_ranks, England_spending))
}



#Run model
model_outputs1 <- run_model(CCG_prevalence, region_shapefile, "metadata")
model_outputs2 <- run_model(depression_prevalence, region_shapefile, "metadata")
model_outputs3 <- run_model(depression_review, region_shapefile, "metadata")
model_outputs4 <- run_model_rates(suicide_rates, region_shapefile, "metadata")
model_outputs5 <- run_model_spending(CCG_spending, region_shapefile, "metadata")
model_outputs6 <- join_prevalence_data_to_CCG_shapefile(CCG_prevalence, CCG_shapefile)
model_outputs7 <- run_model_timeseriesdata(Suicides_time_series_raw)



#Tests
test_results <- test_dir("src/r/", reporter="summary")
test_results

# tests only work with 'data/' in the same dir as model.R and tests_model_functions.R
