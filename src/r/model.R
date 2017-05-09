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

####Model
#Function to aggregate to England
aggregate_prevalence_to_England <- function(prevalence_data) {
  England_count <- sum(prevalence_data$Count)
  England_pop <- sum(prevalence_data$Denominator)
  England_prevalence <- round(100*(England_count / England_pop), digits = 0)
  
  return(England_prevalence)
}

#Function to aggregate to Region
aggregate_prevalence_to_region <- function(prevalence_data) {
  regional_level_prevalence <- prevalence_data %>%
    group_by(Parent.Code, Parent.Name) %>%
    summarise(Count = sum(Count),
              Population = sum(Denominator)) %>%
    mutate(prevalence=round((Count/Population)*100, digits =0))
  
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
    mutate(prevalence = (Count/Population)*100)
  
  #Add row
  thirteen_level_NHS_regional_prevalence <- summed_regions %>%
    bind_rows(removed_regions)
  
  return(thirteen_level_NHS_regional_prevalence)
}

#Add rank column/variable to dataset
rank_prevalence_by_region <- function(thirteen_level_NHS_regional_prevalence){
  thirteen_level_NHS_regional_prevalence$rank <- NA
  thirteen_level_NHS_regional_prevalence$rank[order(-thirteen_level_NHS_regional_prevalence$prevalence)] <- 1:nrow(thirteen_level_NHS_regional_prevalence)
  
  return(thirteen_level_NHS_regional_prevalence)
}

#join shapefile to regional prevalence data
join_prevalence_data_to_shapefile <- function(regional_prevalence_with_ranks, region_shapefile){
  region_shapefile@data <- setnames(region_shapefile@data, "nhsrg15cd", "Parent.Code")
  region_shapefile@data <-  region_shapefile@data %>% 
    left_join(regional_prevalence_with_ranks, by='Parent.Code')
  
  return(region_shapefile)
}

#Create barchart
create_barchart_of_prevalence_by_region <- function(regional_prevalence_with_ranks, england_prevalence, nhs_region){
  
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

# Create a map of prevalence by NHS Region
create_choropleth_map_by_prevalence <- function(shapefile){
  
  # Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
  breaks=classIntervals(shapefile@data$prevalence,
                        n=4, # set the number of ranges to create
                        style="jenks") # set the algorithm to use to create the ranges
  
  #get 4 Green ColorBrewer Colours
  ColourScheme <- brewer.pal(4,"Greens")
  
  # plot a map using the new class breaks and colours we created just now.
  plot(shapefile,
       col= ColourScheme[findInterval(shapefile@data$prevalence, breaks$brks, all.inside = TRUE)],
       axes =FALSE,
       border = rgb(0.6,0.6,0.6))
  
  # Create a legend
  par(xpd=TRUE) # disables clipping of the legend by the map extent
  legend("left", # sets where to place legend
         inset=c(-0.07), # adds space to the right of legend so it doesn't overlap with map
         legend = leglabs(breaks$brks, reverse = TRUE, between = "to"), # create the legend using the breaks created earlier
         fill = rev(ColourScheme), # use the colour scheme created earlier
         bty = "n",
         cex = 1.8, #expansion factor - expands text to make larger
         title = "Prevalence (%)"
  )
  par(xpd=FALSE)# disables clipping of the legend by the map extent
}


#Narrative function
create_narrative <- function(narrative, nhs_region){
  single_region <- subset(narrative, NHS.region == nhs_region) 
  regional_narrative <- as.character(single_region$Narative)
  return(regional_narrative)
}

# user parameter
region <- "South West"

#Create run model function

run_model <- function(prevalence_dataset, shapefile, metadata, narrative, nhs_region){
  england_prevalence <- aggregate_prevalence_to_England(prevalence_dataset)
  region_prevalence <- aggregate_prevalence_to_region(prevalence_dataset)
  thirteen_level_NHS_regional_prevalence <- manipulate_regions_for_shapefile(region_prevalence)
  regional_prevalence_with_ranks <- rank_prevalence_by_region(thirteen_level_NHS_regional_prevalence)
  region_shapefile_with_joined_prevalence_data <- join_prevalence_data_to_shapefile(regional_prevalence_with_ranks, 
                                                                                    shapefile)
  narrative_specific <- create_narrative(narrative, nhs_region)
  
  return(list(region_shapefile_with_joined_prevalence_data, regional_prevalence_with_ranks, england_prevalence, narrative_specific))
}

####Data
#CCG Data
CCG_prevalence <- read.csv("src/r/data/Estimated_Prevalence_of_CMDs_2014-2015.csv")
#Shapefile data
region_shapefile <- readShapePoly("src/r/data/NHS_Regions/NHS_Regions_Geography_April_2015_Super_Generalised_Clipped_Boundaries_in_England.shp")
#Narrative
narrative <- read.csv("src/r/data/NHS_region_narrative.csv")
#Metadata - need to add but in what format?

#Model outputs
model_outputs <- run_model(CCG_prevalence, region_shapefile, "metadata", narrative, region)

#Run plots
create_barchart_of_prevalence_by_region(model_outputs[[2]], model_outputs[[3]], region)
choropleth_map_prevalence_by_NHS_Region <- create_choropleth_map_by_prevalence(model_outputs[[1]])

#Tests
test_results <- test_dir("src/r/", reporter="summary")
test_results

# tests only work with 'data/' in the same dir as model.R and tests_model_functions.R