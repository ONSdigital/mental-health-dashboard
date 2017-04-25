#install.packages("data.table")
#install.packages("dplyr")
#install.packages("maptools")
#install.packages("ggplot2")
#install.packages("classInt")
#install.packages("RColorBrewer")
library(data.table)
library(dplyr)
library(maptools)
library(ggplot2)
library(classInt)
library(RColorBrewer)

####Data
#CCG Data
CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")
#Shapefile data
region_shapefile <- readShapePoly("data/NHS_regions/NHS_Regions_Geography_April_2015_Super_Generalised_Clipped_Boundaries_in_England.shp")

region_shapefile@data
#View(region_shapefile)

####Model
#Function to aggregate to England
aggregate_prevalence_to_England <- function(prevalence_data) {
  England_count <- sum(prevalence_data$Count)
  England_pop <- sum(prevalence_data$Denominator)
  England_prevalence <- 100*(England_count / England_pop)
  
  return(England_prevalence)
}

#Function to aggregate to Region
aggregate_prevalence_to_region <- function(prevalence_data) {
  regional_level_prevalence <- prevalence_data %>%
    group_by(Parent.Code, Parent.Name) %>%
    summarise(Count = sum(Count),
              Population = sum(Denominator)) %>%
    mutate(prevalence=(Count/Population)*100)
  
  return(regional_level_prevalence)
}

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

#Order by rank
regional_prevalence_with_ranks$Parent.Name <- factor(regional_prevalence_with_ranks$Parent.Name, 
                                                     levels = regional_prevalence_with_ranks$Parent.Name[order(regional_prevalence_with_ranks$prevalence)])


create_barchart_of_prevalence_by_region <- function(regional_prevalence_with_ranks, england_prevalence){
  #Create themes for formatting text size, colour etc
  title_label <- element_text(face = "bold", color = "turquoise4", size = 14)
  axis_labels <- element_text(color = "dodgerblue4", size = 12, hjust = 0.5)
  region_labels <- element_text(size = 12, hjust = 1)
  prevalence_labels <- element_text(size = 12, vjust = 0.2, hjust = 0.5)
  
  #Create dataframe for England average line
  england_prev <- rep(england_prevalence, length(regional_prevalence_with_ranks$Parent.Name))
  region_names <- as.vector(regional_prevalence_with_ranks$Parent.Name)
  england_prevalence_line <- data.frame(england_prev, region_names)
  
  #Plot
  ggplot(regional_prevalence_with_ranks, aes(x=Parent.Name, y=prevalence)) +
    coord_flip() +
    theme(axis.title = axis_labels, title = title_label, axis.text.x = prevalence_labels, axis.text.y = region_labels) +
    labs(title = "Prevalence of Common Mental Disorders by NHS Region in England, 2014-2015", x = "NHS Region", y = "Prevalence of Common Mental Disorders (%)") +
    geom_bar(stat = "identity", fill="dodgerblue4") +
    geom_line(data = england_prevalence_line, aes(x=as.numeric(region_names), y=england_prev), color = "red", size = 2) +
    annotate("text", x=0.75, y= 15.75, label = "England average", color = "red", size  = 4)
  
}

# Create a map of prevalence by NHS Region
create_choropleth_map_by_prevalence <- function(region_shapefile_with_joined_prevalence_data){
  
# Uses RColorBrewer to generate 4 classes using the "Jenks" natural breaks methods (it can use other methods also)
breaks=classIntervals(region_shapefile_with_joined_prevalence_data@data$prevalence, n=4, style="jenks")

#get 4 Green ColorBrewer Colours
ColourScheme <- brewer.pal(4,"Greens")

# plot a map using the new class breaks and colours we created just now.
plot(region_shapefile_with_joined_prevalence_data, col= ColourScheme[findInterval(region_shapefile_with_joined_prevalence_data@data$prevalence, breaks$brks, all.inside = TRUE)], axes =FALSE, border = rgb(0.8,0.8,0.8))

# Create a title and a legend
title('Mental Health Prevalence in England, 2015')
legend(x = 10000, y = 120000, legend = leglabs(breaks$brks), fill = ColourScheme, bty = "n")
}

#Run function, specifying dataset to use
england_prevalence <- aggregate_prevalence_to_England(CCG_prevalence)
region_prevalence <- aggregate_prevalence_to_region(CCG_prevalence)
thirteen_level_NHS_regional_prevalence <- manipulate_regions_for_shapefile(region_prevalence)
regional_prevalence_with_ranks <- rank_prevalence_by_region(thirteen_level_NHS_regional_prevalence)
region_shapefile_with_joined_prevalence_data <- join_prevalence_data_to_shapefile(regional_prevalence_with_ranks, region_shapefile)
create_barchart_of_prevalence_by_region(regional_prevalence_with_ranks, england_prevalence)
choropleth_map_prevalence_by_NHS_Region <- create_choropleth_map_by_prevalence(region_shapefile_with_joined_prevalence_data)


View(region_shapefile_with_joined_prevalence_data)




