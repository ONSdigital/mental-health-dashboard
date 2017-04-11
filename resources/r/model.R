#install.packages("data.table")
#install.packages("dplyr")
#install.packages("maptools")
library(data.table)
library(dplyr)
library(maptools)


####Data
#CCG Data
CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")
#Shapefile data
region_shapefile <- readShapePoly("data/NHS_regions/NHS_Regions_Geography_April_2015_Super_Generalised_Clipped_Boundaries_in_England.shp")

region_shapefile@data
View(region_shapefile)

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
              Parent.Name = "Lancashire and Greater Manchester NHS region",
              Count = sum(Count),
              Population = sum(Population)) %>%
    mutate(prevalence = (Count/Population)*100)
  
  #Add row
  thirteen_level_NHS_regional_prevalence <- summed_regions %>%
    bind_rows(removed_regions)
  
  return(thirteen_level_NHS_regional_prevalence)
}
  

#join shapefile to regional prevalence data
join_difference_to_shapefile <- function(thirteen_level_NHS_regional_prevalence,region_shapefile){
  region_shapefile@data <- setnames(region_shapefile@data, "nhsrg15cd", "Parent.Code")
  region_shapefile@data <-  region_shapefile@data %>% left_join(thirteen_level_NHS_regional_prevalence, by='Parent.Code')
  
  return(region_shapefile)
  }

#Run function, specifying dataset to use
england_prevalence <- aggregate_prevalence_to_England(CCG_prevalence)
region_prevalence <- aggregate_prevalence_to_region(CCG_prevalence)
thirteen_level_NHS_regional_prevalence <- manipulate_regions_for_shapefile(region_prevalence)
region_shapefile_with_joined_prevalence_data<- join_difference_to_shapefile(thirteen_level_NHS_regional_prevalence, region_shapefile)

#plot(region_shapefile_with_joined_prevalence_data, col = region_shapefile_with_joined_prevalence_data@data$prevalence)
#region_shapefile_with_joined_prevalence_data@data
