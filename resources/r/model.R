#install.packages("data.table")
#install.packages("dplyr")
library(data.table)
library(dplyr)

#CCG Data
CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")

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

#Run function, specifying dataset to use
england_prevalence <- aggregate_prevalence_to_England(CCG_prevalence)
region_prevalence <- aggregate_prevalence_to_region(CCG_prevalence)



