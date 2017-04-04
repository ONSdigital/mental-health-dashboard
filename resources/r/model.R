#install.packages("data.table")
library(data.table)

#CCG Data
CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")

#Function to aggregate to England
aggregate_prevalence_to_England <- function(prevalence_data) {
  England_count <- sum(prevalence_data$Count)
  England_pop <- sum(prevalence_data$Denominator)
  England_prevalence <- 100*(England_count / England_pop)
  return(England_prevalence)
}
#Run function, specifying dataset to use
aggregate_prevalence_to_England(CCG_prevalence)
