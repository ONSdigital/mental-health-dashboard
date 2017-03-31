#install.packages("data.table")
library(data.table)

#Calculating England Prevalence from CCGs
CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")

England_count <- sum(CCG_prevalence$Count)
England_pop <- sum(CCG_prevalence$Denominator)
England_prevalence <- 100*(England_count / England_pop)
