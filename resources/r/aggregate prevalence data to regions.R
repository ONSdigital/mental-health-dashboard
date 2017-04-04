#install.packages("dplyr")
library(dplyr)

#Calculating England Prevalence from CCGs
CCG_prevalence <- read.csv("data/Estimated_Prevalence_of_CMDs_2014-2015.csv")

#leaving here for Nick's ref:  Region_count <- aggregate(CCG_prevalence$Count, by=list(CCG_prevalence$Parent.Code), FUN=sum)


regional_level_prevalence <- CCG_prevalence%>%
  group_by(Parent.Code) %>%
  summarise(Count = sum(Count),
            Population = sum(Denominator)) %>%
  mutate(prevalence=(Count/Population)*100)