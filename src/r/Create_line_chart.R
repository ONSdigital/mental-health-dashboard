#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("data.table")
#install.packages("dplyr")
library(ggplot2)
library(reshape2)
library(data.table)
library(dplyr)


#Suicides Time series Data
Suicides_time_series_raw <- read.csv("src/r/data/REgion_Suicide_Time_Series.csv", check.names = F)


#Function for tie series
create_suicide_time_series <- function(Suicide_data, nhs_region) {
#reformat dataframe for line chart 
Suicides_time_series_reshaped <- melt(Suicide_data, id.vars="Region.name", value.name="Rate", variable.name="Year")
#Filter data by single region
Specific_region <- filter(Suicides_time_series_reshaped, Region.name == region)
#Plot line chart
suicides_chart <- ggplot(data = Specific_region, aes(x=Year, y=Rate, group = Region.name)) +
  geom_line() 
  #scale_fill_manual(values = Region.name) +
  #geom_line(stat = "identity", size=1, aes(colour=Region.name==nhs_region))
}

region = "Wessex"

timeseries <- create_suicide_time_series(Suicides_time_series_raw, region)
plot(timeseries)
