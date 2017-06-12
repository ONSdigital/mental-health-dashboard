

# load GGplot2
library(ggplot2)

# Create test data.
depression_review <- read.csv("src/r/data/depression-follow-up-fractions.csv")

# Add addition columns, needed for drawing with geom_rect.
depression_review = depression_review[order(depression_review$fraction), ]
depression_review$ymax = cumsum(depression_review$fraction)
depression_review$ymin = c(0, head(depression_review$ymax, n=-1))

# Make the plot
donut <- ggplot(depression_review, aes(fill=Follow.up.category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme (legend.position = "bottom") +
  annotate("text", x = 0, y = 0, label = "Percentage of new depression \n diagnoses that had a follow-up \n review within 10-56 days") +
  labs(x = "", y = "") +
  geom_label(aes(label=paste(fraction*100,"%"),x=5,y=(ymin+ymax)/2))
                 
donut
