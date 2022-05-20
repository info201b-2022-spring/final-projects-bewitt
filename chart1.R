# Chart 1 (Barplot): Proportion of Americans With Heart Disease by Age Category (2020)
# Jake Flynn
# 05/20/22
# Group members: Jake Flynn, Jackson Kamins, Belle Witt, Daniel Kim

# load the data set and package
library(dplyr)
library(ggplot2)
heart_data <- read.csv("/Users/jakeflynn/final-projects-bewitt/data/heart_2020_cleaned.csv")

# new data set that is grouped by age category
grouped <- group_by(heart_data, AgeCategory)

# Convert HeartDisease to values that can be summed
grouped$HeartDisease<-ifelse(grouped$HeartDisease=="Yes", 1, 0)

# Include proportion of people who have reported heart disease for their age category
heart_disease_age <- summarize(grouped, heart_disease_prop = format(sum(HeartDisease) / length(HeartDisease), digits = 2))

heart_disease_age_barplot <- heart_disease_age %>%
  # First sort by `heart_disease_prop`. This sorts the data frame but NOT the factor levels.
  arrange(heart_disease_prop) %>%
  # This trick updates the factor levels.
  mutate(AgeCategory=factor(AgeCategory, levels=AgeCategory)) %>%
  ggplot(aes(x=AgeCategory, y=heart_disease_prop)) +
  geom_bar(stat = "identity", fill="#f68060", alpha=.6, width=.4)

# Call bar plot
heart_disease_age_barplot +
  coord_flip() +
  xlab("Age Category (years)") +
  ylab("Proportion of Age Category With Heart Disease") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Proportion of Americans With\nHeart Disease by Age Category (2020)")
