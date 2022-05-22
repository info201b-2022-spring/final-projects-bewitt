# Chart 3 (Scatter plot matrix): Heart Disease Risk Factor Correlations
# Jake Flynn
# 05/22/22
# Group members: Jake Flynn, Jackson Kamins, Belle Witt

# load the data set and package
library(dplyr)
library(ggplot2)
heart_data <- read.csv(url("https://raw.githubusercontent.com/info201b-2022-spring/final-projects-bewitt/main/data/heart_2020_cleaned.csv"))

# new data set that is grouped by age category
grouped <- group_by(heart_data, AgeCategory)

# Convert HeartDisease, Stroke, Smoking, and AlcoholDrinking columns to values that can be summed
grouped$HeartDisease<-ifelse(grouped$HeartDisease=="Yes", 1, 0)
grouped$Stroke<-ifelse(grouped$Stroke=="Yes", 1, 0)
grouped$Smoking<-ifelse(grouped$Smoking=="Yes", 1, 0)
grouped$AlcoholDrinking<-ifelse(grouped$AlcoholDrinking=="Yes", 1, 0)

# Include proportion of people who have reported heart disease, smoking. alcohol drinking, or a stroke for their age category
new_table <- summarize(grouped, heart_disease_prop = sum(HeartDisease) / length(HeartDisease),
                             stroke_prop = sum(Stroke) / length(Stroke),
                             smoke_prop = sum(Smoking) / length(Smoking),
                             alcohol_prop = sum(AlcoholDrinking) / length(AlcoholDrinking))

# Create new data with continuous values
data <- new_table[ , c(2:5)]
# Create scatter plot matrix
scatter_matrix <- pairs(data, main = "Heart Disease Risk Factor Correlations", labels = c("Heart Disease\nProportion", "Stroke Proportion", "Smoking Proportion", "Alcohol Drinking\nProportion"))
