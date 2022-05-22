# Aggregate table script
# Jake Flynn
# 05/22/22
# Group members: Jake Flynn, Jackson Kamins, Belle Witt

# load the data set and package
library(tidyverse)
library(dplyr)
heart_data <- read.csv(url("https://raw.githubusercontent.com/info201b-2022-spring/final-projects-bewitt/main/data/heart_2020_cleaned.csv"))

# new data set for the heart health data that includes only columns of interest
heart_data_set <- select(heart_data, HeartDisease, Smoking, AlcoholDrinking, Stroke, AgeCategory)

# new data set that is grouped by age category
grouped <- group_by(heart_data_set, AgeCategory)

# Convert HeartDisease, Stroke, Smoking, and AlcoholDrinking columns to values that can be summed
grouped$HeartDisease<-ifelse(grouped$HeartDisease=="Yes", 1, 0)
grouped$Stroke<-ifelse(grouped$Stroke=="Yes", 1, 0)
grouped$Smoking<-ifelse(grouped$Smoking=="Yes", 1, 0)
grouped$AlcoholDrinking<-ifelse(grouped$AlcoholDrinking=="Yes", 1, 0)

# Include proportion of people who have reported heart disease, smoking. alcohol drinking, or a stroke for their age category
aggregate_table <- summarize(grouped, heart_disease_prop = format(sum(HeartDisease) / length(HeartDisease), digits = 2),
                             stroke_prop = format(sum(Stroke) / length(Stroke), digits = 2),
                             smoke_prop = format(sum(Smoking) / length(Smoking), digits = 2),
                             alcohol_prop = format(sum(AlcoholDrinking) / length(AlcoholDrinking), digits = 2))

# Create human readable column names
colnames(aggregate_table) <- c("Age Category", "Heart Disease Proportion", "Stroke Proportion", "Smoking Proportion", "Alcohol Drinking Proportion")
