# Relationship between Sex and Heart Disease
# Jackson Kamins
# 05/20/22
# Group members: Jake Flynn, Jackson Kamins, Belle Witt, Daniel Kim

#Loading Data
library(tidyverse)
library(dplyr)
heart_df <- read.csv(url("https://raw.githubusercontent.com/info201b-2022-spring/final-projects-bewitt/main/data/heart_2020_cleaned.csv"))

heart_df$HeartDisease <- ifelse(heart_df$HeartDisease == "Yes", 1,0)

heart_disease_by_sex <- group_by(heart_df, Sex)

sex_heart_disease <- summarise(heart_disease_by_sex, total_disease = sum(HeartDisease))


#Create Pie Chart
slices <- c(sex_heart_disease$total_disease)
lbls <- c(slices)
lbls <- paste(lbls, sex_heart_disease$Sex)
lbls <- paste(lbls, "Deaths of Heart Disease")
pie(slices, labels = lbls, main = "Pie Chart of Sex and Risk of Heart Disease")

