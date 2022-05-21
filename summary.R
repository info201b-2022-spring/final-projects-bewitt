# Summary 
# Belle Witt
# 05/20/22
# Group members: Jake Flynn, Jackson Kamins, Belle Witt, Daniel Kim

#load data & packages
library(tidyverse)
library(dplyr)
heart_df <- read.csv(url("https://raw.githubusercontent.com/info201b-2022-spring/final-projects-bewitt/main/data/heart_2020_cleaned.csv"))

summary_info <- list()
summary_info$num_observations <- nrow(heart_df)
summary_info$heartdisease <- heart_df %>%
  filter(HeartDisease == "Yes") %>%
  count()
summary_info$average_BMI_YesHD <- heart_df %>%
  filter(HeartDisease == "Yes") %>%
  summarise(mean(BMI))
summary_info$average_BMI_NoHD <- heart_df %>%
  filter(HeartDisease == "No") %>%
  summarise(mean(BMI))
summary_info$gender_heart_occur <- heart_df %>%
  group_by(Sex, HeartDisease) %>%
  count(Sex == "Female")
print(summary_info)