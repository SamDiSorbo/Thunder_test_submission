#load packages
library(readr)
library(tidyverse)
library(dplyr)
#load data
shots_data <- read_csv("Downloads/shots_data.csv")
#make new data values
shots_data$distance <- (sqrt((shots_data$x^2) + (shots_data$y^2)))
shots_data$corner_three <- ifelse(shots_data$y <= 7.8 & abs(shots_data$x) > 22, 1, 0)
shots_data$non_corner_three <- ifelse(shots_data$y > 7.8 & shots_data$distance > 23.75, 1, 0)
shots_data$two_pointer <- ifelse(shots_data$corner_three == 0 & shots_data$non_corner_three == 0, 1, 0)
#get team breakdown
teams <- shots_data %>% 
  mutate(corner_three_made = ifelse(corner_three == 1 & fgmade == 1, 1, 0)) %>% 
  mutate(two_pt_made = ifelse(two_pointer == 1 & fgmade == 1, 1, 0)) %>% 
  mutate(non_corner_three_made = ifelse(non_corner_three == 1 & fgmade == 1, 1, 0)) %>% 
  group_by(team) %>%
  summarise(
    total_shots = n(),
    two_pointers = sum(two_pointer, na.rm=T),
    non_corner_threes = sum(non_corner_three, na.rm=T),
    corner_threes = sum(corner_three, na.rm=T),
    two_pointers_made = sum(two_pt_made, na.rm=T),
    corner_threes_made = sum(corner_three_made, na.rm=T),
    non_corner_threes_made = sum(non_corner_three_made, na.rm=T),
  )
#get shot zone distributions
teams$`Percent_shots_2PT` <- teams$two_pointers/teams$total_shots
teams$`Percent_shots_C3` <- teams$corner_threes/teams$total_shots
teams$`Percent_shots_NC3` <- teams$non_corner_threes/teams$total_shots
#get shot zone eFG%
teams$`eFG%_2PT` <- teams$two_pointers_made/teams$two_pointers
teams$`eFG%_C3` <- (teams$corner_threes_made + (0.5*teams$corner_threes_made)) / teams$corner_threes
teams$`eFG%_NC3` <- (teams$non_corner_threes_made + (0.5*teams$non_corner_threes_made)) / teams$non_corner_threes


# ANSWERS##
# Team A: P
#
# 
# 
# 
# 
# 
# 
