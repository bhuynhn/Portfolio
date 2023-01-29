#install required packages
library(tidyverse)
library(lubridate)
library(ggplot2)

#load dataset
all_trip <- read_csv("Source-data/GOOG_DA_P1/2022_consolidated_clean_tripdata.csv")

#descriptive analysis on ride_length (in minutes)
mean(all_trip$ride_length)
median(all_trip$ride_length)
max(all_trip$ride_length)
min(all_trip$ride_length)

#summarize ride length group by member
by_member <- all_trip %>% group_by(usertype) %>%
  summarize(mean = mean(ride_length),
            max = max(ride_length),
            min = min(ride_length))

#summarize ride length group by member and day of week
by_member_dow <- all_trip %>% group_by(usertype, day_of_week) %>%
  summarize(mean = mean(ride_length),
            max = max(ride_length),
            min = min(ride_length))

by_member_dow$day_of_week <- factor(by_member_dow$day_of_week, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#fix sorting order for week day
by_member_dow[order(by_member_dow$day_of_week), ]
 
#summarize ride length group by member and month
by_member_month <- all_trip %>% group_by(usertype, month) %>%
  summarize(mean = mean(ride_length),
            max = max(ride_length),
            min = min(ride_length))
