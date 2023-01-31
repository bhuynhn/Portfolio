#install required packages
library(tidyverse)

#load dataset
all_trip <- read_csv("Source-data/GOOG_DA_P1/2022_consolidated_clean_tripdata.csv")

#descriptive analysis on ride_length (in minutes)
mean(all_trip$ride_length)
median(all_trip$ride_length)
max(all_trip$ride_length)
min(all_trip$ride_length)

#summarize ride length group by member
by_member <- all_trip %>% 
  filter(all_trip$ride_length > 0) %>%
  group_by(usertype) %>%
  summarize(mean = mean(ride_length),
            max = max(ride_length),
            min = min(ride_length),
            median = median(ride_length))

by_member %>% write.csv("Source-data/GOOG_DA_P1/group_by_mem.csv")

#summarize ride length group by member and day of week
by_member_dow <- all_trip %>% 
  filter(ride_length > 0) %>%
  group_by(usertype, day_of_week) %>%
  summarize(mean = mean(ride_length),
            max = max(ride_length),
            min = min(ride_length),
            median = median(ride_length))

by_member_dow$day_of_week <- factor(by_member_dow$day_of_week, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#fix sorting order for week day
by_member_dow[order(by_member_dow$day_of_week), ]
 
by_member_dow %>% write.csv("Source-data/GOOG_DA_P1/group_by_mem_dow.csv")

#summarize ride length group by member and month
by_member_month <- all_trip %>% 
  filter(ride_length > 0) %>%
  group_by(usertype, month) %>%
  summarize(mean = mean(ride_length),
            max = max(ride_length),
            min = min(ride_length),
            median = median(ride_length))

by_member_month %>% write.csv("Source-data/GOOG_DA_P1/group_by_mem_month.csv")