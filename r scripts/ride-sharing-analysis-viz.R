#install required packages
library(tidyverse)
library(ggplot2)

#load dataset
all_trip <- read_csv("Source-data/GOOG_DA_P1/2022_consolidated_clean_tripdata.csv")

by_member <- read_csv("Source-data/GOOG_DA_P1/group_by_mem.csv")

by_member_dow <- read_csv("Source-data/GOOG_DA_P1/group_by_mem_dow.csv")

by_member_month <- read_csv("Source-data/GOOG_DA_P1/group_by_mem_month.csv")

#dataframe count trip by user
count_trip <- all_trip %>% 
  count(usertype) %>% 
  rename("User type"=usertype, "Total trip" = n)

#ggplot by_member_dow
by_member_dow %>% ggplot()

#ggplot by_member_month
by_member_month %>% ggplot()

