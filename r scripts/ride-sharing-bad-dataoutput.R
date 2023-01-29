#install required packages
library(tidyverse)
library(lubridate)
library(ggplot2)

#function to select and rename variables
clean_df <- function(x){
  df <- rename(x,
               trip_id = ride_id,
               bikeid = rideable_type,
               start_time = started_at,
               end_time = ended_at,
               from_station_name = start_station_name,
               from_station_id = start_station_id,
               to_station_name = end_station_name,
               to_station_id = end_station_id,
               usertype = member_casual,
  )
  df
}

#load dataset from csv and rename columns
jan_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202201-divvy-tripdata.csv"))
feb_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202202-divvy-tripdata.csv"))
mar_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202203-divvy-tripdata.csv"))
apr_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202204-divvy-tripdata.csv"))
may_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202205-divvy-tripdata.csv"))
jun_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202206-divvy-tripdata.csv"))
jul_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202207-divvy-tripdata.csv"))
aug_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202208-divvy-tripdata.csv"))
sep_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202209-divvy-publictripdata.csv"))
nov_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202210-divvy-tripdata.csv"))
oct_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202211-divvy-tripdata.csv"))
dec_22 <- clean_df(read_csv("Source-data/GOOG_DA_P1/202212-divvy-tripdata.csv"))

#stack individual month data frames into one
all_trip <- bind_rows(jan_22, feb_22, mar_22, apr_22, may_22, jun_22, jul_22, aug_22, sep_22, nov_22, oct_22, dec_22) %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

#calculate ride length in minutes
all_trip$ride_length <- difftime(all_trip$end_time,all_trip$start_time) / 60

#bad data return negative length time, 100 observation identified, output to csv
bad <- all_trip %>%
  filter(all_trip$ride_length < 0) %>%
  write.csv(file = "Source-data/GOOG_DA_P1/2022_bad_tripdata.csv")




