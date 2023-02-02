library(tidyverse)
library(scales)

#load data
jan_22 <- read_csv("Source-data/GOOG_DA_P1/202201-divvy-tripdata.csv")
feb_22 <- read_csv("Source-data/GOOG_DA_P1/202202-divvy-tripdata.csv")
mar_22 <- read_csv("Source-data/GOOG_DA_P1/202203-divvy-tripdata.csv")
apr_22 <- read_csv("Source-data/GOOG_DA_P1/202204-divvy-tripdata.csv")
may_22 <- read_csv("Source-data/GOOG_DA_P1/202205-divvy-tripdata.csv")
jun_22 <- read_csv("Source-data/GOOG_DA_P1/202206-divvy-tripdata.csv")
jul_22 <- read_csv("Source-data/GOOG_DA_P1/202207-divvy-tripdata.csv")
aug_22 <- read_csv("Source-data/GOOG_DA_P1/202208-divvy-tripdata.csv")
sep_22 <- read_csv("Source-data/GOOG_DA_P1/202209-divvy-publictripdata.csv")
nov_22 <- read_csv("Source-data/GOOG_DA_P1/202210-divvy-tripdata.csv")
oct_22 <- read_csv("Source-data/GOOG_DA_P1/202211-divvy-tripdata.csv")
dec_22 <- read_csv("Source-data/GOOG_DA_P1/202212-divvy-tripdata.csv")


#stack individual month data frames into one
all_trips <- bind_rows(jan_22, feb_22, mar_22, apr_22, may_22, jun_22, jul_22, aug_22, sep_22, nov_22, oct_22, dec_22) %>% select(-c(start_lat, start_lng, end_lat, end_lng))


#select and format variable for analysis, adding variable 'ride_length' to calculate ride length of each trips
all_trips <- all_trips %>%
  select(rideable_type,
         started_at,
         ended_at,
         start_station_id,
         start_station_name,
         end_station_id,
         end_station_name,
         member_casual) %>%
  mutate(started_at = as.POSIXct(started_at),
         ended_at = as.POSIXct(ended_at),
         ride_length_secs = as.double(difftime(ended_at, started_at)))

#summary of ride length  
summary(all_trips$ride_length_secs)

#remove some bad data that return some negative ride length, 
all_trips <- all_trips %>% filter(all_trips$ride_length_secs > 0)

#visualize ride length
all_trips$ride_length_secs %>% log() %>%
  boxplot(main = "Log of Ride length",
           xlab = "log",medlty = 2,
           medlwd = 2,
           medpch = 22,
           medcex = 2,
           medcol = 2,
           medbg = 1,
           border = "darkred",
           horizontal = TRUE,
           frame = FALSE)

#summary of station
all_station_count <- c(all_trips$start_station_name, all_trips$end_station_name) %>%
  unique() %>%
  length()

all_station_count

#top 10 station where riders start from
top_10_start_station <- all_trips %>%
  mutate(start_station_name = fct_lump(start_station_name, 10)) %>% 
  count(start_station_id, start_station_name, name = "counts", sort = T) %>% 
  filter(!is.na(start_station_name),
         !is.na(start_station_id),
         start_station_name != "Other") %>%
  mutate(start_station_name = fct_reorder(start_station_name, counts))

#plot
top_10_start_plt <- ggplot(top_10_start_station) +
  geom_col(aes(start_station_name, counts, fill = start_station_name),
           width = 0.8,
           show.legend = FALSE) +
  scale_fill_viridis_d(option = "E", direction = -1) +
  coord_flip() +
  labs(title = "Top 10 Start Station",
       x = NULL,
       y = NULL)

#top 10 station where riders end from
top_10_end_station <- all_trips %>%
  mutate(end_station_name = fct_lump(end_station_name, 10)) %>% 
  count(end_station_id, end_station_name, name = "counts", sort = T) %>% 
  filter(!is.na(end_station_name),
         !is.na(end_station_id),
         end_station_name != "Other") %>%
  mutate(end_station_name = fct_reorder(end_station_name, counts))

#plot
top_10_end_plt <- ggplot(top_10_end_station) +
  geom_col(aes(end_station_name, counts, fill = end_station_name),
           width = 0.8,
           show.legend = FALSE) +
  scale_fill_viridis_d(option = "E", direction = -1) +
  coord_flip() +
  labs(title = "Top 10 End Station",
       x = NULL,
       y = NULL)

#comparison of trip by days of week
member_casual_col <- all_trips %>%
  select(member_casual, started_at) %>%
  drop_na(started_at, member_casual) %>%
  mutate(weekday = wday(started_at)) %>%
  group_by(weekday, member_casual) %>%
  summarise(counts = n(),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(desc(counts)) %>%
  ggplot() +
  geom_line(aes(weekday, counts, color = member_casual),
            linewidth = 2) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),
                     labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  scale_color_manual(
    name = NULL,
    values = c("casual" = "#ffc300", "member" = "#003566"),
    labels = c("Casual", "Member")
  ) +
  theme(legend.position = "bottom") +
  labs(title = "Rides by day of week and type of customers",
       x = NULL,
       y = NULL)

#comparison of trip count by weekday and weekend
x <- all_trips %>%
  select(member_casual, started_at) %>%
  drop_na(started_at, member_casual) %>%
  mutate(
    weekday = wday(started_at, label = TRUE),
    weekday_weekend = if_else(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday"),
    started_at_hour = hour(started_at)
  )  %>%
  count(started_at_hour, member_casual, weekday_weekend, name = "counts") %>%
  ggplot(aes(started_at_hour, y = counts, color = member_casual)) +
  geom_line(size = 1.3) +
  facet_wrap(vars(weekday_weekend), ncol = 1) +
  scale_color_manual(values = c("casual" = "#ffc300", "member" = "#003566"),
                     labels = c("Casual", "Member")) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(strip.background = element_rect(fill = "#001d3d")) +
  theme(strip.text = element_text(
    colour = 'white',
    size = 11,
    face = "bold"
  )) +
  labs(
    title = "Weekday VS Weekend Trips By Time Of Day",
    caption = "Data Source: Divvy Chicago Bike Sharing Data",
    x = "Time",
    y = "Counts",
    color = ""
  )


all_trips %>%
  count(week = floor_date(started_at, "week"),
        rideable_type,
        name = "counts") %>%
  ggplot() +
  geom_line(aes(x = week, y = counts, color = rideable_type), size = 1) +
  theme_minimal() +
  facet_wrap(vars(rideable_type)) +
  scale_color_manual(
    values = c(
      "classic_bike" = "#7c0d0e",
      "docked_bike" = "#26453e",
      "electric_bike" = "#ffa500"
    )
  ) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
  theme(strip.text = element_blank())+
  labs(title = "Weekly Rides distribution by Types of rideables",
       caption = "Data Source: Kaggle | Divvy Chicago Bike Sharing Data",
       x = "",
       y = "Counts")