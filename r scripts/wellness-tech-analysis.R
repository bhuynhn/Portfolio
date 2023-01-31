#loading library
library(tidyverse)
library(lubridate)

#loading csv files
activity <- read.csv("Source-data/GOOG_DA_P2/dailyActivity_merged.csv")
intensity <- read.csv("Source-data/GOOG_DA_P2/dailyIntensities_merged.csv")
intensityHr <- read.csv("Source-data/GOOG_DA_P2/hourlyIntensities_merged.csv")
sleep <- read.csv("Source-data/GOOG_DA_P2/sleepDay_merged.csv")
weight <- read.csv("Source-data/GOOG_DA_P2/weightLogInfo_merged.csv")

head(activity)

#format date/time stamp
#activity
activity$ActivityDate <- as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")  
#hourly intensity
intensityHr$ActivityHour <- as.POSIXct(intensityHr$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensityHr$time <- format(intensityHr$ActivityHour, format = "%H:%M:%S")
intensityHr$date <- format(intensityHr$ActivityHour, format = "%m/%d/%y")
#sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
#weight
weight$Date<- as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$time <- format(weight$Date, format = "%H:%M:%S")
weight$date <- format(weight$Date, format = "%m/%d/%y")

#summary of dataset
n_distinct(activity$Id)
n_distinct(intensity$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

#activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes,
         Calories) %>%
  summary()

activity %>%
  select(VeryActiveMinutes, 
         FairlyActiveMinutes, 
         LightlyActiveMinutes) %>%
  summary()

# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

sleepVsActivity <- merge(sleep, activity, by = c("Id", "date"))

#relationship between Sedentary and Sleep
ggplot(data = sleepVsActivity, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color= "#ffc300" ) + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")

#Intensity during the day
int <- intensityHr %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_int = mean(TotalIntensity))

ggplot(data=int, aes(x=time, y=mean_int)) + geom_histogram(stat = "identity", fill="#ffc300") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")
