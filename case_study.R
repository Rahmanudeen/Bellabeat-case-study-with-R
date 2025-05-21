#installing the packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("tidyr")
install.packages("dplyr")

# loading the packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# importing the file

activity <- read.csv("/Users/rahu/RStudio/dailyActivity_merged.csv")
calories <- read.csv("/Users/rahu/RStudio/hourlyCalories_merged.csv")
intensities <- read.csv("/Users/rahu/RStudio/hourlyIntensities_merged.csv")
sleep <- read.csv("/Users/rahu/RStudio/sleepDay_merged.csv")
weight <- read.csv("/Users/rahu/RStudio/weightLogInfo_merged.csv")

head(activity)
head(calories)
head(intensities)
head(sleep)
head(weight)

# fixing format

## intensities
intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")

## calories
calories$ActivityHour=as.POSIXct(calories$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

## activity
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")

## sleep
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")


# Exploring and summarizing data

n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

## activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

## explore num of active minutes per category

activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

## calories

calories %>%
  select(Calories) %>%
  summary()

## sleep

sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()

## weight

weight %>%
  select(WeightKg, BMI) %>%
  summary()


# merging the data

merged_data <- merge(sleep, activity, by=c('Id', 'date'))
head(merged_data)

# visualization

ggplot(data=activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")

ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ labs(title="Total Minutes Asleep vs. Total Time in Bed")

int_new <- intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

head(int_new)

ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")

ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='darkblue') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")