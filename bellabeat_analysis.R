library(tidyverse)


heart<-read.csv("heartrate_seconds_merged.csv")
daily_activity<-read.csv("dailyActivity_merged.csv")
hourly_steps <-read.csv("hourlySteps_merged.csv")
hourly_intensity<-read.csv("hourlyIntensities_merged.csv")

# Checking Structure
str(daily_activity)
str(hourly_intensity)
str(hourly_steps)

# Checking number of users
n_distinct(daily_activity$Id)
n_distinct(hourly_intensity$Id)
n_distinct(hourly_steps$Id)

# Checking for duplicates
sum(duplicated(daily_activity))
sum(duplicated(hourly_steps))
sum(duplicated(hourly_intensity))

# Date needs to be changed to datetime
daily_activity$ActivityDate <- mdy(daily_activity$ActivityDate)
hourly_intensity$ActivityHour<-as.POSIXct(hourly_intensity$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_intensity <- mutate(hourly_intensity, ActivityDate = as.Date(ActivityHour))
# Ran into an issue where midnight(00:00:00) was being changed to NA so I found this solution.
hourly_intensity<- mutate(hourly_intensity, ActivityTime = sprintf("%02d:%02d:%02d", hour(ActivityHour), minute(ActivityHour), second(ActivityHour)))
hourly_steps$ActivityHour<-as.POSIXct(hourly_steps$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_steps <- mutate(hourly_steps, ActivityDate = as.Date(ActivityHour))
hourly_steps<- mutate(hourly_steps, ActivityTime = sprintf("%02d:%02d:%02d", hour(ActivityHour), minute(ActivityHour), second(ActivityHour)))
# Merging the two hourly data frames for later analysis
hourly_steps_intensity <- merge(hourly_intensity,hourly_steps,by=c("Id","ActivityDate","ActivityTime","ActivityHour"))
# Dropping the ActivityHour column as it was split into ActivityDate and ActivityTime
hourly_steps_intensity <- select(hourly_steps_intensity, -ActivityHour)
str(hourly_steps_intensity)
str(daily_activity)

# Adding day of the week for analysis
daily_activity <- daily_activity %>%
  mutate(DayOfWeek = weekdays(ActivityDate))

hourly_steps_intensity <- hourly_steps_intensity %>%
  mutate(DayOfWeek = weekdays(ActivityDate))
View(daily_activity)
View(hourly_steps_intensity)
value_counts<-table(hourly_steps_intensity$ActivityTime)
print(value_counts)

# There are 79 entries with 1440 SedentaryMinutes but 7 that show a non-zero TotalSteps.
filtered_daily_activity <- filter(daily_activity, SedentaryMinutes == 1440)
View(filtered_data)
# I referenced the hourly_steps for these instances by creating a filter from the daily_activity dataframe and found no recorded steps.  
sedentary_dates <- daily_activity %>% 
  filter(SedentaryMinutes == 1440 & TotalSteps != 0) %>% 
  select(Id,ActivityDate)
View(sedentary_dates)
hourly_steps_filtered <- hourly_steps %>%
  semi_join(sedentary_dates, by = c("Id", "ActivityDate"))
View(hourly_steps_filtered)
# Therefore, I'm going to set these values to zero as it seems like a data collection or entry error.
daily_activity<- mutate(daily_activity,
                        TotalSteps = ifelse(SedentaryMinutes == 1440, 0, TotalSteps),
                        TotalDistance = ifelse(SedentaryMinutes == 1440, 0, TotalDistance),
                        TrackerDistance = ifelse(SedentaryMinutes == 1440, 0, TrackerDistance)
                        )
# Checking to make sure the issue is resolved

sedentary_dates <- daily_activity %>% 
  filter(SedentaryMinutes == 1440 & TotalSteps != 0) %>% 
  select(Id,ActivityDate)
View(sedentary_dates)
# Time to checkout the usage statistics.
daily_activity <- daily_activity %>%
  mutate(
    Usage = ifelse(SedentaryMinutes == 1440, 0, (SedentaryMinutes + LightlyActiveMinutes + FairlyActiveMinutes + VeryActiveMinutes) / 1440 * 100)
  )


daily_activity <- daily_activity %>%
  mutate(Usage_Group = case_when(
    Usage == 0 ~ "Did not wear",
    Usage > 0 & Usage < 50 ~ "<50%",
    Usage >= 50 & Usage < 75 ~ "50-74%",
    Usage >= 75 & Usage < 100 ~ "75-99%",
    TRUE ~ "All day"
  ))

usage_counts <- daily_activity %>%
  count(Usage_Group)

# Calculate the total number of rows in the dataset
total_rows <- nrow(daily_activity)

# Calculate the percentage of each Usage_Group
usage_counts <- usage_counts %>%
  mutate(Percentage = n / total_rows * 100)

# Print the usage_counts data frame
print(usage_counts)


usage_chart <- ggplot(usage_counts, aes(x = "", y = Percentage, fill = Usage_Group)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Daily Device Usage",
       fill = "Usage Group",
       y = NULL) +  
  theme_minimal() +
  theme(axis.text.x = element_blank(),  
        axis.ticks = element_blank(),  
        legend.position = "right") +   
  
  # Display percentage labels inside the pie slices
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), position = position_stack(vjust = 0.5))

print(usage_chart)


hourly_steps %>% 
  group_by(ActivityTime) %>% 
  summarise(average_steps = mean(StepTotal)) %>% 
  ggplot() +
  geom_col(mapping=aes(x=ActivityTime,y=average_steps,fill=average_steps))+
  labs(title="Average Hourly Steps",x="Hour",y="Steps")+
  scale_fill_gradient(low="blue",high="green")+
  theme(axis.text.x = element_text(angle = 90))

day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

daily_activity %>% 
  group_by(DayOfWeek) %>% 
  summarise(average_steps = mean(TotalSteps)) %>%
  mutate(DayOfWeek = factor(DayOfWeek, levels = day_order)) %>% 
  ggplot() +
  geom_col(mapping=aes(x=DayOfWeek,y=average_steps,fill=average_steps))+
  labs(title="Average Daily Steps",x="Day of the Week",y="Steps")+
  scale_fill_gradient(low="blue",high="green")+
  theme(axis.text.x = element_text(angle = 90))

hourly_steps_intensity %>% 
  group_by(ActivityTime) %>% 
  summarise(average_intensity = mean(AverageIntensity)) %>% 
  ggplot() +
  geom_col(mapping=aes(x=ActivityTime,y=average_intensity,fill=average_intensity))+
  labs(title="Average Hourly Intensity",x="Hour",y="Intensity")+
  scale_fill_gradient(low="blue",high="green")+
  theme(axis.text.x = element_text(angle = 90))

hourly_steps_intensity %>% 
  group_by(DayOfWeek) %>% 
  summarise(average_intensity = mean(AverageIntensity)) %>%
  mutate(DayOfWeek = factor(DayOfWeek, levels = day_order)) %>%
  ggplot() +
  geom_col(mapping=aes(x=DayOfWeek,y=average_intensity,fill=average_intensity))+
  labs(title="Average Hourly Intensity",x="Day of the Week",y="Intensity")+
  scale_fill_gradient(low="blue",high="green")+
  theme(axis.text.x = element_text(angle = 90))