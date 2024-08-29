install.packages('tidyverse')
library(tidyverse)
install.packages(c(
  "here", "hms", "janitor", "lubridate", "skimr", "gridExtra", "paletteer",
  "crayon", "stringr", "personograph", "patchwork", "ggthemes", "ggforce",
  "ggbeeswarm", "knitr",
))
library("here")
library("hms")
library("janitor")
library("lubridate")
library("skimr")
library("gridExtra")
library("paletteer")
library("crayon")
library("stringr")
library("patchwork")
library("ggthemes")
library("ggforce")
library("ggbeeswarm")
library("personograph")
library("knitr")
library(readr)
#loading data
daily_activity <- read.csv("dailyActivity_merged.csv")
daily_calories <- read.csv("dailyCalories_merged.csv")
daily_intensities <- read.csv("dailyIntensities_merged.csv")
daily_steps <- read.csv("dailySteps_merged.csv")
heart_rate <- read.csv("heartrate_seconds_merged.csv")
hourly_calories <- read.csv("hourlyCalories_merged.csv")
hourly_intensities <- read.csv("hourlyIntensities_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
minutes_calories <- read.csv("minuteCaloriesWide_merged.csv")
minutes_intensities <- read.csv("minuteIntensitiesWide_merged.csv")
minutes_steps <- read.csv("minuteStepsWide_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
weight_log_info <- read.csv("weightLogInfo_merged.csv")
minute_sleep <- read.csv("minuteSleep_merged.csv")
#process_data
#daily_dataset
head(dailyActivity_merged)
colnames(dailyActivity_merged)

head(dailyCalories_merged)
colnames(dailyCalories_merged)

head(dailyIntensities_merged)
colnames(dailyIntensities_merged)

head(dailySteps_merged)
colnames(dailySteps_merged)

#heart_rate_dataset

head(heartrate_seconds_merged)
colnames(heartrate_seconds_merged)

#hourly_dataset

head(hourlyCalories_merged)
colnames(hourlyCalories_merged)

head(hourlyIntensities_merged)
colnames(hourlyIntensities_merged)

head(hourlySteps_merged)
colnames(hourlySteps_merged)

#minutes_dataset

head(minuteCaloriesWide_merged)
colnames(minuteCaloriesWide_merged)

head(minuteIntensitiesWide_merged)
colnames(minuteIntensitiesWide_merged)

head(minuteStepsWide_merged)
colnames(minuteStepsWide_merged)

head(minuteSleep_merged)
colnames(minuteSleep_merged)

#sleep_day_dataset

head(sleepDay_merged)
colnames(sleepDay_merged)

#weight_log_info_dataset
head(weightLogInfo_merged)
colnames(weightLogInfo_merged)

#selecting_a_particular_column_from_daily_dataset
dailyActivity_merged %>%
  select(id, activity_date)
dailyCalories_merged %>%
  select(Id, ActivityDay)
dailyIntensities_merged %>%
  select(Id, ActivityDay)
dailySteps_merged %>%
  select(Id, ActivityDay)
sleepDay_merged %>%
  select(Id, SleepDay)
weightLogInfo_merged %>%
  select(Id, Date)

#selecting_a_particular_column_from_hourly_dataset
hourlyCalories_merged %>%
  select(Id, ActivityHour)
hourlyIntensities_merged %>%
  select(Id, ActivityHour)
hourlySteps_merged %>%
  select(Id, ActivityHour)

#selecting_a_particular_column_from_minute_dataset

minuteSleep_merged %>%
  select(Id, date)

#renaming_the_variables_and_formatting_the_date_in_daily_dataset
dailyActivity_merged <-
  dailyActivity_merged %>% 
  rename(
    activity_date = ActivityDate, 
    total_steps = TotalSteps, 
    total_distance = TotalDistance,
    tracker_distance = TrackerDistance,
    logged_activities_d = LoggedActivitiesDistance,
    very_active_d = VeryActiveDistance, 
    moderately_active_d = ModeratelyActiveDistance, 
    light_active_d = LightActiveDistance, 
    sedentary_d = SedentaryActiveDistance, 
    very_active_m = VeryActiveMinutes, 
    fairly_active_m = FairlyActiveMinutes, 
    lightly_active_m = LightlyActiveMinutes, 
    sedentary_m = SedentaryMinutes, 
    calories = Calories
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    # reformat variable as POSIXct to represent date and time
    activity_date = parse_date_time(activity_date, "%m/%d/%y"),
    # create new variable and format as date only
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    # create new variables for day of week and time of week
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )

# daily_calories ---------------------------
dailyCalories_merged <-
  dailyCalories_merged %>% 
  rename(
    activity_date = ActivityDay,
    calories = Calories
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# daily_intensities ---------------------------
dailyIntensities_merged <-
  dailyIntensities_merged %>% 
  rename(
    activity_date = ActivityDay,
    very_active_d = VeryActiveDistance, 
    moderately_active_d = ModeratelyActiveDistance, 
    light_active_d = LightActiveDistance, 
    sedentary_d = SedentaryActiveDistance, 
    very_active_m = VeryActiveMinutes, 
    fairly_active_m = FairlyActiveMinutes, 
    lightly_active_m = LightlyActiveMinutes, 
    sedentary_m = SedentaryMinutes, 
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# daily_sleep ---------------------------
sleepDay_merged <-
  sleepDay_merged %>% 
  rename(
    activity_date = SleepDay,
    total_sleep_records = TotalSleepRecords,
    total_minutes_asleep = TotalMinutesAsleep,
    total_time_in_bed = TotalTimeInBed
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# daily_steps ---------------------------
dailySteps_merged <-
  dailySteps_merged %>% 
  rename(
    activity_date = ActivityDay,
    step_total = StepTotal
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend")
  )
# weight_log ---------------------------
weightLogInfo_merged <-
  weightLogInfo_merged %>% 
  rename(
    activity_date = Date,
    weight_kg = WeightKg,
    weight_lb = WeightPounds,
    fat = Fat,
    bmi = BMI,
    manual_report = IsManualReport,
    log_id = LogId
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    activity_time = format(activity_date, format = "%I:%M:%S %p"), 
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_date, format = "%I:%M:%S %p")
  )

#renaming_and_formatting_hourly_dataset
# hourly_calories ---------------------------
hourlyCalories_merged <-
  hourlyCalories_merged %>% 
  rename(
    activity_hour = ActivityHour,
    calories = Calories
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_hour = parse_date_time(activity_hour, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_hour, "%Y/%m/%d"), 
    activity_time = format(activity_hour, format = "%I:%M:%S %p"),
    day_of_week = weekdays(as.Date(activity_hour)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p"))

# hourly_intensities ---------------------------
hourlyIntensities_merged <- 
  hourlyIntensities_merged %>% 
  rename(
    activity_hour = ActivityHour,
    total_intensity = TotalIntensity,
    average_intensity = AverageIntensity
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_hour = parse_date_time(activity_hour, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_hour, "%Y/%m/%d"), 
    activity_time = format(activity_hour, format = "%I:%M:%S %p"),
    day_of_week = weekdays(as.Date(activity_hour)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p")
  )
# hourly_steps --------------------------- 
hourlySteps_merged <- 
  hourlySteps_merged %>% 
  rename(
    activity_hour = ActivityHour,
    step_total = StepTotal
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_hour = parse_date_time(activity_hour, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_hour, "%Y/%m/%d"), 
    activity_time = format(activity_hour, format = "%I:%M:%S %p"),
    day_of_week = weekdays(as.Date(activity_hour)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p")
  )

#renaming_and_formatting_minute_dataset
# minute_sleep ---------------------------
minuteSleep_merged <-
  minuteSleep_merged %>% 
  rename(
    activity_date = date,
    sleep_value = value,
    log_id = logId
  ) %>% 
  rename_with(
    tolower, starts_with("Id")
  ) %>% 
  mutate(
    activity_date = parse_date_time(activity_date, "%m/%d/%Y %I:%M:%S %p"),
    activity_date_ymd = as.Date(activity_date, "%Y/%m/%d"),
    activity_time = format(activity_date, format = "%I:%M:00 %p"), 
    day_of_week = weekdays(as.Date(activity_date)),
    time_of_week = case_when(
      day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
      day_of_week %in% c("Saturday", "Sunday") ~ "Weekend"),
    hour_of_day = as.POSIXct(activity_time, format = "%I:%M:%S %p"),
    # sleep_id will be used generate totals per sleep log
    sleep_id = str_c(id, "-", log_id), 
    # Create new variables for sleep values - asleep, restless, and awake
    asleep = ifelse(sleep_value == 1, 1, 0),
    restless = ifelse(sleep_value == 2, 1, 0),
    awake = ifelse(sleep_value == 3, 1, 0)
  )
# Create sleep_summary_0 df ---------------------------
# that shows totals for 3 sleep values per sleep log
sleep_summary_0 <-
  minuteSleep_merged %>%
  # id_date will be used to generate a daily total
  mutate(
    id_date = str_c(id, "-", activity_date_ymd)
  ) %>% 
  group_by(sleep_id, activity_date_ymd, id_date, id) %>% 
  dplyr::summarize(
    total_asleep = sum(sleep_value == "1"),
    total_restless = sum(sleep_value == "2"),
    total_awake = sum(sleep_value == "3")
  )
# Create sleep_summary df ---------------------------
# that combines totals for each day per id
sleep_summary <-
  sleep_summary_0 %>%
  # activity_date will be used to merge with daily_sleep df
  mutate(
    activity_date = parse_date_time(activity_date_ymd, "%Y/%m/%d")
  ) %>%
  group_by(id_date, activity_date, id) %>%
  dplyr::summarize(
    total_asleep_merged = sum(total_asleep),
    total_restless_merged = sum(total_restless),
    total_awake_merged = sum(total_awake)
  )
# Merge these two daily sleep dfs into one ---------------------------
sleep_data <- merge(x = sleepDay_merged, y = sleep_summary, by = c("id", "activity_date"), all = TRUE)

#Analyzing_the_data

#Daily_dataset
# There are 33 users (one user per unique id) in the daily activity df
n_distinct(dailyActivity_merged$id)
# There are 24 users (one user per unique id) in the sleep dfs
n_distinct(sleepDay_merged$id)
n_distinct(sleep_data$id)
# There are 8 users (one user per unique id) in the weight df
n_distinct(weightLogInfo_merged$id)

#hourly_dataset
# There are 33 users (one user per unique id) in the hourly dfs
n_distinct(hourlyCalories_merged$id)
n_distinct(hourlyIntensities_merged$id)
n_distinct(hourlySteps_merged$id)

#minute_dataset
# There are 24 users (one user per unique id) in the minute df
n_distinct(minuteSleep_merged$id)

#total_observation_in_each_dataset
#daily_dataset
# The observations vary across the daily dfs
nrow(dailyActivity_merged)#940
nrow(dailySteps_merged)#940
nrow(sleep_data)#452
nrow(weightLogInfo_merged)#67

#hourly_dataset
# There are 22099 observations in each hourly dataframe
nrow(hourlyCalories_merged)
nrow(hourlyIntensities_merged)
nrow(hourlySteps_merged)

#minutes_dataset
# There are 188521 observations in the minute dataframe
nrow(minuteSleep_merged)

#Quick_overview
#daily_dataset
# Daily totals for steps, distance, calories ---------------------------
dailyActivity_merged %>%
  select(
    total_steps,
    total_distance,
    calories
  ) %>%
  summary()
# Active minute levels per category ---------------------------
dailyActivity_merged %>%
  select(
    very_active_m,
    fairly_active_m,
    lightly_active_m,
    sedentary_m
  ) %>%
  summary()
# Sleep totals ---------------------------
# for records, minutes asleep, and time in bed
sleepDay_merged %>%
  select(
    total_sleep_records,
    total_minutes_asleep,
    total_time_in_bed
  ) %>%
  summary()
# Sleep totals per category ---------------------------
sleep_data %>%
  select(
    total_minutes_asleep, 
    total_time_in_bed,
    total_asleep_merged,
    total_restless_merged,
    total_awake_merged
  ) %>%
  summary()
# Weight totals ---------------------------
weightLogInfo_merged %>%
  select(
    weight_lb,
    fat,
    bmi
  ) %>%
  summary()

#hourly_dataset
# Hourly calories summary ---------------------------
hourlyCalories_merged %>%
  select(calories) %>%
  summary()
# Hourly intensities summary ---------------------------
hourlyIntensities_merged %>%
  select(
    total_intensity,
    average_intensity
  ) %>%
  summary()
# Hourly steps summary ---------------------------
hourlySteps_merged %>%
  select(step_total) %>%
  summary()

#minute_dataset
# Minute sleep summary ---------------------------
minuteSleep_merged %>%
  select(
    sleep_value,
    asleep,
    restless,
    awake
  ) %>%
  summary()

#Data_plotting
#Steps and active minutes 
# Total steps vs calories plot ---------------------------
ggplot(data = dailyActivity_merged, aes(x = total_steps, y = calories)) +
  geom_point() +
  geom_smooth() + # Trend line with a shadow representing  95% confidence interval
  labs(title = "Total steps vs calories") +
  ylab("Calories") +
  xlab("Total Steps") +
  theme_minimal()
cor.test(dailyActivity_merged$total_steps, dailyActivity_merged$calories, method = "pearson")

# Very active minutes vs calories plot ---------------------------
ggplot(data = dailyActivity_merged, aes(x = very_active_m, y = calories)) +
  geom_point() +
  geom_smooth() + # Trend line with a shadow representing  95% confidence interval
  labs(title = "Very active minutes vs calories") +
  ylab("Calories") +
  xlab("Very active minutes") +
  theme_minimal()
cor.test(dailyActivity_merged$very_active_m, dailyActivity_merged$calories, method = "pearson")

# Lightly active minutes vs calories plot ---------------------------
ggplot(data = dailyActivity_merged, aes(x = lightly_active_m, y = calories)) +
  geom_point() +
  geom_smooth() + # Trend line with a shadow representing  95% confidence interval
  labs(title = "Lightly active minutes vs calories") +
  ylab("Calories") +
  xlab("Lightly active minutes") +
  theme_minimal()
cor.test(dailyActivity_merged$lightly_active_m, dailyActivity_merged$calories, method = "pearson")

# Daily calories
# Create labels and limits for plot ---------------------------
labels_weekdays <- c(
  "Monday" = "Monday", "Tuesday" = "Tuesday",
  "Wednesday" = "Wednesday", "Thursday" = "Thursday",
  "Friday" = "Friday", "Saturday" = "Saturday",
  "Sunday" = "Sunday"
)
limits_weekdays <- c(
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
  "Saturday", "Sunday"
)
# Calories vs day of week plot ---------------------------
ggplot(data = dailyActivity_merged) +
  geom_point(
    aes(x = day_of_week, y = calories, color = as.factor(time_of_week)),
    position = "jitter", 
    alpha = 0.3
  ) +
  labs(title = "Daily calories throughout the week", color = "Time of week") +
  ylab("Total Calories") +
  scale_x_discrete(
    "Day",
    labels = labels_weekdays, 
    limits = limits_weekdays,
    guide = guide_axis(angle = 45)
  ) +
  stat_summary(
    aes(x = day_of_week, y = calories),
    fun = mean, 
    geom = "point",
    color = "red", 
    size = 2, 
    alpha = 0.7
  ) +
  theme_minimal()

# Daily sleep 
# Total minutes asleep vs time of week ---------------------------
ggplot(data = sleep_data) +
  geom_point(
    aes(
      x = weekdays.POSIXt(activity_date), 
      y = total_asleep_merged,
      color = as.factor(time_of_week)
    ),
    position = "jitter",
    alpha = 0.3
  ) +
  labs(title = "Total minutes asleep throughout week") +
  guides(color = "none") +
  ylab("Minutes asleep") +
  scale_x_discrete(
    "Day",
    labels = labels_weekdays, 
    limits = limits_weekdays,
    guide = guide_axis(angle = 45)
  ) +
  stat_summary(
    aes(x = weekdays.POSIXt(activity_date), y = total_asleep_merged),
    fun = mean, 
    geom = "point", 
    color = "red", 
    size = 2,
    alpha = 0.7
  ) +
  theme_minimal()

# Total minutes restless vs time of week ---------------------------
ggplot(data = sleep_data) +
  geom_point(
    aes(
      x = weekdays.POSIXt(activity_date), 
      y = total_restless_merged,
      color = as.factor(time_of_week)
    ),
    position = "jitter",
    alpha = 0.3
  ) +
  labs(title = "Total minutes restless throughout week") +
  guides(color = "none") +
  ylab("Minutes restless") +
  scale_x_discrete(
    "Day",
    labels = labels_weekdays, 
    limits = limits_weekdays,
    guide = guide_axis(angle = 45)
  ) +
  stat_summary(
    aes(x = weekdays.POSIXt(activity_date), y = total_restless_merged),
    fun = mean, 
    geom = "point", 
    color = "red", 
    size = 2,
    alpha = 0.7
  ) +
  theme_minimal()

# Total minutes awake vs time of week ---------------------------
ggplot(data = sleep_data) +
  geom_point(
    aes(
      x = weekdays.POSIXt(activity_date), 
      y = total_awake_merged,
      color = as.factor(time_of_week)
    ),
    position = "jitter",
    alpha = 0.3
  ) +
  labs(title = "Total minutes awake throughout week") +
  guides(color = "none") +
  ylab("Minutes awake") +
  scale_x_discrete(
    "Day",
    labels = labels_weekdays, 
    limits = limits_weekdays,
    guide = guide_axis(angle = 45)
  ) +
  stat_summary(
    aes(x = weekdays.POSIXt(activity_date), y = total_awake_merged),
    fun = mean, 
    geom = "point", 
    color = "red", 
    size = 2,
    alpha = 0.7
  ) +
  theme_minimal()

