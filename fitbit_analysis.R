
### getting data

sleep_data <- read.csv("fitbitdata/sleepDay_merged.csv")
colnames(sleep_data)

sleep_data <-  rename(sleep_data, TotalMinutesInBed = TotalTimeInBed)

daily_activity <- read.csv("fitbitdata/dailyActivity_merged.csv")
str(daily_activity)

hourly_steps <- read.csv("fitbitdata/hourlySteps_merged.csv")
View(hourly_steps)

hourly_steps <- hourly_steps %>%
  mutate(
    ActivityHour = parse_date_time(
      ActivityHour, "m/d/Y I:M:S p"
    )
  ) %>% 
  mutate(
    Date = as.Date(ActivityHour),
    Time = format(ActivityHour, format = "%H:%M:%S")
  )





n_distinct(daily_activity)

n_distinct(sleep_data)

n_distinct(heart_rate)

n_distinct(weight_info)



###users on average are only sleeping 7 hours we could try to suggest going to sleep earlier so they can get 
###the recomended 8 hours of sleep
sleep_data%>%
 summarise(avgSleep = mean(TotalMinutesAsleep))

  
##### people normally walk the most around 6 pm so we can set reminders or encouragements at that time.
hourly_steps %>% 
  select(Time, StepTotal) %>% 
  group_by(Time) %>% 
  summarise(avg_steps = mean(StepTotal)) %>% 
  print(n=24)
 
###show relation between total steps and calories(positive, the more you walk the more you burn)
daily_activity %>% 
ggplot( aes(x= TotalSteps, y= Calories))+
  geom_point()

## show relation between total minutes asleep and how much total time in bed
ggplot(data = sleep_data, aes(x= TotalMinutesAsleep, y= TotalMinutesInBed, fill = "brown1"))+
  geom_point()+
  theme(legend.position = "none")



combined_data <- merge(sleep_data, daily_activity, by="Id")

 
  n_distinct(combined_data)

dassteps_bmi<- combined_data  %>% 
  group_by(BMI) %>% 
  summarise(average_steps = mean(TotalSteps))  
steps_bmi

  ggplot(data = steps_bmi,aes(y=TotalSteps, x= BMI))+
  geom_point()+
    geom_smooth()

  ggplot(data = combined_data,aes(y=SedentaryMinutes, x= TotalSteps))+
    geom_point()+
    geom_smooth()
  

## not enough data to see if more steps equals to more sleep.




 ############################ START DOING THE MARDOWN SHIT NEXT TIME KID

