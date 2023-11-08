library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("D:/Q3/") #setup

#================================================
#=====================
# STEP 1: COLLECT DATA
#=====================

#Uploading 2023(Q3) july,aug,sep datasets
July_2023 <- read_csv("July_data.csv")
August_2023 <- read_csv("August_data.csv")
September_2023 <- read_csv("September_data.csv")

#=================================================
#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

colnames(July_2023)
colnames(August_2023)
colnames(September_2023)

#=================================================

#Inspect the dataframes and look for inconguencies
str(September_2023)
str(August_2023)
str(July_2023)

July_2023 <-  mutate(July_2023, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type)) 
August_2023<-  mutate(August_2023, ride_id = as.character(ride_id) ,rideable_type = as.character(rideable_type)) 
September_2023 <- mutate(September_2023, ride_id = as.character(ride_id) ,rideable_type = as.character(rideable_type)) 
#===================================================

# Stack individual month's data frames into one big data frame

all_trips <- bind_rows(July_2023,August_2023,September_2023)

#======================================================

#Remove start_lat, start_lng, end_lat, end_lng

all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

colnames(all_trips)  
nrow(all_trips)  
dim(all_trips)  
head(all_trips)  
str(all_trips)
summary(all_trips)  

#Add date, day, month, year of each rides in separate columns

#time is mentioned,calculate the ride_length in seconds
all_trips$ended_at <- as.POSIXct(all_trips$ended_at, format = "%d-%m-%Y %H:%M")
all_trips$started_at <- as.POSIXct(all_trips$started_at, format = "%d-%m-%Y %H:%M")

#to cALCULATE the day rides started
all_trips$date <- as.Date(all_trips$started_at) #yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


# Remove "bad" data
# Create a new data frame excluding the ride_length<0 and start_station != end_station when ride_length == 0
all_trips_na <- all_trips[all_trips$started_at <= all_trips$ended_at & !(all_trips$start_station_name != all_trips$end_station_name & all_trips$ride_length==0),]


# Create a new DataFrame with rows where all columns have NA values
all_trips_v2 <- all_trips_na[!(is.na(all_trips_na$ride_id) & is.na(all_trips_na$rideable_type) & is.na(all_trips_na$started_at)& is.na(all_trips_na$ended_at)& is.na(all_trips_na$start_station_name)& is.na(all_trips_na$end_station_name)& is.na(all_trips_na$start_station_id )& 
                                is.na(all_trips_na$end_station_id)& is.na(all_trips_na$member_casual)& 
                                is.na(all_trips_na$day)& is.na(all_trips_na$month)& is.na(all_trips_na$date)& is.na(all_trips_na$day_of_week)& is.na(all_trips_na$year)& is.na(all_trips_na$ride_length)), ]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Descriptive analysis on ride_length (all figures in seconds)
average_ridelength <- mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median_ridelength <- median(all_trips_v2$ride_length)
maximum_ridelength <- max(all_trips_v2$ride_length)

# most used bike overall

mode_result <- all_trips_v2 %>%
  group_by(rideable_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count == max(count)) %>%
  select(rideable_type)

mode_rided_bike_overall <- mode_result$rideable_type
print(mode_rided_bike_overall)

#most used bike by each rider type

mode_ridedBike_byeach_ridertype <- all_trips_v2 %>%
  group_by(member_casual, rideable_type) %>%
  summarise(count = n()) %>%
  arrange(member_casual, desc(count)) %>%
  slice_max(order_by = count, n = 1) %>%
  select(member_casual, rideable_type, count)

print(mode_ridedBike_byeach)

#most bike rode day_of_week by each rider type 

mode_day_of_week <- all_trips_v2 %>%
  group_by(member_casual, day_of_week) %>%
  summarise(count = n()) %>%
  arrange(member_casual, desc(count)) %>%
  slice_max(order_by = count, n = 1) %>%
  select(member_casual, day_of_week, count)

print(mode_day_of_week)


min_ridelength <- min(all_trips_v2$ride_length)
summary(all_trips_v2$ride_length)
str(all_trips_v2)

# Compare members and casual users

avgof_member_causal_ridelength <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
median_member_causal_ridelength <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
max_ridelength_member_causal <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
min_ridelength_member_causal <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
avg_ridelength_day_of_week_bymember_causal <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# analyze ridership data by type and weekday
ride_summary_by_day_and_rider_type <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()	
            ,average_duration = mean(ride_length)) %>%  
  arrange(member_casual, weekday)	

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("#90EE90", "pink")) +
  labs(fill = "Rider Type")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("skyblue", "#DEB887")) +
  labs(fill = "Rider Type")


counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

 ridelength_based <- data.frame(
  average_ridelength,median_ridelength,maximum_ridelength,min_ridelength
  )
 bike_based <- data.frame(
   mode_rided_bike_overall,mode_ridedBike_byeach_ridertype
 )
  most_rided_day_byeach <- data.frame(mode_day_of_week)
  
  avgof_member_causal_ridelength <- data.frame(avgof_member_causal_ridelength)
  median_member_causal_ridelength <- data.frame(median_member_causal_ridelength)
  max_ridelength_member_causal <- data.frame(max_ridelength_member_causal)
  min_ridelength_member_causal <- data.frame(min_ridelength_member_causal)
  avg_ridelength_day_of_week_bymember_causal <- data.frame(avg_ridelength_day_of_week_bymember_causal)
  ride_summary_by_day_and_rider_type <- data.frame(ride_summary_by_day_and_rider_type)

# Save the data to a CSV file
write.csv(counts, file = 'D:/Q3/mydata.csv', row.names = FALSE)

write.csv(ridelength_based, "ridelength_based.csv", row.names = FALSE)
write.csv(bike_based, "bike_based.csv", row.names = FALSE)
write.csv(most_rided_day_byeach, "most_rided_day_byeach.csv", row.names = FALSE)
write.csv(avgof_member_causal_ridelength, "avgof_member_causal_ridelength.csv", row.names = FALSE)
write.csv(median_member_causal_ridelength, "median_member_causal_ridelength.csv", row.names = FALSE)
write.csv(max_ridelength_member_causal, "max_ridelength_member_causal.csv", row.names = FALSE)
write.csv(min_ridelength_member_causal, "min_ridelength_member_causal.csv", row.names = FALSE)
write.csv(avg_ridelength_day_of_week_bymember_causal, "avg_ridelength_day_of_week_bymember_causal.csv", row.names = FALSE)
write.csv(ride_summary_by_day_and_rider_type, "ride_summary_by_day_and_rider_type.csv", row.names = FALSE)


