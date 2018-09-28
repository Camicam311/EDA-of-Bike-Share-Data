#load packages
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(ggmap)
library(gridExtra)

#get data
bike_17 <- as_data_frame(read.csv("2017-fordgobike-tripdata.csv"))

glimpse(bike_17)
summary(bike_17)

#center the titles of graphs
theme_update(plot.title = element_text(hjust = 0.5))

#graph usertype and gender of rides
bike_17 %>%
  ggplot(aes(x=user_type, fill=member_gender)) +
  geom_bar(position="dodge")+
  ggtitle("Usertype and Gender of Rides")+
  xlab("user type")+
  scale_fill_manual(values=c("slateblue1", "slategray", "slateblue4", "slategray1"))

#usertype and gender counts
table(bike_17$user_type, bike_17$member_gender)

# create age column: bike_17_ages
bike_17_ages <- bike_17 %>%
  mutate(member_age=2017-member_birth_year) %>%
  filter(member_age<117) 

#histogram of ride ages
ggplot(bike_17_ages, aes(x=member_age)) + 
  geom_histogram(color="dark green", fill="light yellow", size=1) +
  ggtitle("Distribution of Ages") +
  xlab("age")

#median of ride ages
median(bike_17_ages$member_age)

#boxplots of ride ages for gender/usertype
bike_17_ages %>%
  ggplot(aes(x=member_gender, y=member_age)) +
  geom_boxplot(fill="white", color="darkred", size=1) +
  ylim(10,117) +
  facet_wrap(~user_type) + 
  ylab("age") + 
  xlab("gender") +
  coord_flip()

#gender/usertype median ride age values
bike_17_ages %>%
  group_by(member_gender, user_type) %>%
  summarise(median_age=median(member_age))

#add months column: bike_17_months
bike_17_months <- bike_17 %>%
  mutate(start_time= ymd_hms(start_time),
         month=floor_date(start_time, unit="1 month")) 

#graph ride count over months: bike_17_g1
bike_17_g1 <- bike_17_months %>%
  group_by(month) %>%
  summarise(total_monthly_rides=n()) %>%
  ggplot() +
  geom_bar(aes(x=month, y=total_monthly_rides), stat="identity", fill="orange") +
  geom_point(aes(x=month, y=total_monthly_rides)) +
  geom_line(aes(x=month, y=total_monthly_rides), size=1.5, col="orangered3")  +
  ylab("total rides") +
  ylim(0,110000)+
  ggtitle("Rides Count Over Time")

#graph subscriber and customer count over months: bike_17_g2
bike_17_g2 <- bike_17_months %>%
  group_by(month) %>%
  mutate(total_monthly_rides=n()) %>%
  count(total_monthly_rides, user_type, month) %>%
  ggplot() +
  geom_bar(aes(x=month, y=n, fill=user_type), stat="identity", position="dodge") +
  geom_point(aes(x=month, y=n, col=user_type)) +
  geom_line(aes(x=month, y=n, group=user_type), size=1.5)  + 
  ylab("number of rides") +
  ylim(0,110000)+
  ggtitle("Rides Count By Usertype Over Time")

#put graphs next to each other
grid.arrange(bike_17_g1, bike_17_g2, nrow = 1)

#load data for months in 2018
bike_jan18 <- as_data_frame(read.csv("201801-fordgobike-tripdata.csv"))
bike_feb18 <- as_data_frame(read.csv("201802-fordgobike-tripdata.csv"))
bike_mar18 <- as_data_frame(read.csv("201803-fordgobike-tripdata.csv"))
bike_apr18 <- as_data_frame(read.csv("201804-fordgobike-tripdata.csv"))
bike_may18 <- as_data_frame(read.csv("201805-fordgobike-tripdata.csv"))
bike_june18 <- as_data_frame(read.csv("201806-fordgobike-tripdata.csv"))
bike_july18 <- as_data_frame(read.csv("201807-fordgobike-tripdata.csv"))
bike_aug18 <- as_data_frame(read.csv("201808-fordgobike-tripdata.csv"))

#convert start station and end station id into integers
bike_june18$start_station_id <- as.integer(bike_june18$start_station_id)
bike_june18$end_station_id <- as.integer(bike_june18$end_station_id)
bike_july18$start_station_id <- as.integer(bike_july18$start_station_id)
bike_july18$end_station_id <- as.integer(bike_july18$end_station_id)
bike_aug18$start_station_id <- as.integer(bike_aug18$start_station_id)
bike_aug18$end_station_id <- as.integer(bike_aug18$end_station_id)

#joing month data to bike_17: bike_18
bike_18 <- bike_17 %>%
  bind_rows(bike_jan18) %>%
  bind_rows(bike_feb18) %>%
  bind_rows(bike_mar18) %>%
  bind_rows(bike_apr18) %>%
  bind_rows(bike_may18) %>%
  bind_rows(bike_june18) %>%
  bind_rows(bike_july18) %>%
  bind_rows(bike_aug18) 

#add months column: bike_18_months
bike_18_months <- bike_18 %>%
  mutate(start_time= ymd_hms(start_time),
         month=floor_date(start_time, unit="1 month")) 

#graph ride counts over time: bike_18_g1
bike_18_g1 <- bike_18_months %>%
  group_by(month) %>%
  summarise(total_monthly_rides=n()) %>%
  ggplot() +
  geom_bar(aes(x=month, y=total_monthly_rides), stat="identity", fill="orange") +
  geom_point(aes(x=month, y=total_monthly_rides)) +
  geom_line(aes(x=month, y=total_monthly_rides), size=1.5, col="orangered3")  +
  ylab("total rides") +
  ylim(0,200000) +
  ggtitle("Rides Count Over Time")

#graph subscriber and customer counts over time: bike_18_g2
bike_18_g2 <- bike_18_months %>%
  group_by(month) %>%
  mutate(total_monthly_rides=n()) %>%
  count(total_monthly_rides, user_type, month) %>%
  ggplot() +
  geom_bar(aes(x=month, y=n, fill=user_type), stat="identity", position="dodge") +
  geom_point(aes(x=month, y=n, col=user_type)) +
  geom_line(aes(x=month, y=n, group=user_type), size=1.5)  + 
  ylab("number of rides") +
  ylim(0,200000) +
  ggtitle("Ride Count By Usertype Over Time") 

#put graphs next to each other
grid.arrange(bike_18_g1, bike_18_g2, nrow = 1)

#add ride duration in minutes column: bike_17_min
bike_17_min <- bike_17 %>%
  mutate(duration_min=duration_sec/60)

#see longest ride duration
head(bike_17_min %>%
       arrange(desc(duration_min))) %>%
  select(duration_min)

#calculate percent of customer rides  who ride 30 minutes or less
bike_17_min %>%
  mutate(duration_min_breakdown=case_when(
    duration_min <= 30 ~ "30 min or less",
    duration_min > 30 ~ "more than 30 min"
  ))  %>%
  group_by(user_type) %>%
  mutate(total_riders=n()) %>%
  count(user_type, duration_min_breakdown, total_riders) %>%
  mutate(percent=n/total_riders) %>%
  filter(user_type=="Customer")

#calculate percent of subscriber rides who ride 45 minutes or less
bike_17_min %>%
  mutate(duration_min_breakdown=case_when(
    duration_min <= 45 ~ "45 min or less",
    duration_min > 45 ~ "more than 45 min"
  ))  %>%
  group_by(user_type) %>%
  mutate(total_riders=n()) %>%
  count(user_type, duration_min_breakdown, total_riders) %>%
  mutate(percent=n/total_riders) %>%
  filter(user_type=="Subscriber")

#graph percent of rides who ride in each 15 minute interval and facet by usertype
bike_17_min %>%
  mutate(duration_min_breakdown=case_when(
    duration_min >0 & duration_min <= 15  ~ "0 to 15 min",
    duration_min >15 & duration_min <=30  ~ "15 to 30 min",
    duration_min >30 & duration_min <= 45  ~ "30 to 45 min",
    duration_min > 45 ~ "more than 45 min"
  )) %>%
  group_by(user_type) %>%
  mutate(total_riders=n()) %>%
  count(user_type, duration_min_breakdown, total_riders) %>%
  mutate(percent=n/total_riders) %>%
  ggplot(aes(x=user_type, y=percent, fill=duration_min_breakdown)) + 
  geom_bar(stat="identity") +
  xlab("user type") +
  scale_fill_manual(values=c("rosybrown2", "darkseagreen2", "lightblue", "plum3"))

#find the top 12 stations with most rides
head(bike_17 %>%
       group_by(start_station_name) %>%
       summarise(rider_count=n()) %>%
       arrange(desc(rider_count)),n=12)

#save their ids
top12 <- c(15,6,30,67,58,21,81,3,22,16,5,17)

#add days column: bike_17_days
bike_17_days <- bike_17 %>%
  mutate(start_day=wday(bike_17$start_time, label=TRUE), 
         end_day=wday(bike_17$end_time, label=TRUE))

#graph ride counts by day and usertype of the top 12 stations 
bike_17_days %>%
  filter(start_station_id %in% top12) %>%
  ggplot(aes(x=start_day,fill=user_type)) +
  geom_bar(position="dodge") +
  facet_wrap(~start_station_name) +
  theme(strip.text.x = element_text(size = 5)) +
  scale_fill_manual(values=c("#9999CC", "#66CC99"))

#count num of rides for each start station and usertype combination: bike_17_startstations
bike_17_startstations <- bike_17 %>%
  group_by(start_station_longitude, start_station_latitude, user_type) %>%
  summarise(num_of_rides=n())

#get San Francisco map (already in global environment)
#map_SF <- get_map(geocode("351 Shotwell St, San Francisco, CA",source="dsk"),zoom=13, maptype="roadmap")

#graph start stations onto map, scaling dot size by the num of rides
ggmap(map_SF)+
  geom_point(data=bike_17_startstations, 
             aes(x=start_station_longitude, y=start_station_latitude, size=num_of_rides, col=user_type)) +
  facet_wrap(~user_type) +
  ggtitle("Start Station Traffic of San Francisco by User Type")

#get San Jose map (already in global environment)
#map_SJ <- get_map(geocode("233 W Santa Clara St, San Jose, CA 95113", source="dsk"), zoom=14, maptype="roadmap")

#graph start stations onto map, scaling dot size by the num of rides
ggmap(map_SJ)+
  geom_point(data=bike_17_startstations, 
             aes(x=start_station_longitude, y=start_station_latitude, size=num_of_rides, col=user_type)) +
  facet_wrap(~user_type) +
  ggtitle("Start Station Traffic of San Jose by User Type") 

#get East Bay map (already in global environment)
#map_EB <- get_map(geocode("350 Hawthorne Ave, Oakland, CA 94609", source="dsk"), zoom=12, maptype="roadmap")

#graph start stations onto map, scaling dot size by the num of rides
ggmap(map_EB)+
  geom_point(data=bike_17_startstations, 
             aes(x=start_station_longitude, y=start_station_latitude, size=num_of_rides, col=user_type)) +
  facet_wrap(~user_type) +
  ggtitle("Start Station Traffic of the East Bay by User Type")

#issue with google api to retrieve maps
#since maps objects were in global environment I commented them out above and knited the documented using the code below
library(rmarkdown)
render(input="Exploratory_Data_Analysis_on_Ford_GoBike_Data.Rmd", output_file="Exploratory_Data_Analysis_on_Ford_GoBike_Data.html", envir=globalenv())

#render to github document
render(input="Exploratory_Data_Analysis_on_Ford_GoBike_Data.Rmd", output_format="github_document", output_file="Exploratory_Data_Analysis_on_Ford_GoBike_Data.md", envir=globalenv())

#upload to rpubs
library(rsconnect)
rpubsUpload(title="Exploratory Data Analysis on Ford GoBike Data", contentFile="Exploratory_Data_Analysis_on_Ford_GoBike_Data.html", originalDoc = NULL)

