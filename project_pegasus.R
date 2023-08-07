install.packages("tidyverse")
install.packages("ggplot2")
install.packages("skimr")
install.packages("dplyr")


bike <- read_csv(
  "C:\\Users\\brice\\Downloads\\202004-divvy-tripdata\\202004-divvy-tripdata.csv")

#View(bike)
#skim_without_charts(bike)

# 84,776 records
# 13 fields
# dtypes: char, num, POSIXct
# skimming we notice there are 99 end stations missing
# with corresponging 99 end lat and long

#lacking instructions on how to treat missing data lets zoomin on it
#to see whats going on there

bike_missing <- bike[is.na(bike$end_station_name),]
View(bike_missing)

#I do not discern any patterns in the missing data
# since 99 of 84,776 total records is only slightly more 
#than .1% of the total data we'll drop these records

clean_bike <- na.omit(bike)

#not being a resident of Chicago the station names mean nothing to me
#we'll go ahead and drop these columns and just use the station_ids
#we'll also drop rideable_type because it has a single value for every record
# and thus conveys no information

clean_bike <- clean_bike[, -c(2, 5, 7)]

#lets make sure all the start times actually occur before the end times
clean_bike$ride_duration <- clean_bike$ended_at - clean_bike$started_at
sum(clean_bike$ride_duration <=0)
#bad <- clean_bike %>% filter(ride_duration <= 0)
#bad_agg <- bad %>% 
#  group_by(member_casual) %>% 
#  summarize(Count = n())


#57 records duration <= 0, of those 51 are < 0
# roughly 40/60 casual/member

#anyway, lets drop the records with 0 or negative durations 
#as either system or user errors

clean_bike <- clean_bike %>% 
  filter(ride_duration > 0)
View(clean_bike)
# seperate time out from date
clean_bike$start_time <- format(clean_bike$started_at, format = '%H:%M:%S')
clean_bike$start_hour <- format(clean_bike$started_at, format = '%H')

#separate the day of the week from the start date
clean_bike$start_day <- weekdays(clean_bike$started_at)

#I think it might be helpful for our analysis of customer behavior
#to break the day into parts, roughly the morning, midday, evening,
#and late night crowds
clean_bike <- clean_bike %>% 
  mutate(part_of_day = case_when(
    start_hour >= 5 & start_hour < 10 ~ 'morning',
    start_hour >= 10 & start_hour < 16 ~ 'mid',
    start_hour >= 16 & start_hour < 20 ~ 'evening',
    start_hour >= 20 | start_hour < 5 ~ 'late'
  ))

#####more organizing
#now that we've extracted the information from the started_at col
#we can drop that, as well as the ride_id
View(clean_bike)
clean_bike <- clean_bike[, -c(1, 2, 3, 12)]



#plotting
ggplot(data = clean_bike) +
  geom_bar(mapping = aes(x = start_hour, fill='orange')) +
  facet_wrap(~member_casual)

#looking at these graphs lets breifly investigate the time between 
#midnight and 5am
clean_bike$start_hour <- as.numeric(clean_bike$start_hour)

late_crowd  <- clean_bike %>% 
  filter(start_hour < 5)
ggplot(data = clean_bike) +
  geom_bar(data = subset(clean_bike, start_hour < 5), 
           mapping = aes(x = start_hour))

count(late_crowd)
count(late_crowd) / count(clean_bike) * 100

#the total number of customers taking rides between midnight and 5am are 879.
#this is about 1% of our total so we'll drop these hours to focus more tightly
#on the busier times

clean_bike <- clean_bike %>% 
  filter(start_hour >= 5)

ggplot(data = clean_bike) +
  geom_bar(mapping = aes(x = start_hour,)) +
  facet_wrap(~member_casual)


#differences the member users significantly outnumber the casual riders,
#and while the members see the real start of business around 6am it isn't 
# until 11am that the casual riders begin to match that. The casual riders
#peak at about 2,800 at 2pm remain steady for several more hours before declining,
#whereas the members peak at over 6,000 at 5pm and decline rapidly thereafter.

ggplot(data = clean_bike) +
  geom_bar(mapping = aes(x = start_hour)) +
  facet_wrap(member_casual ~ start_day)

#these views are fascinating as they reveal that while the casual riders favor
#Saturday and Sunday rides as one might expect, the members also have a
#strong usage profile on the weekend.
#This is important because it suggests that the member riders are not simply
#using the bikes as a means of transportation but also recreation.
#otherwise the profiles fit the more generalized view of casual riders
#favoring later in the day while members tend to be busiest around the end
#of the workday.

#write.csv(clean_bike, "clean_bike.csv", row.names = FALSE)

ggplot(data = clean_bike) +
  geom_point(data = subset(clean_bike, start_hour = 8),
             mapping = aes(x = start_lat, y = start_lng, alpha = .05)) +
  facet_grid(~member_casual)

#This is merely an example to demonstrate that while we might expect 
#greater activity at the edges of the city in the morning as commuters go into
#work in the city the activity at the endges (suburbs) is rather weak. There is
#high demand in the heart of teh city.
#We can speculate that perhaps customers are taking the "L" into the city and
#using the bike for the final leg of their journey.

ride_avg <- clean_bike %>% 
  groupby(member_casual, start_day, start_hour) %>% 
  summarize(ride_avg = mean(ride_duration) / 60)
ride_avg


#Casual riders average a 71 minute ride while members average a 21 minute ride.
#It is unlikely that causual riders are spending a hour on the bike commuting
#everyday, therefore we can tentatively conclude that they are using the bike
#primarily for recreation.
#Members on the otherhand average just 20 minutes, which is exactly right if
#you were commuting by bike. Here I am drawing from personal experience as I
#commuted to work by bike for 10 years while living in Baltimore. My commute
#covered 4miles in about 20minutes depending on traffic lights.

ride_time <- ggplot(data = ride_avg) +
  geom_point(mapping = aes(x=start_hour, y=ride_avg, fill = 'blue')) +
  geom_smooth(mapping = aes(x=start_hour, y=ride_avg)) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_grid(member_casual ~ start_day)
ride_time

#Here we graph the average ride time by day per member/casual.
#Among casual riders We can see a fairly wide distribution of average 
#ride time cresting somewhere in the middle of the day, while members stay
#consistently in the 20 minute range day in and day out.