######## R Code for DMKD Project ########
####### Written by: Sejal Jaiswal #######
####### UBER Data for 2014 - 2015 #######

library(lubridate) #for date modifications
library(ggmap)
library(plotly)
library(VIM) #for aggr
library(dplyr) #for left join, count_, 
library(DT) #for datatable
library(dbscan)
library(ggplot2)

######## DATA FOR 2014 ########

apr14 <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-apr14.csv")
may14 <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-may14.csv")
jun14 <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-jun14.csv")
jul14 <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-jul14.csv")
aug14 <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-aug14.csv")
sep14 <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-sep14.csv")

#### Bind all the files in one and then delete the individual files
apr_june14 <- bind_rows(apr14, may14, jun14)
july_sep14 <- bind_rows(jul14, aug14, sep14)
rm(apr14,may14,jun14,jul14,aug14,sep14)
gc()

#### Checking the data in the dataset
apr_june14 <- na.omit(apr_june14)
july_sep14 <- na.omit(july_sep14)
#summary(data_2014)
# #### Shows there are no more missing data (or na values)
# aggr(apr_june14)

#### Separate or mutate the Date/Time columns
#colnames(apr_june14)
colnames(apr_june14)[1] <- c("date_time")
colnames(july_sep14)[1] <- c("date_time")

# apr_june14$date_time <- mdy_hms(apr_june14$date_time)
# apr_june14$Day <- factor(day(apr_june14$date_time))
# apr_june14$Month <- factor(month(apr_june14$date_time))
# july_sep14$Month <- factor(month(july_sep14$date_time))
# apr_june14$Year <- factor(year(apr_june14$date_time))
# apr_june14$Weekday <- factor(wday(apr_june14$date_time))
# apr_june14$Hour <- factor(hour(apr_june14$date_time))
# apr_june14$Minute <- factor(minute(apr_june14$date_time))
# apr_june14$Second <- factor(second(apr_june14$date_time))

#### Plot the data of trips in NY city map
NYCMap <- get_map("New York", zoom = 10)
#ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat),data = data_2014)

test_april14 <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-apr14.csv", nrows=10000)

######## DATA ANALYSIS - MODEL 1 ########

#### Making boroughs using DBSCAN
EPS <- 0.009
clusters_dbscan_train <- dbscan(select(test_april14, Lat, Lon), eps = EPS)
test_april14$cluster_db <- as.factor(clusters_dbscan_train$cluster)
groups <- test_april14 %>% filter(clusters_dbscan_train$cluster != 0)
noise_train <- test_april14 %>% filter(clusters_dbscan_train$cluster == 0)
noise_test <- july_sep14 %>% filter(clusters_dbscan_test$cluster == 0)

#### Making boroughs using DBSCAN
# EPS <- 0.2
# clusters_dbscan_train <- dbscan(select(apr_june14, Lat, Lon), eps = EPS)
# clusters_dbscan_test <- dbscan(select(july_sep14, Lat, Lon), eps = EPS)
# test_april14$cluster_db <- as.factor(clusters_dbscan$cluster)
# groups <- test_april14 %>% filter(clusters_dbscan$cluster != 0)
# noise_train <- apr_june14 %>% filter(clusters_dbscan_train$cluster == 0)
# noise_test <- july_sep14 %>% filter(clusters_dbscan_test$cluster == 0)

#### Making boroughs using KMeans
set.seed(20)
clusters_train <- kmeans(apr_june14[,2:3], 5)
clusters_test <- kmeans(july_sep14[,2:3], 5)
apr_june14$Borough <- as.factor(clusters_train$cluster)
july_sep14$Borough <- as.factor(clusters_test$cluster)

#### Making final data for 2014 to be used in Model 2
final_data14 <- bind_rows(apr_june14,july_sep14)
final_data14$date_time <- mdy_hms(final_data14$date_time)
final_data14$Month <- factor(month(final_data14$date_time))

#### Trying to plot the boroughs formed from KMean - Training Data
#ggplot(test_april14, aes(test_april14$Lon, test_april14$Lat, color = test_april14$cluster)) + geom_point()
ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(Borough)),data = apr_june14) +
  #geom_point(aes(x = Lon, y = Lat, fill = "grey"), noise_train) + 
  ggtitle("NYC Boroughs using KMean - Train")

#### Trying to plot the boroughs formed from KMean - Test Data
ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(Borough)),data = july_sep14) +
  #geom_point(aes(x = Lon, y = Lat, fill = "grey"), noise_test) + 
  ggtitle("NYC Boroughs using KMean - Test")


#### Trying to plot the boroughs formed from DBSCAN
ggmap(NYCMap) + geom_point(aes(x = Lon, y = Lat, colour = as.factor(cluster_db)),data = test_april14) +
  geom_point(aes(x = Lon, y = Lat, fill = "grey"), noise_train) +
  geom_point(aes(x = Lon, y = Lat, colour = as.factor(cluster_db)), groups)

#### Check the difference of number of rows between the orig table and after removing .na values
# data_2014_borough_omit = na.omit(data_2014_borough)
# outside_ny_pickup = nrow(data_2014_borough) - nrow(data_2014_borough_omit)
# outside_ny_pickup

######## Data prepartion ends for 2014 ########



######## DATA FOR 2015 ########
# Easier to work on 2015 data since it also have information about the Location Id rather than the longitude and latitude
# Even though the data is divided 50:50 according to the month, it's still a 40:60 distribution of trips.

# data_15 <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-janjune-15.csv")
#data_15_temp <- read.csv("/Users/Sejal/Desktop/DataAnalysis/uber-pickups-NYCity/uber-raw-data-janjune-15.csv", nrows=100000)
# na.omit(data_15)

#### Mapping the Borough acc to the Location ID from taxi_zone_lookup data
colnames(data_15_temp)[colnames(data_15_temp) == 'locationID'] <- "LocationID"
taxi_zone_lookup <- read.csv("https://s3.amazonaws.com/nyc-tlc/misc/taxi+_zone_lookup.csv")
final_data15 <- left_join(data_15, taxi_zone_lookup, by = c("locationID"="LocationID"))

#### Making Test Data and Training Data
##Only Month needed for now to divide the data.
##Also, rendering only the month and later other components of date is computationally efficient.
final_data15$Pickup_date <- ymd_hms(final_data15$Pickup_date)
final_data15$Month <- factor(month(final_data15$Pickup_date))

#Test data (January to March)
final_data15$Month <- as.numeric(as.character(final_data15$Month))

jan_mar15 <- filter(final_data15,final_data15$Month <= 3)
jan_mar15$Day <- factor(day(jan_mar15$Pickup_date))
jan_mar15$Year <- factor(year(jan_mar15$Pickup_date))
jan_mar15$Weekday <- factor(wday(jan_mar15$Pickup_date))
jan_mar15$Hour <- factor(hour(jan_mar15$Pickup_date))
jan_mar15$Minute <- factor(minute(jan_mar15$Pickup_date))
jan_mar15$Second <- factor(second(jan_mar15$Pickup_date))

#Training data (April to June)
apr_june15 <- filter(final_data15,final_data15$Month > 3)
apr_june15$Day <- factor(day(apr_june15$Pickup_date))
apr_june15$Year <- factor(year(apr_june15$Pickup_date))
apr_june15$Weekday <- factor(wday(apr_june15$Pickup_date))
apr_june15$Hour <- factor(hour(apr_june15$Pickup_date))
apr_june15$Minute <- factor(minute(apr_june15$Pickup_date))
apr_june15$Second <- factor(second(apr_june15$Pickup_date))

#rm(data_15,taxi_zone_lookup)
#rm(final_data15)

######## Data preparation ends for 2015 ########

######## DATA ANALYSIS - MODEL 2 ########
#### Number of Trips by month, Borough for 2014
final_data14$Month <- as.double(final_data14$Month)
month_borough_14 <- count_(final_data14, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(month_borough_14)

#### Number of Trips by month, Borough for 2015
month_borough_15 <- count_(final_data15, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(month_borough_15)

####### Plots #######
## Monthly growth 2014
monthly_growth_all_14 <- month_borough_14 %>%
  mutate(Date = paste("04", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2014")
monthly_growth_all_14

## Monthly growth 2015
monthly_growth_all_15 <- month_borough_15 %>%
  filter(!(Borough %in% c ("EWR", "Unknown"))) %>% 
  mutate(Date = paste("01", Month)) %>%
  ggplot(aes(Month, n, colour = Borough)) + geom_line() +
  ggtitle("Uber Monthly Growth - 2015")
monthly_growth_all_15

#### Changing names of columns so as to make names consistent between 2014 and 2015 data ####
## Might also delete columns + add columns
## NOTHING FOR NOW, ALREADY DONE EARLIER
################################################


######## DATA ANALYSIS - MODEL 3 ########

#### Model Deployment on Training Data
#### Number of Trips by Borough
trips_by_borough <- count_(jan_mar15, vars = "Borough", sort = TRUE)
#### Show in a table
datatable(trips_by_borough)

#### Number of Trips by month, Borough
trips_by_month_borough <- count_(jan_mar15, vars = c('Month', 'Borough'), sort = TRUE) %>% 
  arrange(Month, Borough)
datatable(trips_by_month_borough)

#### Number of Trips by Weekday, Borough
##Sunday(1) - Saturday(7)
trips_by_weekday_borough <- count_(jan_mar15, vars = c('Weekday', 'Borough'), sort = TRUE) %>% 
  arrange(Weekday, Borough)
datatable(trips_by_weekday_borough)

#### Number of Trips by Weekday, Hour, Borough
trips_by_wd_hr_br <- count_(jan_mar15, vars = c('Hour', 'Weekday', 'Borough'), sort = TRUE) #%>%
#arrange(Weekday)
datatable(trips_by_wd_hr_br)

####### Plots #######

## Rides by Borough and day
ggplot(jan_mar15) + 
  geom_bar(aes(x=Weekday, fill=Borough),position="dodge") +
  ggtitle('Rides over the days by Borough') +
  xlab('Day of the Week') + 
  ylab('Total Ride')

####### Model 3 - Popular Destinations #######
## Home Destinations
train_Home_dest_weekday <- filter(jan_mar15, jan_mar15$Weekday %in% c(2,3,4,5) & jan_mar15$Hour %in% c(7,8,9,10)) %>%
  count_(vars = c('Zone','Borough'),sort = TRUE)
datatable(train_Home_dest_weekday)

## Brunch Destination Weekend
train_Brunch_dest_weekend <- filter(jan_mar15, jan_mar15$Weekday %in% c(1,6,7) & jan_mar15$Hour %in% c(11,12,13,14)) %>%
  count_(vars = c('Zone','Borough'),sort = TRUE)
datatable(train_Brunch_dest_weekend)

## Office/Return Destinations
train_Office_dest_weekday <- filter(jan_mar15, jan_mar15$Weekday %in% c(2,3,4,5) & jan_mar15$Hour %in% c(16,17,18,19)) %>% 
  count_(vars = c('Zone','Borough'),sort = TRUE)
datatable(train_Office_dest_weekday)

## Party Destinations Weekend
train_Party_dest_weekend <- filter(jan_mar15, jan_mar15$Weekday %in% c(1,6,7) & jan_mar15$Hour %in% c(21,22,23,0,1)) %>%
  count_(vars = c('Zone','Borough'),sort = TRUE)
datatable(train_Party_dest_weekend)

###### EVALUATIONS #######

# Con1: Top three most rides happen in: Manhattan > Brooklyn > Queens
# Con2: Top three popular Zone in the top three boroughs (through use of): 
#       Return_dest_weekday, Party_dest_weekend, Brunch_dest_weekend and Office_dest_weekday.
# RESULT: Hence, given the borough, day of the week and time, we were able to identify the most popular pick-up destination. 
# RESULT: These destination can be categorised as Office area, Brunch/party area, Residential Area accordingly. 

###### Deployment #######
#Checking the model against the apr_june15 data.

####### Plots #######

## Rides by Borough and day
ggplot(apr_june15) + 
  geom_bar(aes(x=Weekday, fill=Borough),position="dodge") +
  ggtitle('Rides over the days by Borough for Test Data') +
  xlab('Day of the Week') + 
  ylab('Total Ride')

#Result1: Consistent with Con1.
# Top three most rides happen in: Manhattan > Brooklyn > Queens

####### Model 3 DEPLOYMENT #######
## Home Destination Weekday morning
Home_dest_weekday_test <- filter(apr_june15, apr_june15$Weekday %in% c(2,3,4,5) & apr_june15$Hour %in% c(7,8,9,10)) %>%
  count_(vars = c('Zone','Borough'),sort = TRUE)
datatable(Home_dest_weekday_test)

## Brunch Destination Weekend morning
Brunch_dest_weekend_test <- filter(apr_june15, apr_june15$Weekday %in% c(1,6,7) & apr_june15$Hour %in% c(10,11,12,13)) %>%
  count_(vars = c('Zone','Borough'),sort = TRUE)
datatable(Brunch_dest_weekend_test)

## Office/Return Destinations Weekday evening
Office_dest_weekday_test <- filter(apr_june15, apr_june15$Weekday %in% c(2,3,4,5) & apr_june15$Hour %in% c(16,17,18,19)) %>% 
  count_(vars = c('Zone','Borough'),sort = TRUE)
datatable(Office_dest_weekday_test)

## Party Destination Weekend night
Party_dest_weekend_test <- filter(apr_june15, apr_june15$Weekday %in% c(1,6,7) & apr_june15$Hour %in% c(22,23,12,1)) %>%
  count_(vars = c('Zone','Borough'),sort = TRUE)
datatable(Party_dest_weekend_test)
