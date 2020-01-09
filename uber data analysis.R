library(ggplot2)
##ggplot2 is the most popular data visualization library 
##that is most widely used for creating aesthetic visualization plots.
library(ggthemes)
##we can create better create extra themes and scales with the mainstream ggplot2 package.
library(lubridate)
##Our dataset involves various time-frames. In order to understand our data in separate time categories,
##we will make use of the lubridate package.
library(dplyr)
##This package is the lingua franca of data manipulation in R
library(tidyr)
##The basic principle of tidyr is to tidy the columns where each variable is present in a column,
##each observation is represented by a row and each value depicts a cell.
library(DT)
##With the help of this package, we will be able to interface with the JavaScript Library called - Datatables.
library(scales)
##With the help of graphical scales, 
##we can automatically map the data to the correct scales with well-placed axes and legends.



##we will create a vector of our colors that will be included in our plotting functions
colors <- c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

#Reading the Data
apr_data <- read.csv("G:\\R Language\\R project\\uber_data_analysis\\uber-raw-data-apr14.csv")
may_data <- read.csv("G:\\R Language\\R project\\uber_data_analysis\\uber-raw-data-may14.csv")
jun_data <- read.csv("G:\\R Language\\R project\\uber_data_analysis\\uber-raw-data-jun14.csv")
jul_data <- read.csv("G:\\R Language\\R project\\uber_data_analysis\\uber-raw-data-jul14.csv")
aug_data <- read.csv("G:\\R Language\\R project\\uber_data_analysis\\uber-raw-data-aug14.csv")
sep_data <- read.csv("G:\\R Language\\R project\\uber_data_analysis\\uber-raw-data-sep14.csv")

##combine all data in one table
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)
head(data_2014)
tail(data_2014)
dim(data_2014)##number of rows and columns in dataset data_2014

##as.POSIXct() used to convert date-time formart
##There are two POSIX date/time classes, which differ in the way that the values are stored internally. 
##The POSIXct class stores date/time values as the number of seconds since January 1, 1970,
##while the POSIXlt class stores them as a list with elements for second, minute, hour, day, month, and year, 
##among others
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
View(data_2014)##View() will show you whole table of dataset
head(data_2014)

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
head(data_2014$Time)
View(data_2014$Time)

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)
##ymd_hms() functions automatically assigns the Universal Coordinated Time Zone (UTC) to the parsed date. 
data_2014$Date.Time

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
           
head(data_2014)

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))
head(data_2014)

#########################################################################################

#. We will also use dplyr to aggregate our data.
#In the resulting visualizations, we can understand how the number of passengers fares throughout the day.
## Here we have used dplyr to aggregate our data in hour basis and trip
hour_data <- data_2014 %>% 
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

#In the next step or R project, we will use the ggplot function
#to plot the number of trips that the passengers had made in a day
#this plot displays the of trip made per hour 
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)



#########################################################################################

#This dplyr summarize the table of Total trip  made in each hour of every month
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())
datatable(month_hour)

##THIs plot will display Total trip  made in each hour of every month
ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

##########################################################################################

##Plotting data by trips during every day of the every month
## dplyr will summarize the the table of Total trip made per day of every month
month_day <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())
datatable(month_day)

##THIs plot will display Total trip  made per day of every month
ggplot(month_day, aes(day, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)

########################################################################################
##Plotting data by trips during every day 
## dplyr will summarize the the table of Total trip made per day 
only_day <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n())
datatable(only_day)

##THIs plot will display Total trip  made per day
ggplot(only_day, aes(day, Total)) + 
  geom_bar( stat = "identity",fill = "red", color = "green") +
  ggtitle("Trips by Day ") +
  scale_y_continuous(labels = comma)
  
#########################################################################################

##Number of Trips taking place during per months in a year
## dplyr will summarize the the table of Total trip made per month 
month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

##THIs plot will display Total trip  made per month

ggplot( month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

########################################################################################


## dplyr will summarize the the table of Total trip made per weekday in every month
month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())
  datatable(month_weekday)
  
##plot of Total trip made per weekday in every month
ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Dayofweek and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


########################################################################################
str(data_2014$Base)

##plot of the TOtal number of Trips per bases(There are 5 bases)
ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkblue") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

########################################################################################

##plot of the TOtal number of Trips per bases(There are 5 bases) per month
ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values = colors)

########################################################################################


##plot of the TOtal number of Trips per bases(There are 5 bases) per dayofweek
ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)

########################################################################################

########Creating a Heatmap visualization of day, hour and month################

##First, we will plot Heatmap by Hour and Day
##dplyr will display per hour per day total trip
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

##plot heatmap by per hour per day Total trip
ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

#######################################################################################
##second, we will plot Heatmap by day and month
day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())
datatable(day_month_group)


ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

#######################################################################################
##third, we will plot Heatmap by dayofweek and month

month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())
datatable(month_weekday)


ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

#############################################################################################
##Fourth, a Heatmap that delineates Month and Bases.

month_base <-  data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_base)

ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")


#############################################################################################
##Fifth, a heatmap that delineats dayofweek and bases\

dayofweek_bases <-  data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 
datatable(dayofweek_bases)

ggplot(dayofweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

##############################################################################################

min_lat<-min(data_2014$Lat)
max_lat<-max(data_2014$Lat)
min_lon<-min(data_2014$Lon)
max_lon<-max(data_2014$Lon)
min_lat
min_lon
max_lat
max_lon
head(data_2014)
ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_lon, max_lon)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_lon, max_lon)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")


