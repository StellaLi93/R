install.packages("devtools")
library("devtools")
library(statsr)
library(dplyr)
library(ggplot2)

# ---load data---
load(url("https://stat.duke.edu/~mc301/data/nycflights.RData"))
names(nycflights)

# Q1: Create a new data frame that includes flights headed to SFO in February, and save this data frame assfo_feb_flights. How many flights meet these criteria?
assfo_feb_flights <- nycflights%>%
  filter(dest == "SFO", nycflights$month == "2")

#Q2: Make a histogram and calculate appropriate summary statistics for arrival delays of sfo_feb_flights. Which of the following is false?
qplot(x = arr_delay, data = assfo_feb_flights, geom = "histogram", binwidth = 15)

#Q3: Calculate the median and interquartile range for arr_delays of flights in the sfo_feb_flights data frame, grouped by carrier. Which carrier has the highest IQR of arrival delays?
assfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_ad = median(arr_delay), IQR_ad = IQR(arr_delay))

#Q4: Considering the data from all the NYC airports, which month has the highest average departure delay?
nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay))

#Q5: Which month has the highest median departure delay from an NYC airport?
nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay))

#Q7: If you were selecting an airport simply based on on time departure percentage, which NYC airport would you choose to fly out of?
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))
nycflights %>%
  group_by(origin) %>%
  summarise(otp = sum(dep_type == "on time")/n()) %>%
  arrange(desc(otp))

#Q8: Mutate the data frame so that it includes a new variable that contains the average speed, avg_speed traveled by the plane for each journey (in mph). What is the tail number of the plane with the fastest avg_speed?
nycflights <- nycflights %>%
  mutate(avg_speed = distance/air_time/60)
nycflights %>%
  arrange(desc(avg_speed), tailnum)

#Q9: Make a scatterplot of avg_speed vs. distance. Which of the following is true about the relationship between average speed and distance.
qplot(x = distance, y = avg_speed, data = nycflights, geom = "point")

#Q10: uppose you define a flight to be “on time” if it gets to the destination on time or earlier than expected, regardless of any departure delays.
# Mutate the data frame to create a new variable called arr_type with levels "on time" and "delayed" based on this definition. 
nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay < 0, "on time", "delayed")) 
nycflights <- nycflights %>%
  summarise(arr_dep = sum(arr_type == "on time", dep_type == "delayed"))
