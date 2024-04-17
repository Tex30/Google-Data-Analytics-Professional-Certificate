# Load library
library(tidyverse)
library(conflicted)



# Load cleaned data

all_trips_q1 <- read.csv("FinalData/all_trips_2023_q1.csv")
all_trips_q2 <- read.csv("FinalData/all_trips_2023_q2.csv")
all_trips_q3 <- read.csv("FinalData/all_trips_2023_q3.csv")
all_trips_q4 <- read.csv("FinalData/all_trips_2024_q1.csv")
all_tripsv2 <- read.csv("FinalData/all_tripsv2.csv")

# Analysis on rife_lenght (seconds)
summary(all_tripsv2$ride_lenght)

# Ordering to day_of_week
all_tripsv2$day_of_week <- ordered(all_tripsv2$day_of_week, levels = c(
    "Monday", "Tuesday", "Wednesday", # nolint
    "Thursday", "Friday", "Saturday", "Sunday"
)) # nolint

# ride_lenght grouped by member type
aggregate(all_tripsv2$ride_lenght ~ all_tripsv2$member_casual + all_tripsv2$day_of_week, FUN = mean) # nolint

# analyze ridership data by type and weekday
all_tripsv2 %>%
    group_by(member_casual, day_of_week) %>% # groups by usertype and weekday
    summarise(number_rides = n(), average_duration = mean(ride_lenght)) %>% # calculates the average
    arrange(member_casual, day_of_week)

# visualize Average Trip Duration by Member Type and Day of the Week
all_tripsv2 %>%
    group_by(member_casual, day_of_week) %>% # nolint
    summarise(number_rides = n(), avg_duration = mean(ride_lenght)) %>%
    arrange(member_casual, day_of_week) %>%
    ggplot(aes(x = day_of_week, y = avg_duration, fill = member_casual)) +
    geom_col(position = "dodge") +
    xlab("Day of Week") +
    ylab("Average Trip Duration") +
    labs(fill = "Member Type") +
    ggtitle("Average Trip Duration by Member Type and Day of the Week")

# visualize the number of rides by rider type
all_tripsv2 %>%
    group_by(member_casual, day_of_week) %>% # nolint
    summarise(number_rides = n(), avg_duration = mean(ride_lenght)) %>%
    arrange(member_casual, day_of_week) %>%
    ggplot(aes(x = day_of_week, y = number_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    xlab("Day of Week") +
    ylab("Number of Rides") +
    labs(fill = "Member Type") +
    ggtitle("Number of Rides by Member Type and Day of the Week")



# Export Average Trip Lenght Data for further analysis on Tableau

AvgDataLenghtFile <- aggregate(all_tripsv2$ride_lenght ~ all_tripsv2$member_casual + all_tripsv2$day_of_week, FUN = mean)
write.csv(AvgDataLenghtFile, file = "AvgTripLenght.csv")
