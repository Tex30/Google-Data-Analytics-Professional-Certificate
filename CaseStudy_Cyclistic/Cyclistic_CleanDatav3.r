# Load library
library(tidyverse)
library(conflicted)
library(janitor)
library(dbplyr)
# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Load all rider data from February 2023 to February 2024
trips_2023_02 <- read.csv("202302-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_03 <- read.csv("202303-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_04 <- read.csv("202304-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_05 <- read.csv("202305-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_06 <- read.csv("202306-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_07 <- read.csv("202307-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_08 <- read.csv("202308-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_09 <- read.csv("202309-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_10 <- read.csv("202310-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_11 <- read.csv("202311-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2023_12 <- read.csv("202312-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2024_01 <- read.csv("202401-divvy-tripdata.csv") # nolint: line_length_linter.
trips_2024_02 <- read.csv("202402-divvy-tripdata.csv") # nolint: line_length_linter.

# Compare columns of all tables
compare_df_cols(trips_2023_02, trips_2023_03, trips_2023_04, trips_2023_05,
    trips_2023_06, trips_2023_07, trips_2023_08, trips_2023_09, # nolint
    trips_2023_10, trips_2023_11, trips_2023_12, trips_2024_01, # nolint
    trips_2024_02,
    return = "mismatch"
)



# Combibe all files to all_trips
all_trips_q1 <- bind_rows(
    trips_2023_02, trips_2023_03, trips_2023_04, # nolint # nolint
)

all_trips_q2 <- bind_rows(
    trips_2023_05, trips_2023_06, trips_2023_07, trips_2023_08 # nolint
)
all_trips_q3 <- bind_rows(
    trips_2023_09, trips_2023_10, trips_2023_11, trips_2023_12 # nolint
)
all_trips_q4 <- bind_rows(
    trips_2024_01, trips_2024_02 # nolint
)

# Purpose to count the original rows
all_trips_rows <- bind_rows(
    trips_2023_02, trips_2023_03, trips_2023_04,
    trips_2023_05, trips_2023_06, trips_2023_07, trips_2023_08,
    trips_2023_09, trips_2023_10, trips_2023_11, trips_2023_12,
    trips_2024_01, trips_2024_02
)

nrow(all_trips_rows) # 5897613 rows


# Adding columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ...


all_trips_q1$date <- as.Date(all_trips_q1$started_at) # The default format is yyyy-mm-dd
all_trips_q1$month <- format(as.Date(all_trips_q1$date), "%m")
all_trips_q1$day <- format(as.Date(all_trips_q1$date), "%d")
all_trips_q1$year <- format(as.Date(all_trips_q1$date), "%Y")
all_trips_q1$day_of_week <- format(as.Date(all_trips_q1$date), "%A")

all_trips_q2$date <- as.Date(all_trips_q2$started_at) # The default format is yyyy-mm-dd
all_trips_q2$month <- format(as.Date(all_trips_q2$date), "%m")
all_trips_q2$day <- format(as.Date(all_trips_q2$date), "%d")
all_trips_q2$year <- format(as.Date(all_trips_q2$date), "%Y")
all_trips_q2$day_of_week <- format(as.Date(all_trips_q2$date), "%A")

all_trips_q3$date <- as.Date(all_trips_q3$started_at) # The default format is yyyy-mm-dd
all_trips_q3$month <- format(as.Date(all_trips_q3$date), "%m")
all_trips_q3$day <- format(as.Date(all_trips_q3$date), "%d")
all_trips_q3$year <- format(as.Date(all_trips_q3$date), "%Y")
all_trips_q3$day_of_week <- format(as.Date(all_trips_q3$date), "%A")

all_trips_q4$date <- as.Date(all_trips_q4$started_at) # The default format is yyyy-mm-dd
all_trips_q4$month <- format(as.Date(all_trips_q4$date), "%m")
all_trips_q4$day <- format(as.Date(all_trips_q4$date), "%d")
all_trips_q4$year <- format(as.Date(all_trips_q4$date), "%Y")
all_trips_q4$day_of_week <- format(as.Date(all_trips_q4$date), "%A")



# Add ride_lenght column
all_trips_q1$ride_lenght <- difftime(all_trips_q1$ended_at, all_trips_q1$started_at) # nolint
all_trips_q2$ride_lenght <- difftime(all_trips_q2$ended_at, all_trips_q2$started_at) # nolint
all_trips_q3$ride_lenght <- difftime(all_trips_q3$ended_at, all_trips_q3$started_at) # nolint
all_trips_q4$ride_lenght <- difftime(all_trips_q4$ended_at, all_trips_q4$started_at) # nolint


# Inspect the structure of the columns
str(all_trips_q1)
str(all_trips_q2)
str(all_trips_q3)
str(all_trips_q4)


# Remove unused columns
all_trips_q1 <- subset(all_trips_q1, select = -c(start_lat, start_lng, end_lat, end_lng)) # nolint
all_trips_q2 <- subset(all_trips_q2, select = -c(start_lat, start_lng, end_lat, end_lng)) # nolint
all_trips_q3 <- subset(all_trips_q3, select = -c(start_lat, start_lng, end_lat, end_lng)) # nolint
all_trips_q4 <- subset(all_trips_q4, select = -c(start_lat, start_lng, end_lat, end_lng)) # nolint


# Check for negative ride lenght
table(sign(all_trips_q1$ride_lenght))
table(sign(all_trips_q2$ride_lenght))
table(sign(all_trips_q3$ride_lenght))
table(sign(all_trips_q4$ride_lenght))


# Analysis on rife_lenght (seconds)
summary(all_trips_q1$ride_lenght)
summary(all_trips_q2$ride_lenght)
summary(all_trips_q3$ride_lenght)
summary(all_trips_q4$ride_lenght)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips_q1$ride_length)
all_trips_q1$ride_length <- as.numeric(as.character(all_trips_q1$ride_length))
is.numeric(all_trips_q1$ride_length)

is.factor(all_trips_q2$ride_length)
all_trips_q2$ride_length <- as.numeric(as.character(all_trips_q2$ride_length))
is.numeric(all_trips_q2$ride_length)

is.factor(all_trips_q3$ride_length)
all_trips_q3$ride_length <- as.numeric(as.character(all_trips_q3$ride_length))
is.numeric(all_trips_q3$ride_length)

is.factor(all_trips_q4$ride_length)
all_trips_q4$ride_length <- as.numeric(as.character(all_trips_q4$ride_length))
is.numeric(all_trips_q4$ride_length)

# Bind of all data in one big Dataframe
all_trips <- bind_rows(all_trips_q1, all_trips_q2, all_trips_q3, all_trips_q4)

# Remove duplicates in a new dataframe as we are now removing data
all_tripsv2 <- distinct(all_trips)

# Remove rows with ride lenghts samller than 1 min and rides longer than 24h
all_tripsv2 <- all_tripsv2[all_tripsv2$ride_lenght <= 86400, ]
all_tripsv2 <- all_tripsv2[all_tripsv2$ride_lenght >= 60, ]

nrow(all_tripsv2)


# Re Analyse values on rife_lenght (seconds)
summary(all_tripsv2$ride_lenght)

str(all_tripsv2)



nrow(all_trips_rows)
nrow(all_tripsv2) # Total of 5897613 - 3006449 = 2891164 rows of data cleaned



# Save all data in csv
write.csv(all_trips_q1, "FinalData/all_trips_2023_q1.csv", row.names = FALSE) # before removing data part1
write.csv(all_trips_q2, "FinalData/all_trips_2023_q2.csv", row.names = FALSE) # before removing data part2
write.csv(all_trips_q3, "FinalData/all_trips_2023_q3.csv", row.names = FALSE) # before removing data part3
write.csv(all_trips_q4, "FinalData/all_trips_2024_q1.csv", row.names = FALSE) # before removing data part4
write.csv(all_trips, "FinalData/all_trips.csv", row.names = FALSE) # binded data before removing data
write.csv(all_tripsv2, "FinalData/all_tripsv2.csv", row.names = FALSE) # Cleaned data in csv
