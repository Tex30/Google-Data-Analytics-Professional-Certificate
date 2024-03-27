# Load library
library(tidyverse)
library(conflicted)

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

# Compare colums of all tables
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

# Add day_of_week column
all_trips_q1$day_of_week <- wday(all_trips_q1$started_at, label = TRUE, abbr = FALSE) # nolint
all_trips_q2$day_of_week <- wday(all_trips_q2$started_at, label = TRUE, abbr = FALSE) # nolint
all_trips_q3$day_of_week <- wday(all_trips_q3$started_at, label = TRUE, abbr = FALSE) # nolint
all_trips_q4$day_of_week <- wday(all_trips_q4$started_at, label = TRUE, abbr = FALSE) # nolint

# Add ride_lenght column
all_trips_q1$ride_lenght <- difftime(all_trips_q1$ended_at, all_trips_q1$started_at) # nolint
all_trips_q2$ride_lenght <- difftime(all_trips_q2$ended_at, all_trips_q2$started_at) # nolint
all_trips_q3$ride_lenght <- difftime(all_trips_q3$ended_at, all_trips_q3$started_at) # nolint
all_trips_q4$ride_lenght <- difftime(all_trips_q4$ended_at, all_trips_q4$started_at) # nolint
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

# Remove rows with negative ride lenghts
all_trips_q1 <- all_trips_q1[all_trips_q1$ride_lenght >= 0, ]
all_trips_q2 <- all_trips_q1[all_trips_q2$ride_lenght >= 0, ]
all_trips_q3 <- all_trips_q1[all_trips_q3$ride_lenght >= 0, ]
all_trips_q4 <- all_trips_q1[all_trips_q4$ride_lenght >= 0, ]

# Re check for negative ride lenghts
table(sign(all_trips_q1$ride_lenght))
table(sign(all_trips_q2$ride_lenght))
table(sign(all_trips_q3$ride_lenght))
table(sign(all_trips_q4$ride_lenght))

# Remove duplicates
all_trips_q1 <- distinct(all_trips_q1)
all_trips_q2 <- distinct(all_trips_q2)
all_trips_q3 <- distinct(all_trips_q3)
all_trips_q4 <- distinct(all_trips_q4)

# Covert ride_lenght from Factor to numeric, so we can run calculations
all_trips_q1$ride_lenght <- as.numeric(as.character(all_trips_q1$ride_lenght))
all_trips_q2$ride_lenght <- as.numeric(as.character(all_trips_q2$ride_lenght))
all_trips_q3$ride_lenght <- as.numeric(as.character(all_trips_q3$ride_lenght))
all_trips_q4$ride_lenght <- as.numeric(as.character(all_trips_q4$ride_lenght))

# Bind of all data
all_trips <- bind_rows(all_trips_q1, all_trips_q2, all_trips_q3, all_trips_q4)

# Save cleaned data in csv
write.csv(all_trips_q1, "all_trips_2023_q1.csv", row.names = FALSE)
write.csv(all_trips_q2, "all_trips_2023_q2.csv", row.names = FALSE)
write.csv(all_trips_q3, "all_trips_2023_q3.csv", row.names = FALSE)
write.csv(all_trips_q4, "all_trips_2024_q1.csv", row.names = FALSE)
write.csv(all_trips, "all_trips.csv", row.names = FALSE)
