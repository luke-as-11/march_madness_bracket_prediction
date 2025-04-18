## this will combine all of the team statistics into a single data set to reference

# upload packages
library(df.formatter)
library(tidyverse)
library(geosphere)

# upload data
data_14 <- read.csv("source_data/train_data_14.csv")
data_15 <- read.csv("source_data/train_data_15.csv")
data_16 <- read.csv("source_data/train_data_16.csv")
data_17 <- read.csv("source_data/train_data_17.csv")
data_18 <- read.csv("source_data/train_data_18.csv")
data_19 <- read.csv("source_data/train_data_19.csv")
data_21 <- read.csv("source_data/train_data_21.csv")
data_22 <- read.csv("source_data/train_data_22.csv")
data_23 <- read.csv("source_data/train_data_23.csv")
data_24 <- read.csv("source_data/train_data_24.csv")

# combined all of the data into a single data frame
data <- rbind(data_14, data_15, data_16, data_17, data_18, data_19, data_21, data_22, data_23, data_24)

# remove unnecessary variables
rm(data_14, data_15, data_16, data_17, data_18, data_19, data_21, data_22, data_23, data_24)

# add year identifiers to distinguish the data entries that are now all in one location
data <- cbind(year = c(rep(2014, 64),rep(2015, 64),rep(2016, 64),rep(2017, 64),rep(2018, 64),rep(2019, 64),rep(2021, 64),rep(2022, 64),rep(2023, 64),rep(2024, 64)), data)

# upload data of all division one universities and their latitudes and longitudes
schools <- read.csv("geo_data/clean_school_list.csv")

# format the schools data set to allow for easy merging with the combined team statistic data
schools <- schools[,-2]
colnames(schools)[1] <- "team"

# merge the school locations to the existing team statistic data frame to later compute distances 
data <- data %>%
  left_join(schools, by = "team")

# minor formatting
data <- move.cols(data, 9:10, 3)
colnames(data)[1] <- "team"

# export final data set
write.csv(data, "source_data/all_team_stats.csv", row.names = FALSE)

# remove used variables
rm(schools)
rm(data)