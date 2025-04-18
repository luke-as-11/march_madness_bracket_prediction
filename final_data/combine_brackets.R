## this combines all of the structured bracket data

# upload packages
library(df.formatter)
library(tidyverse)
library(geosphere)

# upload data
bracket_24 <- read.csv("final_data/bracket_data_24.csv")
bracket_23 <- read.csv("final_data/bracket_data_23.csv")
bracket_22 <- read.csv("final_data/bracket_data_22.csv")
bracket_21 <- read.csv("final_data/bracket_data_21.csv")
bracket_19 <- read.csv("final_data/bracket_data_19.csv")
bracket_18 <- read.csv("final_data/bracket_data_18.csv")
bracket_17 <- read.csv("final_data/bracket_data_17.csv")
bracket_16 <- read.csv("final_data/bracket_data_16.csv")
bracket_15 <- read.csv("final_data/bracket_data_15.csv")
bracket_14 <- read.csv("final_data/bracket_data_14.csv")

# combine data
final_bracket <- rbind(bracket_24, bracket_23, bracket_22, bracket_21, bracket_19, bracket_18, bracket_17, bracket_16, bracket_15, bracket_14)

# remove data sets
rm(bracket_24, bracket_23, bracket_22, bracket_21, bracket_19, bracket_18, bracket_17, bracket_16, bracket_15, bracket_14)

# upload additional data
cities <- us_cities
schools <- read.csv("geo_data/clean_school_list.csv")

# wrangle cities data
cities <- cities %>%
  select(city, state_id, lat, lng) %>%
  mutate(location = paste(city, state_id, sep = ", ")) %>%
  select(location, lat, lng)

# clean school data
schools <- schools[,-2]

## wrangle main data set by adding locations
test_bracket <- final_bracket %>%
  left_join(cities, by = "location")

test_bracket <- move.cols(test_bracket, 20:21, 18)

lat_x <- sapply(test_bracket$team_x, function(x) {schools$lat[schools$school == x]})
lng_x <- sapply(test_bracket$team_x, function(x) {schools$lng[schools$school == x]})
lat_y <- sapply(test_bracket$team_y, function(x) {schools$lat[schools$school == x]})
lng_y <- sapply(test_bracket$team_y, function(x) {schools$lng[schools$school == x]})

test_bracket <- insert.col(test_bracket, "lat_x", 4, lat_x)
test_bracket <- insert.col(test_bracket, "lng_x", 5, lng_x)
test_bracket <- insert.col(test_bracket, "lat_y", 7, lat_y)
test_bracket <- insert.col(test_bracket, "lng_y", 8, lng_y)

# calculate distances between each individual school and the location of the match
test_bracket <- test_bracket %>%
  rowwise() %>%
  mutate(dist_x = (distVincentySphere(c(lng_x, lat_x), c(lng, lat)))/1609.344, dist_y = (distVincentySphere(c(lng_y, lat_y), c(lng, lat)))/1609.344) %>%
  ungroup()

# minor formatting
test_bracket <- move.col(test_bracket, 26, 6)
test_bracket <- move.col(test_bracket, 27, 10)

# calculate differences
bracket <- test_bracket %>%
  mutate(diff_seed = (seed.x - seed.y),
         diff_bpi = (bpi.x - bpi.y),
         diff_qual_win = (qual_win.x - qual_win.y),
         diff_qual_loss = (qual_loss.x - qual_loss.y),
         diff_qual_win_perc = (qual_win_perc.x - qual_win_perc.y),
         diff_schdl_strength = (schdl_strngth.x - schdl_strngth.y),
         diff_dist = (dist_x - dist_y))

# structure final data set
bracket <- bracket %>%
  select(year, round, team_x, team_y, diff_seed, diff_bpi, diff_qual_win, diff_qual_loss, diff_qual_win_perc, diff_schdl_strength, diff_dist, location, team_x_win, team)

# sort final data set
my_data <- bracket %>%
  arrange(desc(year), desc(round))

# export final data set
write.csv(my_data, "final_data/final_bracket_data.csv", row.names = FALSE)

# remove used variables
rm(bracket)
rm(my_data)
rm(test_bracket)
rm(final_bracket)
rm(lat_x)
rm(lat_y)
rm(lng_x)
rm(lat_y)
rm(schools)
rm(cities)