## this is a pre-made script to run the 2024-2025 season data through when the bracket is officially released
## this is meant to streamline a quick prediction that can be easily entered into a verified bracket challenge
## this references the adaptive score function heavily but removes the finals steps as the actual results are unknown

# upload packages
library(tidyverse)
library(df.formatter)
library(geosphere)

# import the data that will be used in the model compiled in google sheets sourced from espn
team_stats_25 <- read.csv("source_data/train_data_25.csv")

# upload school geographical data to add latitude and longitude
schools <- read.csv("geo_data/clean_school_list.csv")
schools <- schools[,-2]
colnames(schools)[1] <- "team"

# merge the locations to the team statistic data frame
team_stats_25 <- team_stats_25 %>%
  left_join(schools, by = "team")

# minor formatting
team_stats_25 <- move.cols(team_stats_25, 9:10, 2)
colnames(team_stats_25)[1] <- "team"

# remove used variable
rm(schools)

# export team statistics for later use
write.csv(team_stats_25, "source_data/train_data_25.csv", row.names = FALSE)

# start structuring data as seen in the structure data script

# create list to loop through each round
bracket_data_25 <- list(team_stats_25, round_64 = data.frame(), round_32 = data.frame(), round_16 = data.frame(), round_8 = data.frame(), round_4 = data.frame(), round_2 = data.frame())

# enter placeholders for the outcomes as these are unknown
outcomes <- list(round_64 = rep(TRUE, 32), round_32 = rep(TRUE, 16), round_16 = rep(TRUE, 8), round_8 = rep(TRUE, 4), round_4 = rep(TRUE, 2), round_2 = rep(TRUE, 1))

# create a function to structure the bracket
bracket_data_collection <- function(bracket_data) {
  
  a <- c(64,32,16,8,4,2)
  b <- c(2,3,4,5,6,7)
  
  for(c in 1:6) {
    # make each entry of data frame contain both opponents
    bracket_data[[b[c]]] <- data.frame(team_x = bracket_data[[b[c]-1]]$team[seq(from = 1, to = (a[c] - 1), by = 2)], team_y = bracket_data[[b[c]-1]]$team[seq(from = 2, to = a[c], by = 2)])
    # merge the team data with the corresponding matches
    bracket_data[[b[c]]] <- merge(x = bracket_data[[b[c]]], y = bracket_data[[1]], by.x = "team_x", by.y = "team", sort = FALSE)
    bracket_data[[b[c]]] <- merge(x = bracket_data[[b[c]]], y = bracket_data[[1]], by.x = "team_y", by.y = "team", sort = FALSE)
    bracket_data[[b[c]]] <- move.col(bracket_data[[b[c]]], 2, 1)
    bracket_data[[b[c]]] <- cbind(round = a[c], bracket_data[[b[c]]], team_x_win = as.logical(NA))
    
    # input the outcomes for each match
    bracket_data[[b[c]]]$team_x_win <- outcomes[[b[c]-1]]
    
    # use match outcomes to create column of all the winners to be used at the beginning of this for loop
    bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
      mutate(team = ifelse(team_x_win == TRUE, team_x, team_y))
  }
  
  return(bracket_data)
}

# run the function
final_bracket_data <- bracket_data_collection(bracket_data_25)

# consolidate the list of data frames into a single data frame
final_bracket_data <- rbind(data.frame(final_bracket_data[[2]]),
                            data.frame(final_bracket_data[[3]]),
                            data.frame(final_bracket_data[[4]]),
                            data.frame(final_bracket_data[[5]]),
                            data.frame(final_bracket_data[[6]]),
                            data.frame(final_bracket_data[[7]]))

# add the locations for each match of the tournament
hosts <- c("Lexington, KY","Denver, CO","Milwaukee, WI","Cleveland, OH",
           "Raleigh, NC","Seattle, WA","Wichita, KS","Providence, RI",
           "Raleigh, NC","Seattle, WA","Denver, CO","Cleveland, OH",
           "Wichita, KS","Providence, RI","Milwaukee, WI","Lexington, KY",
           "Atlanta, GA","San Francisco, CA","Newark, NJ","Indianapolis, IN",
           "San Antonio, TX")

locations <- c(rep(hosts[1],2),rep(hosts[2],2),rep(hosts[3],2),rep(hosts[4],2),
               rep(hosts[5],2),rep(hosts[6],2),rep(hosts[7],2),rep(hosts[8],2),
               rep(hosts[9],2),rep(hosts[10],2),rep(hosts[11],2),rep(hosts[12],2),
               rep(hosts[13],2),rep(hosts[14],2),rep(hosts[15],2),rep(hosts[16],2),
               rep(hosts[1],1),rep(hosts[2],1),rep(hosts[3],1),rep(hosts[4],1),
               rep(hosts[5],1),rep(hosts[6],1),rep(hosts[7],1),rep(hosts[8],1),
               rep(hosts[9],1),rep(hosts[10],1),rep(hosts[11],1),rep(hosts[12],1),
               rep(hosts[13],1),rep(hosts[14],1),rep(hosts[15],1),rep(hosts[16],1),
               rep(hosts[17],2),rep(hosts[18],2),rep(hosts[19],2),rep(hosts[20],2),
               rep(hosts[17],1),rep(hosts[18],1),rep(hosts[19],1),rep(hosts[20],1),
               rep(hosts[21],3))

# add locations to data
final_bracket_data <- cbind(final_bracket_data, locations)

# minor formatting
final_bracket_data <- move.col(final_bracket_data, 24, 22)

# add year identifier
final_bracket_data <- cbind(year = 2025, final_bracket_data)

# minor formatting
final_bracket_data <- move.cols(final_bracket_data, 5:6, 4)
final_bracket_data <- move.cols(final_bracket_data, 14:15, 7)
final_bracket_data <- move.cols(final_bracket_data, 6:8, 16)
final_bracket_data <- move.cols(final_bracket_data, 16:18, 13)

# upload geographical city data
cities <- us_cities

# format city data
cities <- cities %>%
  select(city, state_id, lat, lng) %>%
  mutate(location = paste(city, state_id, sep = ", ")) %>%
  select(location, lat, lng)

# merge geographical data with team statistics
colnames(final_bracket_data)[23] <- "location"
final_bracket_data <- final_bracket_data %>%
  left_join(cities, by = "location")

# calculate distance between school and match location
final_bracket_data <- final_bracket_data %>%
  rowwise() %>%
  mutate(dist_x = (distVincentySphere(c(lng.x, lat.x), c(lng, lat)))/1609.344, dist_y = (distVincentySphere(c(lng.y, lat.y), c(lng, lat)))/1609.344) %>%
  ungroup()

# calculate the differences of team statistics between team one and team two
final_bracket_data <- final_bracket_data %>%
  mutate(diff_seed = (seed.x - seed.y),
         diff_bpi = (bpi.x - bpi.y),
         diff_qual_win = (qual_win.x - qual_win.y),
         diff_qual_loss = (qual_loss.x - qual_loss.y),
         diff_qual_win_perc = (qual_win_perc.x - qual_win_perc.y),
         diff_schdl_strength = (schdl_strngth.x - schdl_strngth.y),
         diff_dist = (dist_x - dist_y))

# select final desired data
final_bracket_data <- final_bracket_data %>%
  select(year, round, team_x, team_y, diff_seed, diff_bpi, diff_qual_win, diff_qual_loss, diff_qual_win_perc, diff_schdl_strength, diff_dist, location, team_x_win)

# export the final data set
write.csv(final_bracket_data, "final_data/bracket_data_25.csv", row.names = FALSE)

# upload the combined bracket data
bracket_data <- read.csv("final_data/final_bracket_data.csv")

# calculate maximums and minimums to scale the data later
minimums <- sapply(bracket_data[,5:11], min)
maximums <- sapply(bracket_data[,5:11], max)

# remove used variable
rm(bracket_data)

# scale differences
for(t in 1:7) {
  final_bracket_data[t+4] <- sapply(final_bracket_data[t+4], function(x) {(x-minimums[t])/(maximums[t]-minimums[t])})
}

# export final bracket data for 2024-2025 season
write.csv(final_bracket_data, "final_data/bracket_data_25.csv", row.names = FALSE)

# import data collected above to run through prediction model
bracket_data_25 <- read.csv("final_data/bracket_data_25.csv")
team_stats_25 <- read.csv("source_data/train_data_25.csv")

# mark predictions bracket that loops through all matches
predict_bracket <- function(model) {
  
  # collect data to initially reference for the first round
  test_data <- bracket_data_25
  test_data <- test_data[1:32,]
  
  # predict the outcomes for the first round only
  set.seed(2025)
  temp <- compute(model, test_data)
  probs <- temp$net.result
  temp <- sapply(probs, function(x) {ifelse(x > .5, 1, 0)})
  probs <- sapply(probs, function(x) {ifelse(x < .5, (1-x), x)})
  predictions <- test_data %>%
    mutate(probability = probs, team_x_win = temp, team = ifelse(team_x_win == 1, team_x, team_y))
  predictions <- predictions[,-1]
  
  # create a list to loop through the remaining rounds of the tournament 
  bracket_data <- list(team_stats_25, round_64 = predictions, round_32 = data.frame(), round_16 = data.frame(), round_8 = data.frame(), round_4 = data.frame(), round_2 = data.frame())
  
  a <- c(32,16,8,4,2)
  b <- c(3,4,5,6,7)
  
  for(c in 1:5) {
    # make each entry of data frame contain both opponents
    bracket_data[[b[c]]] <- data.frame(team_x = bracket_data[[b[c]-1]]$team[seq(from = 1, to = (a[c] - 1), by = 2)], team_y = bracket_data[[b[c]-1]]$team[seq(from = 2, to = a[c], by = 2)])
    # merge the team data with the corresponding matches
    bracket_data[[b[c]]] <- merge(x = bracket_data[[b[c]]], y = bracket_data[[1]], by.x = "team_x", by.y = "team", sort = FALSE)
    bracket_data[[b[c]]] <- merge(x = bracket_data[[b[c]]], y = bracket_data[[1]], by.x = "team_y", by.y = "team", sort = FALSE)
    bracket_data[[b[c]]] <- move.col(bracket_data[[b[c]]], 2, 1)
    bracket_data[[b[c]]] <- cbind(round = a[c], bracket_data[[b[c]]], probability = as.numeric(NA), team_x_win = as.logical(NA))
    
    # enter wrangling of data for differences and distances
    bracket_data[[b[c]]] <- cbind(bracket_data[[b[c]]], location = bracket_data_25$location[bracket_data_25$round == a[c]])
    bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
      left_join(cities, by = "location")
    
    # calculate distances between the schools and match locations
    bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
      rowwise() %>%
      mutate(dist_x = (distVincentySphere(c(lng.x, lat.x), c(lng, lat)))/1609.344, dist_y = (distVincentySphere(c(lng.y, lat.y), c(lng, lat)))/1609.344) %>%
      ungroup()
    
    # calculate the team statistic differences between team one and team two
    bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
      mutate(diff_seed = (seed.x - seed.y),
             diff_bpi = (bpi.x - bpi.y),
             diff_qual_win = (qual_win.x - qual_win.y),
             diff_qual_loss = (qual_loss.x - qual_loss.y),
             diff_qual_win_perc = (qual_win_perc.x - qual_win_perc.y),
             diff_schdl_strength = (schdl_strngth.x - schdl_strngth.y),
             diff_dist = (dist_x - dist_y))
    
    # select desired variables
    bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
      select(round, team_x, team_y, diff_seed, diff_bpi, diff_qual_win, diff_qual_loss, diff_qual_win_perc, diff_schdl_strength, diff_dist, location, team_x_win)
    
    # scale differences
    for(t in 1:7) {
      bracket_data[[b[c]]][t+3] <- sapply(bracket_data[[b[c]]][t+3], function(x) {(x-minimums[t])/(maximums[t]-minimums[t])})
    }
    
    # predict match outcomes
    set.seed(2025)
    temp <- compute(model, bracket_data[[b[c]]])
    probs <- temp$net.result
    temp <- sapply(probs, function(x) {ifelse(x > .5, 1, 0)})
    probs <- sapply(probs, function(x) {ifelse(x < .5, (1-x), x)})
    bracket_data[[b[c]]]$probability <- probs
    bracket_data[[b[c]]]$team_x_win <- temp
    
    # add final column to the data frame that contains the name of the predicted match winner
    bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
      mutate(team = ifelse(team_x_win == 1, team_x, team_y))
  }
  
  # combine all rounds of the tournament into a single data frame
  results <- rbind(data.frame(bracket_data[[2]]),
                   data.frame(bracket_data[[3]]),
                   data.frame(bracket_data[[4]]),
                   data.frame(bracket_data[[5]]),
                   data.frame(bracket_data[[6]]),
                   data.frame(bracket_data[[7]]))
  
  # simple formatting
  colnames(results)[14] <- "probability"
  
  # create final data frame that will be used to create the bracket for the bracket challenge
  results <- data.frame(round = results[,1], probability = results[,14], predicted = results[,13])
  
  return(results)
}

# make predictions
bracket_prediction <- predict_bracket(mdl2)

# view all of the predictions
bracket_prediction

# export predictions
write.csv(bracket_prediction, "prediction_model/final_prediction.csv", row.names = FALSE)

# remove used variables
rm(bracket_prediction)
rm(bracket_data_25)
rm(team_stats_25)
rm(bracket_data)
rm(final_bracket_data)
rm(maximums)
rm(minimums)
rm(cities)
rm(locations)
rm(hosts)