## this is an example of constructing the bracket data sets for the 2023-2024 season

# upload packages
library(df.formatter)
library(tidyverse)

# upload data compiled in google sheets from espn
stats_24 <- read.csv("source_data/train_data_24.csv")

# create list to organize each round of the bracket
bracket_data_24 <- list(stats_24, round_64 = data.frame(), round_32 = data.frame(), round_16 = data.frame(), round_8 = data.frame(), round_4 = data.frame(), round_2 = data.frame())
rm(stats_24)

# create a list of vectors of data type boolean that signify whether team one won the match or not
outcomes <- list(round_64 = c(), round_32 = c(), round_16 = c(), round_8 = c(), round_4 = c(), round_2 = c())

c <- 32
for(a in c(1,2,3,4,5,6)) {
  for(b in 1:c) {
    outcomes[[a]][b] <- readline()
  }
  c <- c/2
}
rm(a)
rm(b)
rm(c)

# create a function to loop through the lists created above to fill each round with data and outcomes
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
final_bracket_data <- bracket_data_collection(bracket_data_24)

# consolidate the list of data frames into a single data frame
final_bracket_data <- rbind(data.frame(final_bracket_data[[2]]),
                      data.frame(final_bracket_data[[3]]),
                      data.frame(final_bracket_data[[4]]),
                      data.frame(final_bracket_data[[5]]),
                      data.frame(final_bracket_data[[6]]),
                      data.frame(final_bracket_data[[7]]))

# add the locations for each match of the tournament
hosts <- c("Brooklyn, NY","Spokane, WA","Omaha, NE","Omaha, NE",
           "Charlotte, NC","Spokane, WA","Memphis, TN","Salt Lake City, UT",
           "Memphis, TN","Brooklyn, NY","Pittsburgh, PA","Indianapolis, IN",
           "Indianapolis, IN","Salt Lake City, UT","Pittsburgh, PA","Charlotte, NC",
           "Boston, MA","Los Angeles, CA","Dallas, TX","Detroit, MI",
           "Phoenix, AZ")

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

final_bracket_data$location <- locations

# format the data frame
final_bracket_data <- move.col(final_bracket_data, 20, 18)

# add year identifier to data frame
final_bracket_data <- cbind(year = 2024, final_bracket_data)

# export data frame for later use
write.csv(final_bracket_data, "final_data/bracket_data_24.csv", row.names = FALSE)

# remove used variables
rm(final_bracket_data)
rm(outcomes)
rm(locations)
rm(hosts)