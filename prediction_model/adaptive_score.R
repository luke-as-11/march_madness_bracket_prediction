## this creates a more accurate method of scoring the predictive capability of the model
## many of the variables referenced here are created within the main markdown document
## this should only be used as reference to the markdown document which contains all of this information already

# upload packages
library(df.formatter)
library(tidyverse)
library(neuralnet)
library(geosphere)

# make a function to predict bracket by looping through all matches and comparing the results to the actual outcomes
active_score_bracket <- function(model) {

  # create a list to loop through each year individually
  final_data <- list(year_2024 = data.frame(), year_2021 = data.frame(), year_2017 = data.frame())
  year <- c(2024, 2021, 2017)
  
  for(y in 1:3) {
    
    # create a reference data frame of team statistics for a given year
    test_data <- test_bracket[test_bracket$year == year[y],]
    test_data <- test_data[1:32,]
    
    # predict test data of first round only
    temp <- compute(model, test_data)
    # gain probabilities of team one winning the match
    probs <- temp$net.result
    # generalize probabilities to either a win or a loss
    temp <- sapply(probs, function(x) {ifelse(x > .5, 1, 0)})
    # combine results into a single data frame
    predictions <- test_data %>%
      mutate(probability = probs, team_x_win = temp, team = ifelse(team_x_win == 1, team_x, team_y))
    predictions <- predictions[,-1]
    
    # create a list to loop through all of the remaining rounds
    bracket_data <- list(all_team_stats, round_64 = predictions, round_32 = data.frame(), round_16 = data.frame(), round_8 = data.frame(), round_4 = data.frame(), round_2 = data.frame())
    
    a <- c(32,16,8,4,2)
    b <- c(3,4,5,6,7)
  
    for(c in 1:5) {
      # make each entry of data frame contain both opponents
      bracket_data[[b[c]]] <- data.frame(team_x = bracket_data[[b[c]-1]]$team[seq(from = 1, to = (a[c] - 1), by = 2)], team_y = bracket_data[[b[c]-1]]$team[seq(from = 2, to = a[c], by = 2)])
      # merge the team data with the corresponding matches
      bracket_data[[b[c]]] <- merge(x = bracket_data[[b[c]]], y = bracket_data[[1]][bracket_data[[1]]$year == year[y],], by.x = "team_x", by.y = "team", sort = FALSE)
      bracket_data[[b[c]]] <- merge(x = bracket_data[[b[c]]], y = bracket_data[[1]][bracket_data[[1]]$year == year[y],], by.x = "team_y", by.y = "team", sort = FALSE)
      bracket_data[[b[c]]] <- move.col(bracket_data[[b[c]]], 2, 1)
      bracket_data[[b[c]]] <- cbind(round = a[c], bracket_data[[b[c]]], probability = as.numeric(NA), team_x_win = as.logical(NA))
    
      # wrangle data to add latitude and longitude to data frame
      bracket_data[[b[c]]] <- bracket_data[[b[c]]][,-c(4,13)]
      bracket_data[[b[c]]] <- cbind(bracket_data[[b[c]]], location = test_bracket$location[test_bracket$year == year[y] & test_bracket$round == a[c]])
      bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
        left_join(cities, by = "location")
    
      # calculate distances from the school to the match location
      bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
        rowwise() %>%
        mutate(dist_x = (distVincentySphere(c(lng.x, lat.x), c(lng, lat)))/1609.344, dist_y = (distVincentySphere(c(lng.y, lat.y), c(lng, lat)))/1609.344) %>%
        ungroup()
    
      # calculate the differences in statistics between team one and team two
      bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
        mutate(diff_seed = (seed.x - seed.y),
               diff_bpi = (bpi.x - bpi.y),
               diff_qual_win = (qual_win.x - qual_win.y),
               diff_qual_loss = (qual_loss.x - qual_loss.y),
               diff_qual_win_perc = (qual_win_perc.x - qual_win_perc.y),
               diff_schdl_strength = (schdl_strngth.x - schdl_strngth.y),
               diff_dist = (dist_x - dist_y))
    
      # select final variables for data frame
      bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
        select(round, team_x, team_y, diff_seed, diff_bpi, diff_qual_win, diff_qual_loss, diff_qual_win_perc, diff_schdl_strength, diff_dist, location, team_x_win)
    
      # scale differences to match scaling of the previous training and test data sets
      for(t in 1:7) {
        bracket_data[[b[c]]][t+3] <- sapply(bracket_data[[b[c]]][t+3], function(x) {(x-minimums[t])/(maximums[t]-minimums[t])})
      }
      
      # enter model prediction as covered previously
      temp <- compute(model, bracket_data[[b[c]]])
      probs <- temp$net.result
      temp <- sapply(probs, function(x) {ifelse(x > .5, 1, 0)})
      bracket_data[[b[c]]]$probability <- probs
      bracket_data[[b[c]]]$team_x_win <- temp
    
      bracket_data[[b[c]]] <- bracket_data[[b[c]]] %>%
        mutate(team = ifelse(team_x_win == 1, team_x, team_y))
    }
  
  # combine all rounds into one data frame
  final_data[[y]] <- rbind(data.frame(bracket_data[[2]]),
                           data.frame(bracket_data[[3]]),
                           data.frame(bracket_data[[4]]),
                           data.frame(bracket_data[[5]]),
                           data.frame(bracket_data[[6]]),
                           data.frame(bracket_data[[7]]))
  }
  
  #gets points for each bracket
  results <- rbind(final_data[[1]], final_data[[2]], final_data[[3]])
  colnames(results)[14] <- "probability"
  
  # create data frame that combines all predicted years into one
  results <- data.frame(year = c(rep(2024, 63),rep(2021,63),rep(2017,63)), round = results[,1], actual = test_bracket[,14], probability = results[,14], predicted = results[,13])
  
  # map all of the points to their associated rounds
  input_values <- c(64, 32, 16, 8, 4, 2)
  output_values <- c(1, 2, 4, 8, 16, 32)
  value_mapping <- setNames(output_values, input_values)
  
  # score each match based on the mapped points
  points <- sapply(1:nrow(results), function(x) {
    if(results$actual[x] == results$predicted[x]) {
      return(value_mapping[as.character(results$round[x])])
    } else {
      return(0)
    }
  })
  
  results <- cbind(results, points)
  
  # create score summary for each bracket year
  score <- results %>%
    group_by(year) %>%
    summarise(score = sum(points))
  
  return(score)
}

# show scores to analyze model performance
final_score <- bracket_data_collection(mdl1)
final_score

# remove used variable
rm(final_score)