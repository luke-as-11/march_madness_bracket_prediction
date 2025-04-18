## this creates the final neural net model used

# upload packages
library(neuralnet)
library(tidyverse)

# upload data
bracket_data <- read.csv("final_data/final_bracket_data.csv")

# scale data
min_max_scale <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

bracket_data[,5:11] <- lapply(bracket_data[,5:11], min_max_scale)

# split data
train_bracket <- bracket_data %>%
  filter(year != "2024" & year != "2021" & year != "2017")

test_bracket <- bracket_data %>%
  filter(year == "2024" | year == "2021" | year == "2017")

# create initial machine learning model
set.seed(2025)
mdl1 <- neuralnet(team_x_win ~ diff_bpi + diff_qual_win + diff_qual_win_perc, data = train_bracket, hidden = c(2,2), act.fct = "logistic")
plot(mdl1)

# check model performance with a by-game approach rather than a by-bracket approach
score_bracket <- function(model) {
  # predict test data
  computation <- compute(model, test_bracket)
  # gain probabilities of team one winning the match
  probability <- computation$net.result
  # generalize probabilities to either a win or a loss
  predicted <- sapply(probability, function(x) {ifelse(x > .5, 1, 0)})
  # combine results into a single data frame
  results <- cbind(test_bracket$year, test_bracket$round, test_bracket$team_x_win, probability, predicted)
  results <- data.frame(year = results[,1], round = results[,2], actual = results [,3], probability = results[,4], predicted = results[,5])
  
  # map points resembling an official bracket competition scoring method
  input_values <- c(64, 32, 16, 8, 4, 2)
  output_values <- c(1, 2, 4, 8, 16, 32)
  value_mapping <- setNames(output_values, input_values)
  
  # applied mapped points based on success of predictions
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

# results of initial model prediction
score1 <- score_bracket(mdl1)
models1 <- data.frame(mdl1 = score1$score)

# revise machine learning model
set.seed(2025)
mdl2 <- neuralnet(team_x_win ~ diff_bpi + diff_qual_win + diff_qual_win_perc, data = train_bracket, hidden = c(2,3,2), act.fct = "logistic")
plot(mdl2)

# results of revised model prediction
score2 <- score_bracket(mdl2)
models2 <- data.frame(mdl2 = score2$score)