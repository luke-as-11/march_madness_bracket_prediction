## this produces data visualizations for the basketball team statistics

# upload packages
library(pheatmap)

# upload data
bracket_data <- read.csv("final_data/final_bracket_data.csv")

# format data
bracket_data$team_x_win <- as.integer(bracket_data$team_x_win)

# create a correlation matrix of the team statistics
cor_matrix <- cor(bracket_data[,5:11])

# initiate png device
png("correlation_matrix.png", bg = "transparent")

# create a heat map using the correlation matrix
pheatmap(cor_matrix,
         color = colorRampPalette(c("#9ddcff","#2d78ca"))(100),
         clustering_distance_rows = "correlation", 
         clustering_distance_cols = "correlation",
         treeheight_row = 0,
         treeheight_col = 0,
         legend = FALSE,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "Correlation Between Team Statistics",
         angle_col = 315)

# turn off the png device
dev.off()

# create box plots for all of the different variables
boxplot(diff_seed ~ team_x_win, data = bracket_data, col = "#2d78ca", horizontal = TRUE, main = "Seed Difference Impact on Match Outcome", xlab = "Seed Difference", ylab = "Outcome")
boxplot(diff_bpi ~ team_x_win, data = bracket_data, col = "#2d78ca", horizontal = TRUE, main = "BPI Difference Impact on Match Outcome", xlab = "BPI Difference", ylab = "Outcome")
boxplot(diff_qual_win ~ team_x_win, data = bracket_data, col = "#2d78ca", horizontal = TRUE, main = "Quality Win Difference Impact on Match Outcome", xlab = "Quality Win Difference", ylab = "Outcome")
boxplot(diff_qual_loss ~ team_x_win, data = bracket_data, col = "#2d78ca", horizontal = TRUE, main = "Quality Loss Difference Impact on Match Outcome", xlab = "Quality Loss Difference", ylab = "Outcome")
boxplot(diff_qual_win_perc ~ team_x_win, data = bracket_data, col = "#2d78ca", horizontal = TRUE, main = "Quality Win Percentage Difference Impact on Match Outcome", xlab = "Quality Win Percentage Difference", ylab = "Outcome")
boxplot(diff_schdl_strength ~ team_x_win, data = bracket_data, col = "#2d78ca", horizontal = TRUE, main = "Schedule Strength Difference Impact on Match Outcome", xlab = "Schedule Strength Difference", ylab = "Outcome")
boxplot(diff_dist ~ team_x_win, data = bracket_data, col = "#2d78ca", horizontal = TRUE, main = "Distance Difference Impact on Match Outcome", xlab = "Distance Difference", ylab = "Outcome")

# remove used variables
rm(bracket_data)
rm(cor_matrix)