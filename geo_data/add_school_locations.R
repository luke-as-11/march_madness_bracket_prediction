## this will add latitude and longitude data to each university based on their city

# upload packages
library(df.formatter)
library(dplyr)

# upload data
schools <- read.csv("geo_data/school_list.csv")
cities <- us_cities

# clean us cities data to prepare for data frame merge
cities <- cities %>%
  select(city, state_id, lat, lng) %>%
  mutate(Location = paste(city, state_id, sep = ", ")) %>%
  select(Location, lat, lng)

# merge data frames
data <- merge(schools, cities, by = "Location")

# find schools that were removed in the merge
which(!(schools$Location %in% data$Location))
schools$School[which(!(schools$Location %in% data$Location))]
schools$Location[which(!(schools$Location %in% data$Location))]

# fix schools locations to ensure that they are included in the merge
# some of the following are simple spelling changes like from saint to st.
# if not a spelling issue the closest available city is imputed
schools$Location[8] <- "Port Gibson, MS"
schools$Location[26] <- "Newton, MA"
schools$Location[32] <- "Woonsocket, RI"
schools$Location[39] <- "San Fernando, CA"
schools$Location[56] <- "Earlville, NY"
schools$Location[63] <- "Lebanon, NH"
schools$Location[84] <- "Fairfield University, CT"
schools$Location[113] <- "Garden City, NY"
schools$Location[145] <- "St. Charles, MO"
schools$Location[160] <- "Yonkers, NY"
schools$Location[166] <- "Northampton, MA"
schools$Location[171] <- "Lawrence, MA"
schools$Location[195] <- "Dover, NH"
schools$Location[231] <- "State College, PA"
schools$Location[243] <- "Northford, CT"
schools$Location[249] <- "Carnot-Moon, PA"
schools$Location[250] <- "New Brunswick, NJ"
schools$Location[252] <- "Fairfield University, CT"
schools$Location[282] <- "St. Bonaventure, NY"
schools$Location[285] <- "St. Paul, MN"
schools$Location[289] <- "Brockton, MA"

# merge data frames again
data <- merge(schools, cities, by = "Location")

# verify corrections worked
schools$School[which(!(schools$Location %in% data$Location))]

# minor formatting
data <- move.col(data, 2, 1)
colnames(data) <- tolower(colnames(data))

data <- data %>%
  arrange(school)

# export clean school data
write.csv(data, "geo_data/clean_school_list.csv", row.names = FALSE)

# remove used variables
rm(schools)
rm(cities)
rm(data)