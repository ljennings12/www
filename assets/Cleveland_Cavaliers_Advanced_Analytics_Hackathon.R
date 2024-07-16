## Brogan Berkey and Liam Jennings
## Cleveland Cavaliers Hackathon - Advanced Analytics
## Due: 11/20/2023


## Libraries
if(!require('tidyverse') & !require('readxl') & !require('gmapsdistance') & !require('measurements') &
   !require('lubridate') & !require('rvest') & !require('factoextra')) {
  install.packages('tidyverse')
  library('tidyverse')
  install.packages('readxl')
  library('readxl')
  install.packages('gmapsdistance')
  library('gmapsdistance')
  install.packages('measurements')
  library('measurements')
  install.packages('lubridate')
  library('lubridate')
  install.packages('rvest')
  library('rvest')
  install.packages('factoextra')
  library('factoextra')
}


## Load in data
cavs <- read_xlsx("Advanced Analytics Data.xlsx")


## Variables

# Opponent
cavs <- cavs %>%
  mutate(Opponent = str_remove_all(`Event Name`, "Cavaliers vs. "))


# Opponent City
cavs <- cavs %>%
  mutate('Opponent City' = str_split_i(cavs$Opponent, ' ', 1), 
         'Opponent City' = recode(`Opponent City`,"Los" = "Los Angeles", "Oklahoma" = "Oklahoma City",
                                  "San" = "San Antonio", "Golden" = "San Francisco", "Indiana" = "Indianapolis",
                                  "Utah" = "Salt Lake City", "Minnesota" = "Minneapolis", "Washington" = "Washington DC"),
         `Opponent City` = ifelse(Opponent == "New York Knicks", "New York", `Opponent City`),
         `Opponent City` = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", `Opponent City`))


# Home City
cavs <- cavs %>%
  mutate('Home City' = "Cleveland")


# Distance (using Google Maps API)

# Set API key
set.api.key("AIzaSyBRsc7SGEQB6-L6WBkXYqHdzxdFwLaGs-s")

# Use gmapsdistance function (may take a while to run)
Distance <- unname(unlist(gmapsdistance(origin = cavs$'Home City', 
                                        destination = cavs$'Opponent City',
                                        combinations = "pairwise")[1]))

# Convert from meters to miles
Distance <- conv_unit(as.numeric(Distance[245:366]), "m", "mi")

# Save into dataframe
cavs$Distance <- Distance


# Top 5 Pick
first5 <- cavs %>%
  filter(`Event Date` > "2021-10-21", `Event Date` <= "2022-04-10") %>%
  mutate('Top 5 Pick' = ifelse(Opponent %in% c("Toronto Raptors", "Orlando Magic", 
                                               "Houston Rockets", "Detroit Pistons"), 1, 0))

second5 <- cavs %>%
  filter(`Event Date` > "2022-10-22",`Event Date` <= "2023-04-09") %>%
  mutate('Top 5 Pick' = ifelse(Opponent %in% c("Orlando Magic", "Oklahoma City Thunder", "Sacramento Kings",
                                               "Houston Rockets", "Detroit Pistons"), 1, 0))

third5 <- cavs %>%
  filter(`Event Date` > "2023-10-26", `Event Date` <= "2024-04-14") %>%
  mutate('Top 5 Pick' = ifelse(Opponent %in% c("San Antonio Spurs", "Charlotte Hornets", "Portland Trail Blazers",
                                               "Houston Rockets", "Detroit Pistons"), 1, 0))

cavs$'Top 5 Pick' <- c(first5$`Top 5 Pick`, second5$`Top 5 Pick`, third5$`Top 5 Pick`)


# Former All-Star (LeBron James, Kyrie Irving, and Kevin Love)
firstAS <- cavs %>%
  filter(`Event Date` > "2021-10-21", `Event Date` <= "2022-04-10") %>%
  mutate('Former All-Star' = ifelse(Opponent %in% c("Los Angeles Lakers", "Brooklyn Nets"), 1, 0))

# Two data frames are needed for the second season because Kyrie Irving and Kevin Love both changed teams around the trade deadline
secondAS1 <- cavs %>%
  filter(`Event Date` > "2022-10-22",`Event Date` <= "2023-02-20") %>%
  mutate('Former All-Star' = ifelse(Opponent %in% c("Los Angeles Lakers", "Brooklyn Nets"), 1, 0))

secondAS2 <- cavs %>%
  filter(`Event Date` > "2023-02-20",`Event Date` <= "2023-04-09") %>%
  mutate('Former All-Star' = ifelse(Opponent %in% c("Los Angeles Lakers", "Dallas Mavericks", "Miami Heat"), 1, 0))

thirdAS <- cavs %>%
  filter(`Event Date` > "2023-10-26", `Event Date` <= "2024-04-14") %>%
  mutate('Former All-Star' = ifelse(Opponent %in% c("Los Angeles Lakers", "Dallas Mavericks", "Miami Heat"), 1, 0))

cavs$'Former All-Star' <- c(firstAS$`Former All-Star`, secondAS1$`Former All-Star`, 
                            secondAS2$`Former All-Star`, thirdAS$`Former All-Star`)


# Division
cavs$Division <- ifelse(cavs$Opponent == "Indiana Pacers" | cavs$Opponent == "Milwaukee Bucks"| 
                          cavs$Opponent == "Chicago Bulls" | cavs$Opponent == "Detroit Pistons",
                        1, 0)


# Conference
cavs$Conference <- ifelse(cavs$Opponent == "Indiana Pacers" | cavs$Opponent == "Milwaukee Bucks"| 
                            cavs$Opponent == "Chicago Bulls" | cavs$Opponent == "Detroit Pistons"|
                            cavs$Opponent == "Boston Celtics" | cavs$Opponent == "Philadelphia 76ers"| 
                            cavs$Opponent == "Brooklyn Nets" | cavs$Opponent == "New York Knicks"|
                            cavs$Opponent == "Toronto Raptors" | cavs$Opponent == "Miami Heat"| 
                            cavs$Opponent == "Atlanta Hawks" | cavs$Opponent == "Orlando Magic"|
                            cavs$Opponent == "Charlotte Hornets" | cavs$Opponent == "Washington Wizards",
                          1, 0)


# Days of the Week
cavs <- cavs %>% 
  mutate(Days = recode(DOW, "MON" = 1, "TUE" = 2, "WED" = 3, 
                       "THU" = 4, "FRI" = 5, "SAT" = 6, "SUN" = 7))


## Dataframe structure
str(cavs)


## Remove NAs (December 8th, 2023 game was removed from Cleveland Cavaliers schedule)
cavs <- cavs %>% na.omit() %>% as.data.frame()


## k-means Clustering

# Row Names
cavsClusters <- data.frame(cavs, row.names = 1)

# Get rid of character variables
cavs_dfs <- cavsClusters[, -c(1:2, 4:6)]

# Scale
cavs_scale <- scale(cavs_dfs)

# Set seed
set.seed(1120)

# k-means function
cavsKM <- kmeans(cavs_scale, 4, nstart = 25)

# Print out results
print(cavsKM)

# Save cluster number to dataframe
cavsGames <- cbind(cavs, cavsKM$cluster) 

# Cluster Centers
cavsKM$centers

## Plot the results
fviz_cluster(cavsKM, data = cavs_scale,
             palette = c("blue", "green3","orange2", "yellow3"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)

# Show cluster means
aggregate(cavs, by = list(cluster = cavsKM$cluster), mean)

# Show cluster sizes
cavsKM$size


## Scrape Attendance Data from Basketball Reference

# Vectors for dates to scrape
years <- c("2022", "2023")
months <- c("october", "november", "december", "january", "february", "march", "april")

# Base data frame to join data into
Cavs <- data.frame(matrix(vector(), 0, 1,
                          dimnames=list(c(), c(""))),
                   stringsAsFactors=F)

# Scrape loop (may take a while to run)
for (j in 1:length(years)) {
  for (i in 1:length(months)) {
    # URL
    link <- paste0("https://www.basketball-reference.com/leagues/NBA_", years[j], 
                   "_games-", months[i], ".html")
    
    # Return html code for website
    content <- read_html(link)
    
    # Pulls from website
    gameScores <- html_table(content, fill = TRUE)
    
    # Save as dataframe
    gameScores <- as.data.frame(gameScores)
    
    # Combine dataframes to get each month/year
    Cavs <- rbind(Cavs, gameScores)
  }
}

# Filter to have only Home Cavaliers games
CavsHome <- Cavs %>%
  filter(Arena == "Rocket Mortgage Fieldhouse")

# Get rid of postseason games
CavsHome <- CavsHome[-c(42, 84:86),]


# Attendance Variable
Attendance <- CavsHome$Attend. %>% recode("0" = "19,432")

Attendance <- as.numeric(gsub(",", "", Attendance))

# 2021-2023 Dataframe
cavsOld <- cavs %>%
  filter(`Event Date` < '2023-04-20')

cavsOld$Attendance <- Attendance
