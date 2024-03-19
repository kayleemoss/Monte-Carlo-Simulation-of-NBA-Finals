library(knitr)
library(data.table)
library(tidyverse)


# Read in data
seed <- fread("C:/Users/kayle/OneDrive/Documents/nba_seeds.csv")
teams <- fread("C:/Users/kayle/OneDrive/Documents/games.csv")



# simulate  game
game <- function(team1, team2){
  team1_seed <- seed[seed$Team == team1, "Seed"][[1]]
  team2_seed <- seed[seed$Team == team2, "Seed"][[1]]
  
  if(team1_seed > team2_seed){
    tmp <- team1
    team1 <- team2
    team2 <- tmp
  }
  
  # Probabilites
  p_1_2 <- teams[teams$home_team == team1 & teams$away_team == team2, "probability"][[1]]
  p_2_1 <- 1 - p_1_2
  
  
  # simulate Game
  result <- sample(c(team1, team2), size = 1, prob = c(p_1_2, p_2_1), replace=TRUE)
  
  # return the winner
  result
}


# Using the game simulation above, format the games in a best of 7 tournament
best_of_seven <- function(team1, team2){
  n <- 7
  win_count1 <- 0
  win_count2 <- 0
  for (i in seq(n)){
    east <- game(team1, team2)
    if (east == team1){
      win_count1 <- win_count1 + 1
      if (win_count1 == 4){
        winner <- team1
        break
      }
    }
    else {
      win_count2 <- win_count2 +1
      if (win_count2 == 4){
        winner <- team2
        break
      }
    }
  }
  
  winner
  
}

mc_results <- c()

sim <- 100
i <- 1

# East Playoff Teams
east_1 <- "Bucks"
east_2 <- "Celtics"
east_3 <- "76ers"
east_4 <- "Cavaliers"
east_5 <- "Knicks"
east_6 <- "Nets"
east_7 <- "Hawks"
east_8 <- "Heat"

# West Playoff Teams
west_1 <- "Nuggets"
west_2 <- "Grizzlies"
west_3 <- "Kings"
west_4 <- "Suns"
west_5 <- "Clippers"
west_6 <- "Warriors"
west_7 <- "Lakers"
west_8 <- "Timberwolves"



#Simulations

set.seed(1234)
while (i <= sim) {
  
  ##### First Round
  # East
  east_1v8 <- best_of_seven(east_1, east_8)
  east_2v7 <- best_of_seven(east_2, east_7)
  east_3v6 <- best_of_seven(east_3, east_6)
  east_4v5 <- best_of_seven(east_4, east_5)
  
  
  # West
  west_1v8 <- best_of_seven(west_1, west_8)
  west_2v7 <- best_of_seven(west_2, west_7)
  west_3v6 <- best_of_seven(west_3, west_6)
  west_4v5 <- best_of_seven(west_4, west_5)
  
  
  ##### Conference Semis
  # East
  east_conference_semi1 <- best_of_seven(east_1v8, east_4v5 )
  east_conference_semi2 <- best_of_seven(east_2v7, east_3v6 )
  
  
  
  # West
  
  west_conference_semi1 <- best_of_seven(west_1v8,west_4v5 )
  west_conference_semi2 <- best_of_seven(west_2v7,west_3v6 )
 
  
  ##### Conference Final
  # East
  east_conference_final <- best_of_seven(east_conference_semi1, east_conference_semi2)
  
  
  
  # West
  west_conference_final <- best_of_seven(west_conference_semi1, west_conference_semi2)
  
  
  
  ##### Finals
  champ <- best_of_seven(east_conference_final, west_conference_final)
  
  
  
  
  results_final <- c( 
    i,  
    east_1v8,
    east_2v7,
    east_3v6,
    east_4v5,
    west_1v8,
    west_2v7,
    west_3v6,
    west_4v5,
    east_conference_semi1,
    east_conference_semi2,
    west_conference_semi1,
    west_conference_semi2,
    east_conference_final,
    west_conference_final,
    champ
  )
  
  mc_results <- c(mc_results, results_final)
  
  i <- i + 1 
}

print(paste('Winner of East 1 vs 8:', east_1v8))
print(paste('Winner of East 2 vs 7:', east_2v7))
print(paste('Winner of East 3 vs 6:', east_3v6))
print(paste('Winner of East 4 vs 5:', east_4v5))
print(paste('Winner of West 1 vs 8:', west_1v8))
print(paste('Winner of West 2 vs 7:', west_2v7))
print(paste('Winner of West 3 vs 6:', west_3v6))
print(paste('Winner of West 4 vs 5:', west_4v5))
print(paste('Winner of East Conference Semis 1:', east_conference_semi1))
print(paste('Winner of East Conference Semis 2:', east_conference_semi2))
print(paste('Winner of West Conference Semis 1:', west_conference_semi1))
print(paste('Winner of West Conference Semis 2:', west_conference_semi2))
print(paste('Winner of East Conference:', east_conference_final))
print(paste('Winner of West Conference:', west_conference_final))
print(paste('NBA Champion:', champ))


mc_mat <- matrix(mc_results, ncol=16, byrow=TRUE)
mc_df <- as.data.frame(mc_mat)
names(mc_df) <- c( 
  "sim",
  "east_1v8",
  "east_2v7",
  "east_3v6",
  "east_4v5",
  "west_1v8",
  "west_2v7",
  "west_3v6",
  "west_4v5",
  "east_conference_semi1",
  "east_conference_semi2",
  "west_conference_semi1",
  "west_conference_semi2",
  "east_conference_final",
  "west_conference_final",
  "champ"
)
mc_df
chance_win <- function(series){
  
  tbl <- table(mc_df[ , series])
  df <- data.frame(team = names(tbl), chance = as.numeric(tbl)/sum(tbl))
  df <- df[order(df$chance, decreasing=TRUE), ]
  df
}

# Superbowl Champions
champs_df <- chance_win("champ")


champ_chances <- allteams %>% 
  left_join(champs_df, by = "team") %>%
  rename(Champion = chance) %>%
  arrange(desc(Champion))

champ_chances

