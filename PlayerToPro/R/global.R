library(shiny)
library(readxl)
library(DT)
library(tidyr)
library(dplyr)
library(rlang)
library(rsconnect)
library(shinyjs)
library(shinyWidgets)

# current usports players
current_players <- read_excel("../data/processed_current_usports_players.xlsx")

# pro players usports data
pro_players_usports_data <- read_excel("../data/usports_stats_with_teams.xlsx")

# pro players pro data
pro_data <- read_excel("../data/pro_season_data.xlsx")

# Helper function to extract last name for sorting
extract_last_name <- function(full_name) {
  sapply(strsplit(full_name, " "), function(x) x[length(x)])
}

# Sort players by last name
all_players <- unique(c(pro_players_usports_data$Player, pro_data$Player))
all_players_sorted <- all_players[order(extract_last_name(all_players))]

# Calculate unique season counts dynamically from the data
season_counts <- pro_players_usports_data %>%
  filter(Season != "Total") %>%
  count(Player, name = "seasons") %>%
  pull(seasons) %>%
  unique()
num_seasons_sorted <- sort(season_counts)

# Sort pro seasons chronologically (descending - most recent first)
pro_seasons_sorted <- sort(unique(pro_data$Season), decreasing = TRUE)

# Sort leagues alphabetically
leagues_sorted <- sort(unique(pro_data$League))

# Sort USPORTS team
usports_teams_sorted <- sort(unique(pro_players_usports_data$`USports Team`))

# Getting algo functions
source("algo.R")