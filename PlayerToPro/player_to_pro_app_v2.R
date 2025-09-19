library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(rlang)
library(rsconnect)

# Test Data
players <- data.frame(
  Player = c("Alice", "Bob", "Charlie"),
  Team   = c("Acadia", "Alberta", "Waterloo")
)

usports_data <- read_excel("data/usports_stats_with_teams.xlsx")
pro_data <- read_excel("data/pro_season_data.xlsx")

# Helper function to extract last name for sorting
extract_last_name <- function(full_name) {
  sapply(strsplit(full_name, " "), function(x) x[length(x)])
}

# Sort players by last name
all_players <- unique(c(usports_data$Player, pro_data$Player))
all_players_sorted <- all_players[order(extract_last_name(all_players))]

# Calculate unique season counts dynamically from the data
season_counts <- usports_data %>%
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
usports_teams_sorted <- sort(unique(usports_data$`USports Team`))

ui <- fluidPage(
  titlePanel("Player To Pro Dashboard"),
  br(),
  fluidRow(
    # Current Player Section
    column(
      6,
      shinyWidgets::pickerInput("usports_teams", "Please Select a USPORTS Team: ", usports_teams_sorted, multiple = TRUE, options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")),
      dataTableOutput("current_usports_players_table")
    ),
    # Pro Player Section
    column(
      6,
      p("This is where the pro players info will go")
    ),
  )
)

server <- function(input, output, session) {
  filtered_players <- reactive({
    if (is.null(input$usports_teams) || length(input$usports_teams) == 0) {
      # No team is selected / all teams are cleared display all players
      players
    } else {
      # If team is selected show filters
      players %>% filter(Team %in% input$usports_teams)
    }
  })
  
  output$current_usports_players_table <- renderDataTable({
    filtered_players()
  })
}

shinyApp(ui, server)
