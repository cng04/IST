library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(rlang)
library(rsconnect)
library(shinyjs)

# Test Data
current_players <- read_excel("data/processed_usports_players.xlsx")

pro_players_usports_data <- read_excel("data/usports_stats_with_teams.xlsx")
pro_data <- read_excel("data/pro_season_data.xlsx")

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

ui <- fluidPage(
  titlePanel("Player To Pro Dashboard"),
  br(),
  fluidRow(
    # Current Player Section
    column(
      6,
      style = "overflow-x: auto;",
      shinyWidgets::pickerInput("usports_teams", "Please Select a USPORTS Team: ", usports_teams_sorted, multiple = TRUE, options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")),
      dataTableOutput("current_usports_players_table")
    ),
    column(
      2,
      align = "center",
      br(),
      actionButton("compare_btn", "Compare Players", 
                   class = "btn btn-secondary", 
                   style = "opacity: 0.5;")
    ),
    # Pro Player Section
    column(
      6,
      style = "overflow-x: auto;",
      br(),
      dataTableOutput("pro_players_general_info_table")
    ),
  )
)

server <- function(input, output, session) {
  filtered_usports_players <- reactive({
    data <- if (is.null(input$usports_teams) || length(input$usports_teams) == 0) {
      # No team is selected / all teams are cleared display all players
      current_players %>% filter(Season == "Total")
    } else {
      # If team is selected show filters
      current_players %>% filter(Season == "Total", Team %in% input$usports_teams)
    }
    
    # Keeping only columns we want
    data %>% select(Player, `All Seasons`, Team)
  })
  
  filtered_pro_players_general_info <- reactive({
    data <- pro_players_usports_data %>% filter(Season == "Total")
    data %>% select(Player, `All USports Teams Played For`)
  })
  
  output$current_usports_players_table <- renderDataTable({
    DT::datatable(
      filtered_usports_players(),
      selection = "single",
      options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  output$pro_players_general_info_table <- renderDataTable({
    DT::datatable(
      filtered_pro_players_general_info(),
      selection = "single",
      options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  # Tracking which players were selected
  selected_usports_player <- reactive({
    s <- input$current_usports_players_table_rows_selected
    if (length(s)) filtered_usports_players()[s, "Player"] else NULL
  })
  
  selected_pro_player <- reactive({
    s <- input$pro_players_general_info_table_rows_selected
    if (length(s)) filtered_pro_players_general_info()[s, "Player"] else NULL
  })
  
  # Enable button once both players selected
  observe({
    if (!is.null(selected_usports_player()) && !is.null(selected_pro_player())) {
      shinyjs::enable("compare_btn")
      shinyjs::runjs("$('#compare_btn').css('opacity', 1)")
    } else {
      shinyjs::disable("compare_btn")
      shinyjs::runjs("$('#compare_btn').css('opacity', 0.5)")
    }
  })
  
  # Show modal when button clicked
  observeEvent(input$compare_btn, {
    showModal(modalDialog(
      title = "Compare Players",
      paste("USPORTS Player:", selected_usports_player()),
      br(),
      paste("Pro Player:", selected_pro_player()),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(
  ui = tagList(useShinyjs(), ui), 
  server = server
)
