library(shiny)
library(readxl)
library(DT)
library(tidyr)
library(dplyr)
library(rlang)
library(rsconnect)
library(shinyjs)

# current usports players
current_players <- read_excel("data/processed_current_usports_players.xlsx")

# pro players usports data
pro_players_usports_data <- read_excel("data/usports_stats_with_teams.xlsx")

# pro players pro data
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

# Function to standardize specific columns in a data frame
scale_stats <- function(df, cols) {
  # across applies the scale function to each column in cols
  # scale subtracts the column's mean, divides by the column's standard deviation
  df %>% mutate(across(all_of(cols), scale))
}

# Function to determine similarity score between current usport player and pro player
get_similar_players <- function(usports_player, k = 10) {
  usports_player <- trimws(usports_player)
  
  # Columns to compare
  stat_cols <- c("PPG", "REB", "AST", "STL", "BLK", "FG%", "3P%", "FT%")
  
  # Grab the USPORTS player's Total row
  u_row <- current_players %>%
    filter(trimws(Player) == usports_player, Season == "Total") %>%
    select(all_of(stat_cols))
  
  # Grab pro players with Total row
  pro_raw <- pro_players_usports_data %>%
    filter(Season == "Total") %>%
    select(Player, all_of(stat_cols), `All USports Teams Played For`) %>%
    drop_na()
  
  # Bind together for consistent scaling, current usports players tagged with "u" and pro player tagged with "p"
  all_data <- bind_rows(
    u_row %>% mutate(source = "u"),
    pro_raw %>% select(all_of(stat_cols)) %>% mutate(source = "p")
  )
  
  # Scale selected stats (drop = FALSE ensures it's still a data frame)
  # Standardizes by subtracting the column mean then dividing by standard deviation for that column
  scaled_mat <- scale(as.matrix(all_data[, stat_cols, drop = FALSE]))
  
  # Separate back out
  u_scaled_vec <- scaled_mat[all_data$source == "u", , drop = FALSE]
  pro_scaled_mat <- scaled_mat[all_data$source == "p", , drop = FALSE]
  
  # Compute squared Euclidean distances
  dists <- rowSums((pro_scaled_mat - matrix(u_scaled_vec,
                                            nrow = nrow(pro_scaled_mat),
                                            ncol = ncol(pro_scaled_mat),
                                            byrow = TRUE))^2)
  
  # Add distances back to the raw pro dataset
  pro_raw$dist <- dists
  
  # Return top-k
  pro_raw %>%
    arrange(dist) %>% # arrange sorts players by increasing distance (smallest distance first)
    head(k) %>%
    select(Player, `All USports Teams Played For`, dist, all_of(stat_cols))
}


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
  filtered_current_usports_players <- reactive({
    data <- if (is.null(input$usports_teams) || length(input$usports_teams) == 0) {
      # No team is selected / all teams are cleared display all players
      current_players %>% filter(Season == "Total")
    } else {
      # input_values from the selected filter in R Shiny UI
      input_values <- trimws(strsplit(input$usports_teams, ",")[[1]])
      
      # If team is selected show filters
      current_players %>% filter(Season == "Total",
                                 sapply(strsplit(Team, ","), function(team_values) {
                                   # row by row
                                   # team_values are from data frame
                                   team_values <- trimws(team_values)
                                   
                                   # keep if there's any overlap
                                   any(team_values %in% input_values)
                                   
                                   # returns a vector, one value per row of Team indicating whether that row should be kept
                                 }))
    }
    
    # Keeping only columns we want
    data %>% select(Player, `All Seasons`, Team)
  })
  
  # Tracking which players were selected
  selected_usports_player <- reactive({
    s <- input$current_usports_players_table_rows_selected # this is the number of the row this player is located on
    if (length(s)) filtered_current_usports_players()[s, "Player"] else NULL # if no player selected this reactive is NULL
  })
  
  selected_pro_player <- reactive({
    s <- input$pro_players_general_info_table_rows_selected
    if (length(s)) filtered_pro_players_general_info()[s, "Player"] else NULL
  })
  
  
  filtered_pro_players_general_info <- reactive({
    if (is.null(selected_usports_player())) {
      data <- pro_players_usports_data %>% filter(Season == "Total")
    } else {
      data <- get_similar_players(selected_usports_player())
    }
    data %>% select(Player, `All USports Teams Played For`)
  })
  
  output$current_usports_players_table <- renderDataTable({
    DT::datatable(
      filtered_current_usports_players(),
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
    req(selected_usports_player(), selected_pro_player())
    
    ## see why the player names in the csv have trailing / leading whitespaces
    
    # ---- USPORTS side (left) ----
    usports_history <- current_players %>%
      filter(
        trimws(Player) == trimws(selected_usports_player())
      ) %>% arrange(Season) %>% select(-Player, -`All Seasons`)
    
    # ---- Pro player USPORTS side (right, above) ----
    pro_usports_history <- pro_players_usports_data %>%
      filter(trimws(Player) == trimws(selected_pro_player())) %>%
      arrange(Season) %>% select(-Player, -Height, -Hometown, -`All USports Teams Played For`)
    
    # ---- Pro player PRO side (right, below) ----
    pro_history <- pro_data %>%
      filter(trimws(Player) == trimws(selected_pro_player())) %>%
      arrange(desc(Season)) %>% select(-Player)
    
    showModal(modalDialog(
      title = paste(selected_usports_player(), "vs", selected_pro_player()),
      size = "xl",
      easyClose = TRUE,
      footer = NULL,
      
      # --- Custom CSS for full-screen modal ---
      tags$head(tags$style(HTML("
        .modal-dialog {
          width: 95% !important;   /* take almost full width */
          max-width: 95% !important;
        }
        .modal-body {
          max-height: 90vh;        /* fit within viewport height */
          overflow-y: auto;        /* scroll if content overflows */
        }
      "))),
      
      fluidRow(
        column(
          6,
          h4(paste(selected_usports_player(), " - USPORTS")),
          renderDT({
            datatable(
              usports_history,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE
            )
          })
        ),
        column(
          6,
          h4(paste(selected_pro_player(), " - USPORTS")),
          renderDT({
            datatable(
              pro_usports_history,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE
            )
          }),
          br(),
          h4(paste(selected_pro_player(), " - Professional Career")),
          renderDT({
            datatable(
              pro_history,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE
            )
          })
        )
      )
    ))
  })
}

shinyApp(
  ui = tagList(useShinyjs(), ui), 
  server = server
)
