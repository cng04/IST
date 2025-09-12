library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(rlang)

usports_data <- read_excel("data/usports_stats_with_teams.xlsx")
pro_data <- read_excel("data/pro_season_data.xlsx")

# Helper function to extract last name for sorting
extract_last_name <- function(full_name) {
  sapply(strsplit(full_name, " "), function(x) x[length(x)])
}

# Sort players by last name
all_players <- unique(c(usports_data$Player, pro_data$Player))
all_players_sorted <- all_players[order(extract_last_name(all_players))]

# Sort other categorical variables alphabetically
positions_sorted <- sort(unique(usports_data$Position))
usports_teams_sorted <- sort(unique(usports_data$`USports Team`))
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

ui <- fluidPage(
  titlePanel("Player To Pro Dashboard"),

  # ---- Conditional Filters Row ----
  conditionalPanel(
    condition = "input.tabset != 'Pro Transition Analysis'",
    fluidRow(
      column(3, shinyWidgets::pickerInput(
        "player", "Player Name",
        choices = c("All", all_players_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")
      )),
      column(3, shinyWidgets::pickerInput(
        "position", "Position",
        choices = c("All", positions_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")
      )),
      column(3, shinyWidgets::pickerInput(
        "usports_team", "USports Team",
        choices = c("All", usports_teams_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")
      )),
      column(3, shinyWidgets::pickerInput(
        "pro_season", "Pro Season",
        choices = c("All", pro_seasons_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")
      )),
      column(3, shinyWidgets::pickerInput(
        "league", "Pro League",
        choices = c("All", leagues_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")
      ))
    ),
    br()
  ),
  
  # ---- Pro Transition Analysis Filters ----
  conditionalPanel(
    condition = "input.tabset == 'Pro Transition Analysis'",
    fluidRow(
      column(3, shinyWidgets::pickerInput(
        "transition_position", "Position",
        choices = c("All", positions_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")
      )),
      column(3, shinyWidgets::pickerInput(
        "transition_usports_team", "USports Team",
        choices = c("All", usports_teams_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")
      )),
      column(3, shinyWidgets::pickerInput(
        "num_seasons_filter", "Number of USports Seasons Played",
        choices = c("All", num_seasons_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")
      ))
    ),
    br()
  ),

  # ---- Main Tables ----
  tabsetPanel(
    id = "tabset",
    tabPanel("USPORTS Stats", DTOutput("usports_table")),
    tabPanel("Pro Info", DTOutput("pro_table")),
    tabPanel("Pro Transition Analysis", DTOutput("transition_table"))
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    # --- USPORTS ---
    df_us <- usports_data %>%
      filter(.data$Season != "Total")

    if (!is.null(input$player) && !"All" %in% input$player) {
      df_us <- df_us %>% filter(.data$Player %in% input$player)
    }

    if (!is.null(input$usports_team) && !"All" %in% input$usports_team) {
      df_us <- df_us %>% filter(.data$`USports Team` %in% input$usports_team)
    }

    # --- PRO filters ---
    df_pro <- pro_data

    if (!is.null(input$league) && !"All" %in% input$league) {
      df_pro <- df_pro %>% filter(.data$League %in% input$league)
    }

    if (!is.null(input$pro_season) && !"All" %in% input$pro_season) {
      df_pro <- df_pro %>% filter(.data$Season %in% input$pro_season)
    }

    # --- Match players across datasets ---
    common_players <- intersect(df_us$Player, df_pro$Player)

    df_us <- df_us %>% filter(.data$Player %in% common_players)
    df_pro <- df_pro %>% filter(.data$Player %in% common_players)

    list(usports = df_us, pro = df_pro)
  })
  
  # Create dynamic transition analysis based on filters
  filtered_transition_data <- reactive({
    # Start with USports data (excluding Total rows)
    df_filtered <- usports_data %>%
      filter(.data$Season != "Total")
    
    # Apply position filter
    if (!is.null(input$transition_position) && !"All" %in% input$transition_position) {
      df_filtered <- df_filtered %>% filter(.data$Position %in% input$transition_position)
    }
    
    # Apply USports team filter
    if (!is.null(input$transition_usports_team) && !"All" %in% input$transition_usports_team) {
      df_filtered <- df_filtered %>% filter(.data$`USports Team` %in% input$transition_usports_team)
    }
    
    # Recreate the transition analysis with filtered data
    player_analysis <- df_filtered %>%
      group_by(.data$Player) %>%
      summarise(
        Num_USports_Seasons = n(),
        Avg_MPG = mean(as.numeric(.data$MPG), na.rm = TRUE),
        Avg_3PM = mean(as.numeric(.data$`3PM`), na.rm = TRUE),
        Avg_3PA = mean(as.numeric(.data$`3PA`), na.rm = TRUE),
        Avg_3P_percent = mean(as.numeric(.data$`3P%`), na.rm = TRUE),
        Avg_FGM = mean(as.numeric(.data$FGM), na.rm = TRUE),
        Avg_FGA = mean(as.numeric(.data$FGA), na.rm = TRUE),
        Avg_FG_percent = mean(as.numeric(.data$`FG%`), na.rm = TRUE),
        Avg_FTM = mean(as.numeric(.data$FTM), na.rm = TRUE),
        Avg_FTA = mean(as.numeric(.data$FTA), na.rm = TRUE),
        Avg_FT_percent = mean(as.numeric(.data$`FT%`), na.rm = TRUE),
        Avg_REB = mean(as.numeric(.data$REB), na.rm = TRUE),
        Avg_PF = mean(as.numeric(.data$PF), na.rm = TRUE),
        Avg_AST = mean(as.numeric(.data$AST), na.rm = TRUE),
        Avg_TOV = mean(as.numeric(.data$TOV), na.rm = TRUE),
        Avg_BLK = mean(as.numeric(.data$BLK), na.rm = TRUE),
        Avg_STL = mean(as.numeric(.data$STL), na.rm = TRUE),
        Avg_PPG = mean(as.numeric(.data$PPG), na.rm = TRUE),
        .groups = "drop"
      )
    
    # Group by number of seasons and calculate overall averages
    transition_stats <- player_analysis %>%
      group_by(.data$Num_USports_Seasons) %>%
      summarise(
        Player_Count = n(),
        MPG = round(mean(.data$Avg_MPG, na.rm = TRUE), 1),
        `3PM` = round(mean(.data$Avg_3PM, na.rm = TRUE), 1),
        `3PA` = round(mean(.data$Avg_3PA, na.rm = TRUE), 1),
        `3P%` = round(mean(.data$Avg_3P_percent, na.rm = TRUE), 1),
        FGM = round(mean(.data$Avg_FGM, na.rm = TRUE), 1),
        FGA = round(mean(.data$Avg_FGA, na.rm = TRUE), 1),
        `FG%` = round(mean(.data$Avg_FG_percent, na.rm = TRUE), 1),
        FTM = round(mean(.data$Avg_FTM, na.rm = TRUE), 1),
        FTA = round(mean(.data$Avg_FTA, na.rm = TRUE), 1),
        `FT%` = round(mean(.data$Avg_FT_percent, na.rm = TRUE), 1),
        REB = round(mean(.data$Avg_REB, na.rm = TRUE), 1),
        PF = round(mean(.data$Avg_PF, na.rm = TRUE), 1),
        AST = round(mean(.data$Avg_AST, na.rm = TRUE), 1),
        TOV = round(mean(.data$Avg_TOV, na.rm = TRUE), 1),
        BLK = round(mean(.data$Avg_BLK, na.rm = TRUE), 1),
        STL = round(mean(.data$Avg_STL, na.rm = TRUE), 1),
        PPG = round(mean(.data$Avg_PPG, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(.data$Num_USports_Seasons) %>%
      # Rename columns for better display
      rename(
        `Number of USports Seasons Played` = Num_USports_Seasons,
        `Player Count` = Player_Count
      )
    
    # Apply seasons filter after calculation
    if (!is.null(input$num_seasons_filter) && !"All" %in% input$num_seasons_filter) {
      transition_stats <- transition_stats %>% 
        filter(.data$`Number of USports Seasons Played` %in% as.numeric(input$num_seasons_filter))
    }
    
    transition_stats
  })

  # Render tables
  output$usports_table <- renderDT({
    datatable(
      filtered_data()$usports,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '80px', targets = c(0, 1, 2)),  # Player, Position, Height
          list(width = '120px', targets = c(3, 4)),     # Hometown, Team
          list(width = '60px', targets = c(5:ncol(filtered_data()$usports)-1))  # Stats columns
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    ) %>%
      formatRound(columns = c("MPG", "3P%", "FG%", "FT%", "REB", "PF", "AST", "TOV", "BLK", "STL", "PPG"), digits = 1)
  })

  output$pro_table <- renderDT({
    datatable(
      filtered_data()$pro,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '120px', targets = c(0)),        # Player
          list(width = '80px', targets = c(1, 2, 3)),   # Season, Team, League
          list(width = '60px', targets = c(4:ncol(filtered_data()$pro)-1))  # Stats columns
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    ) %>%
      formatRound(columns = names(select_if(filtered_data()$pro, is.numeric)), digits = 1)
  })
  
  # Render transition analysis table
  output$transition_table <- renderDT({
    datatable(
      filtered_transition_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '80px', targets = c(0, 1)),      # Num_USports_Seasons, Player_Count
          list(width = '60px', targets = c(2:ncol(filtered_transition_data())-1))  # Stats columns
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE,
      caption = "Average statistics by number of USports seasons played (for players who went pro)"
    ) %>%
      formatRound(columns = names(select_if(filtered_transition_data(), is.numeric)), digits = 1)
  })
}

# ---- Run App ----
shinyApp(ui, server)
