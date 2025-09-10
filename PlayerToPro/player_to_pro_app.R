library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(rlang)

usports_data <- read_excel("data/usports_stats_with_teams.xlsx")
pro_data <- read_excel("data/pro_season_data.xlsx")
pro_transition_data <- read_excel("data/pro_transition_stats_by_seasons.xlsx")

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
num_seasons_sorted <- sort(unique(pro_transition_data$Num_USports_Seasons))

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
        options = list(`actions-box` = TRUE)
      )),
      column(3, shinyWidgets::pickerInput(
        "position", "Position",
        choices = c("All", positions_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )),
      column(3, shinyWidgets::pickerInput(
        "usports_team", "USports Team",
        choices = c("All", usports_teams_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )),
      column(3, shinyWidgets::pickerInput(
        "pro_season", "Pro Season",
        choices = c("All", pro_seasons_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )),
      column(3, shinyWidgets::pickerInput(
        "league", "Pro League",
        choices = c("All", leagues_sorted),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ))
    ),
    br()
  ),

  # ---- Main Tables ----
  tabsetPanel(
    id = "tabset",
    tabPanel("USPORTS Stats", DTOutput("usports_table")),
    tabPanel("Pro Info", DTOutput("pro_table")),
    tabPanel("Pro Transition Analysis", 
             br(),
             fluidRow(
               column(4, shinyWidgets::pickerInput(
                 "num_seasons_filter", "Number of USports Seasons",
                 choices = c("All", num_seasons_sorted),
                 multiple = TRUE,
                 options = list(`actions-box` = TRUE)
               ))
             ),
             br(),
             DTOutput("transition_table")
    )
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
  
  # Filter transition data
  filtered_transition_data <- reactive({
    df_transition <- pro_transition_data
    
    if (!is.null(input$num_seasons_filter) && !"All" %in% input$num_seasons_filter) {
      df_transition <- df_transition %>% 
        filter(.data$Num_USports_Seasons %in% as.numeric(input$num_seasons_filter))
    }
    
    df_transition
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
      formatRound(columns = names(select_if(filtered_transition_data(), is.numeric))[-c(1,2)], digits = 1)
  })
}

# ---- Run App ----
shinyApp(ui, server)
