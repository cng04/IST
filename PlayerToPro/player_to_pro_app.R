library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(rlang)

usports_data <- read_excel("data/cleaned_usports_player_box_score_stats.xlsx")
pro_data <- read_excel("data/pro_season_data.xlsx")

ui <- fluidPage(
  titlePanel("Player To Pro Dashboard"),
  
  # ---- Filters Row ----
  fluidRow(
    column(3, selectInput("player", "Player Name", 
                          choices = c("All", unique(c(usports_data$Player, pro_data$Player))))),
    column(3, selectInput("position", "Position",
                          choices = c("All", unique(c(usports_data$Position, pro_data$Position))))),
    column(3, selectInput("pro_season", "Pro Season", 
                          choices = c("All", unique(pro_data$Season)))),
    column(3, selectInput("league", "Pro League", 
                          choices = c("All", unique(pro_data$League))))
  ),
  br(),
  
  # ---- Main Tables ----
  tabsetPanel(
    tabPanel("USPORTS Stats", DTOutput("usports_table")),
    tabPanel("Pro Stats", DTOutput("pro_table"))
  )
)


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    # --- USPORTS ---
    df_us <- usports_data %>%
      filter(.data$Season != "Total")
    if (input$player != "All") {
      df_us <- df_us %>% filter(.data$Player == input$player)
    }
    if (input$position != "All") {
      df_us <- df_us %>% filter(.data$Position == input$position)
    }

    # --- PRO filters ---
    df_pro <- pro_data
    if (input$league != "All") {
      df_pro <- df_pro %>% filter(.data$League == input$league)
    }
    if (input$pro_season != "All") {
      df_pro <- df_pro %>% filter(.data$Season == input$pro_season)
    }
    
    common_players <- intersect(df_us$Player, df_pro$Player)
    
    df_us <- df_us %>% filter(.data$Player %in% common_players)
    df_pro <- df_pro %>% filter(.data$Player %in% common_players)
    
    list(usports = df_us, pro = df_pro)
  })
  
  # Render tables
  output$usports_table <- renderDT({
    datatable(filtered_data()$usports, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$pro_table <- renderDT({
    datatable(filtered_data()$pro, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# ---- Run App ----
shinyApp(ui, server)