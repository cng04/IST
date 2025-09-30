server <- function(input, output, session) {
  output$current_usports_player_position <- renderText({
      req(selected_usports_player())
      current_players %>% filter(
      trimws(Player) == trimws(selected_usports_player()), Season == "Total"
    ) %>% pull(Position)
  })
  
  output$pro_player_position <- renderText({
      req(selected_pro_player())
      pro_players_usports_data %>% filter(
      trimws(Player) == trimws(selected_pro_player()), Season == "Total"
    ) %>% pull(Position)
  })

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
    s1 <- input$pro_players_general_info_table_rows_selected
    s2 <- input$pro_players_similar_positions_rows_selected
    
    idx <- if (length(s1)) s1 else if (length(s2)) s2 else NULL
    
    if (!is.null(idx)) {
      return (filtered_pro_players_general_info()[idx, "Player"])
    } else {
      NULL
    }
  })
  
  filtered_pro_players_general_info <- reactive({
    if (is.null(selected_usports_player())) {
      data <- pro_players_usports_data %>% filter(Season == "Total")
    } else {
      data <- get_similar_players(selected_usports_player(), input$filter_players_by_position)
    }
    data %>% select(Player, `All USports Teams Played For`)
  })
  
  output$current_usports_players_table <- renderDataTable({
    DT::datatable(
      filtered_current_usports_players(),
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100))
    )
  })
  
  output$pro_players_general_info_table <- renderDataTable({
    DT::datatable(
      filtered_pro_players_general_info(),
      rownames = FALSE,
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
      arrange(Season) %>% select(-Player, -Height, -Hometown, -`All USports Teams Played For`) %>%
      rename(
        Team = `USports Team`
      )
    
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
          h4(selected_usports_player()),
          p(strong("Position: "), textOutput("current_usports_player_position", inline = TRUE)),
          br(),
          h4("USPORTS Career"),
          renderDT({
            datatable(
              usports_history,
              options = list(dom = "t", pageLength = 10, scrollX = TRUE),
              rownames = FALSE
            )
          })
        ),
        column(
          6,
          h4(selected_pro_player()),
          p(strong("Position: "), textOutput("pro_player_position", inline = TRUE)),
          br(),
          h4("USPORTS Career"),
          renderDT({
            datatable(
              pro_usports_history,
              options = list(dom = "t", pageLength = 10, scrollX = TRUE),
              rownames = FALSE
            )
          }),
          br(),
          h4("Professional Career"),
          renderDT({
            datatable(
              pro_history,
              options = list(dom = "t", pageLength = 10, scrollX = TRUE),
              rownames = FALSE
            )
          })
      )
    ))
  )})
}