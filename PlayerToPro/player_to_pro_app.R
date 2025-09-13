library(shiny)
library(readxl)
library(DT)
library(dplyr)
library(rlang)
library(rsconnect)

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
    tabPanel("Pro Transition Analysis", 
             uiOutput("transition_analysis_ui")
    )
    # tabPanel("USPORTS Stats", DTOutput("usports_table")),
    # tabPanel("Pro Info", DTOutput("pro_table"))
  )
)

server <- function(input, output, session) {
  # Reactive value to track selected seasons for player details
  selected_seasons <- reactiveVal(NULL)
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
    
    list(summary = transition_stats, player_details = player_analysis)
  })
  
  # Create detailed player data for child rows
  player_details_data <- reactive({
    # Start with filtered USports data to get last season team info
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
    
    # Get last season info for each player
    last_season_info <- df_filtered %>%
      group_by(.data$Player) %>%
      slice_max(.data$Season, n = 1, with_ties = FALSE) %>%
      select(.data$Player, Last_USports_Team = `USports Team`, Last_Season = Season) %>%
      ungroup()
    
    player_analysis <- filtered_transition_data()$player_details
    
    # Apply seasons filter to individual players
    if (!is.null(input$num_seasons_filter) && !"All" %in% input$num_seasons_filter) {
      player_analysis <- player_analysis %>% 
        filter(.data$Num_USports_Seasons %in% as.numeric(input$num_seasons_filter))
    }
    
    # Join with last season info and format for display
    player_analysis %>%
      left_join(last_season_info, by = "Player") %>%
      mutate(
        MPG = round(.data$Avg_MPG, 1),
        `3PM` = round(.data$Avg_3PM, 1),
        `3PA` = round(.data$Avg_3PA, 1),
        `3P%` = round(.data$Avg_3P_percent, 1),
        FGM = round(.data$Avg_FGM, 1),
        FGA = round(.data$Avg_FGA, 1),
        `FG%` = round(.data$Avg_FG_percent, 1),
        FTM = round(.data$Avg_FTM, 1),
        FTA = round(.data$Avg_FTA, 1),
        `FT%` = round(.data$Avg_FT_percent, 1),
        REB = round(.data$Avg_REB, 1),
        PF = round(.data$Avg_PF, 1),
        AST = round(.data$Avg_AST, 1),
        TOV = round(.data$Avg_TOV, 1),
        BLK = round(.data$Avg_BLK, 1),
        STL = round(.data$Avg_STL, 1),
        PPG = round(.data$Avg_PPG, 1)
      ) %>%
      select(
        Player, 
        `USports Seasons` = Num_USports_Seasons,
        `Last Team` = Last_USports_Team,
        MPG, `3PM`, `3PA`, `3P%`, FGM, FGA, `FG%`, 
        FTM, FTA, `FT%`, REB, PF, AST, TOV, BLK, STL, PPG
      ) %>%
      arrange(.data$`USports Seasons`, .data$Player)
  })

  # Render tables
  # output$usports_table <- renderDT({
  #   datatable(
  #     filtered_data()$usports,
  #     options = list(
  #       pageLength = 15,
  #       scrollX = TRUE,
  #       scrollY = "500px",
  #       dom = 'Bfrtip',
  #       columnDefs = list(
  #         list(className = 'dt-center', targets = "_all"),
  #         list(width = '80px', targets = c(0, 1, 2)),  # Player, Position, Height
  #         list(width = '120px', targets = c(3, 4)),     # Hometown, Team
  #         list(width = '60px', targets = c(5:ncol(filtered_data()$usports)-1))  # Stats columns
  #       )
  #     ),
  #     class = 'cell-border stripe hover',
  #     rownames = FALSE
  #   ) %>%
  #     formatRound(columns = c("MPG", "3P%", "FG%", "FT%", "REB", "PF", "AST", "TOV", "BLK", "STL", "PPG"), digits = 1)
  # })

  # output$pro_table <- renderDT({
  #   datatable(
  #     filtered_data()$pro,
  #     options = list(
  #       pageLength = 15,
  #       scrollX = TRUE,
  #       scrollY = "500px",
  #       dom = 'Bfrtip',
  #       columnDefs = list(
  #         list(className = 'dt-center', targets = "_all"),
  #         list(width = '120px', targets = c(0)),        # Player
  #         list(width = '80px', targets = c(1, 2, 3)),   # Season, Team, League
  #         list(width = '60px', targets = c(4:ncol(filtered_data()$pro)-1))  # Stats columns
  #       )
  #     ),
  #     class = 'cell-border stripe hover',
  #     rownames = FALSE
  #   ) %>%
  #     formatRound(columns = names(select_if(filtered_data()$pro, is.numeric)), digits = 1)
  # })
  
  # Simple working transition table first
  output$transition_analysis_ui <- renderUI({
    summary_data <- filtered_transition_data()$summary
    
    div(
      h4("Pro Transition Analysis"),
      p("Average statistics by number of USports seasons played (for players who went pro). Click a row to see individual players in a popup."),
      DTOutput("basic_transition_table")
    )
  })
  
  # Basic transition table with row selection
  output$basic_transition_table <- renderDT({
    summary_data <- filtered_transition_data()$summary
    
    datatable(
      summary_data,
      selection = 'single',
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '80px', targets = c(0, 1)),
          list(width = '60px', targets = c(2:ncol(summary_data)-1))
        )
      ),
      class = 'cell-border stripe hover',
      rownames = FALSE
    ) %>%
      formatRound(columns = setdiff(names(select_if(summary_data, is.numeric)), c("Number of USports Seasons Played", "Player Count")), digits = 1)
  })
  
  # Handle row selection and show modal popup
  observeEvent(input$basic_transition_table_rows_selected, {
    if (length(input$basic_transition_table_rows_selected) > 0) {
      selected_row <- input$basic_transition_table_rows_selected[1]
      summary_data <- filtered_transition_data()$summary
      selected_seasons_val <- summary_data$`Number of USports Seasons Played`[selected_row]
      selected_seasons(selected_seasons_val)
      
      # Get player data for this season count
      player_data <- player_details_data() %>%
        filter(`USports Seasons` == selected_seasons_val)
      
      # Show modal popup with custom width and close button
      showModal(modalDialog(
        title = div(
          style = "display: flex; justify-content: space-between; align-items: center; margin: 0;",
          span(paste0("Individual Players (", selected_seasons_val, " USports Seasons)")),
          actionButton("modal_close_btn", "×", 
                      style = "background: none; border: none; font-size: 20px; color: #6c757d; padding: 0; width: 30px; height: 30px; display: flex; align-items: center; justify-content: center;",
                      onclick = "Shiny.setInputValue('close_modal', Math.random(), {priority: 'event'});")
        ),
        size = "xl",
        easyClose = TRUE,
        footer = modalButton("Close"),
        
        tags$head(tags$style(HTML("
          .modal-xl {
            max-width: 98% !important;
            width: 98% !important;
          }
          .modal-body {
            padding: 5px;
          }
          .modal-dialog {
            margin: 10px auto;
          }
        "))),
        
        div(
          if (nrow(player_data) > 0) {
            schools <- unique(player_data$`Last Team`)
            schools <- schools[!is.na(schools)]
            schools <- sort(schools)
            
            tagList(
              # School filter dropdown
              div(
                style = "background-color: #f8f9fa; padding: 10px; margin-bottom: 15px; border-radius: 4px; border: 1px solid #dee2e6;",
                fluidRow(
                  column(4,
                    selectInput(
                      inputId = "modal_school_filter",
                      label = "Filter by School:",
                      choices = c("All Schools" = "all", setNames(schools, schools)),
                      selected = "all",
                      width = "100%"
                    )
                  ),
                  column(8,
                    div(
                      style = "padding-top: 25px;",
                      textOutput("modal_filter_info")
                    )
                  )
                )
              ),
              
              # Filtered content
              div(
                style = "max-height: 600px; overflow-y: auto;",
                uiOutput("filtered_schools_content")
              )
            )
          } else {
            p("No players found for this season count.", style = "text-align: center; padding: 20px;")
          }
        )
      ))
    }
  })
  
  # Clear row selection when modal is dismissed
  observeEvent(input$basic_transition_table_rows_selected, {
    # Small delay to allow modal to show first
    invalidateLater(100, session)
    dataTableProxy('basic_transition_table') %>% selectRows(NULL)
  }, once = FALSE, ignoreInit = TRUE)
  
  # Modal filter info text
  output$modal_filter_info <- renderText({
    req(selected_seasons())
    if (is.null(input$modal_school_filter) || input$modal_school_filter == "all") {
      player_data <- player_details_data() %>%
        filter(`USports Seasons` == selected_seasons())
      schools <- unique(player_data$`Last Team`)
      schools <- schools[!is.na(schools)]
      paste0("Showing all ", length(schools), " schools with ", nrow(player_data), " total players (grouped by school)")
    } else {
      player_data <- player_details_data() %>%
        filter(`USports Seasons` == selected_seasons(), `Last Team` == input$modal_school_filter)
      paste0("Showing ", nrow(player_data), " players from ", input$modal_school_filter)
    }
  })
  
  # Handle close modal button
  observeEvent(input$close_modal, {
    removeModal()
  })
  
  # Filtered schools content - single table with School column and Pro Info
  output$filtered_schools_content <- renderUI({
    req(selected_seasons())
    
    player_data <- player_details_data() %>%
      filter(`USports Seasons` == selected_seasons())
    
    # Apply school filter
    if (!is.null(input$modal_school_filter) && input$modal_school_filter != "all") {
      player_data <- player_data %>% filter(`Last Team` == input$modal_school_filter)
    }
    
    if (nrow(player_data) == 0) {
      return(p("No players found for the selected criteria.", style = "text-align: center; padding: 20px;"))
    }
    
    # Prepare data with School column and Pro Info button
    table_data <- player_data %>%
      rename(School = `Last Team`) %>%
      select(Player, School, MPG, `3PM`, `3PA`, `3P%`, FGM, FGA, `FG%`, 
             FTM, FTA, `FT%`, REB, PF, AST, TOV, BLK, STL, PPG) %>%
      arrange(School, Player)
    
    # Add Pro Info column with buttons (will be moved to position 4, before MPG)
    pro_info_col <- sapply(1:nrow(table_data), function(i) {
      player_name <- table_data$Player[i]
      
      # Get pro info for this player
      player_pro_data <- pro_data %>% filter(Player == player_name)
      
      if (nrow(player_pro_data) > 0) {
        # Create tooltip content
        pro_info_text <- player_pro_data %>%
          mutate(info = paste0(Season, ": ", Team, " (", League, ", ", Country, ")")) %>%
          pull(info) %>%
          paste(collapse = "<br/>")
        
        paste0('<button class="btn btn-info btn-sm" data-toggle="tooltip" data-html="true" title="',
               pro_info_text, '" style="font-size: 10px; padding: 2px 6px;">View</button>')
      } else {
        '<span style="color: #999; font-size: 10px;">No data</span>'
      }
    })
    
    # Reorder columns to put Pro Info before MPG
    table_data <- table_data %>%
      select(Player, School, all_of("MPG")) %>%
      mutate(`Pro Info` = pro_info_col) %>%
      select(Player, School, `Pro Info`, everything()) %>%
      bind_cols(table_data %>% select(-Player, -School, -MPG)) %>%
      select(Player, School, `Pro Info`, MPG, everything())
    
    # Render single table
    div(
      style = "max-height: 600px; overflow-y: auto;",
      # Add custom CSS for tooltips
      tags$style(HTML("
        .tooltip-inner {
          max-width: 300px;
          text-align: left;
          font-size: 11px;
          background-color: #333;
        }
        .tooltip.show {
          opacity: 1;
        }
      ")),
      
      # Add JavaScript to initialize tooltips
      tags$script(HTML("
        $(document).ready(function(){
          // Initialize tooltips after table renders
          setTimeout(function() {
            $('[data-toggle=\"tooltip\"]').tooltip({
              placement: 'right',
              trigger: 'hover'
            });
          }, 100);
        });
      ")),
      
      renderDT({
        datatable(
          table_data,
          options = list(
            pageLength = 50,
            scrollX = TRUE,
            dom = 'frtip',
            order = list(list(1, 'asc'), list(0, 'asc')), # Sort by School then Player
            columnDefs = list(
              list(className = 'dt-center', targets = "_all"),
              list(width = '120px', targets = 0),      # Player name
              list(width = '100px', targets = 1),      # School
              list(width = '70px', targets = 2, orderable = FALSE), # Pro Info column
              list(width = '50px', targets = c(3:ncol(table_data)-1)) # Stats columns (MPG onwards)
            )
          ),
          class = 'table-sm table-striped',
          rownames = FALSE,
          escape = FALSE # Allow HTML in Pro Info column
        ) %>%
          formatRound(columns = setdiff(names(select_if(table_data[, 1:(ncol(table_data)-1)], is.numeric)), 
                                       c("USports Seasons")), digits = 1)
      })
    )
  })
  
}

# ---- Run App ----
shinyApp(ui, server)
