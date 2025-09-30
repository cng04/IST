ui <- fluidPage(
  useShinyjs(),
  titlePanel("Player To Pro Dashboard"),
  # br(),
  fluidRow(
    # Current Player Section
    column(
      6,
      style = "overflow-x: auto;",
      # shinyWidgets::pickerInput("usports_teams", "Please Select a USPORTS Team: ", usports_teams_sorted, multiple = TRUE, options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")),
      # checkboxInput("filter_players_by_position", "Filter Pro Players by Position", FALSE),
      div(
        style = "display: flex; align-items: flex-start; gap: 15px",
        checkboxInput("filter_players_by_position", "Filter Pro Players by Position", FALSE),
        shinyWidgets::pickerInput("usports_teams", "Please Select a USPORTS Team: ", usports_teams_sorted, multiple = TRUE, options = list(`actions-box` = TRUE, `deselect-all-text` = "Clear", `select-all-text` = "All")),
        ),
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

