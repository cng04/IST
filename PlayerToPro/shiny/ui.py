from shiny import ui
from data_global import usports_teams_sorted

app_ui = ui.page_fluid(
    # Page title
    ui.head_content(ui.tags.title("Player To Pro Dashboard")),
    ui.h2("Player To Pro Dashboard"),

    # --- Main layout row ---
    ui.row(
        # === Current Player Section ===
        ui.column(
            6,
            {"style": "overflow-x: auto;"},
            ui.div(
                {"style": "display: flex; align-items: flex-start; gap: 15px"},
                ui.input_checkbox(
                    "filter_players_by_position",
                    "Filter Pro Players by Position",
                    value=False,
                ),
                ui.input_selectize(
                    "usports_teams",
                    "Please Select a USPORTS Team:",
                    choices=usports_teams_sorted,
                    multiple=True,
                    options={
                        "plugins": ["remove_button"],
                        "placeholder": "Select teams...",
                    },
                ),
            ),
            ui.output_data_frame("current_usports_players_table"),
        ),

        # === Compare Button Section ===
        ui.column(
            2,
            {"style": "text-align: center;"},
            ui.br(),
            ui.input_action_button(
                "compare_btn",
                "Compare Players",
                class_="btn btn-secondary",
                style="opacity: 0.5;",
            ),
        ),

        # === Pro Player Section ===
        ui.column(
            6,
            {"style": "overflow-x: auto;"},
            ui.br(),
            ui.output_data_frame("pro_players_general_info_table"),
        ),
    ),
)
