from shiny import reactive, render, ui
import logging

from data_global import (
    current_players,
    pro_players_usports_data,
    pro_data,
)
from algo import get_similar_players
logging.basicConfig(level=logging.INFO)


def server(input, output, session):

    # === current_usports_player_position ===
    @output
    @render.text
    def current_usports_player_position():
        if not selected_usports_player():
            return ""
        player = selected_usports_player()
        pos = (
            current_players.loc[
                (current_players["Player"].str.strip() == player)
                & (current_players["Season"] == "Total"),
                "Position",
            ]
            .dropna()
            .values
        )
        return pos[0] if len(pos) else ""

    # === pro_player_position ===
    @output
    @render.text
    def pro_player_position():
        if not selected_pro_player():
            return ""
        player = selected_pro_player()
        pos = (
            pro_players_usports_data.loc[
                (pro_players_usports_data["Player"].str.strip() == player)
                & (pro_players_usports_data["Season"] == "Total"),
                "Position",
            ]
            .dropna()
            .values
        )
        return pos[0] if len(pos) else ""

    # === filtered_current_usports_players ===
    @reactive.calc
    def filtered_current_usports_players():
        teams = input.usports_teams()
        if not teams:
            data = current_players[current_players["Season"] == "Total"].copy()
        else:
            input_values = [t.strip() for t in teams]
            mask = current_players["Team"].apply(
                lambda x: any(tv in [t.strip() for t in str(x).split(",")] for tv in input_values)
            )
            data = current_players.loc[
                (current_players["Season"] == "Total") & mask, :
            ].copy()
        return data[["Player", "All Seasons", "Team"]]

    # === selected_usports_player ===
    @reactive.calc
    def selected_usports_player():
        s = input.current_usports_players_table_selected_rows()
        if s:
            df = filtered_current_usports_players()
            return df.iloc[s[0]]["Player"]
        return None

    # === filtered_pro_players_general_info ===
    @reactive.calc
    def filtered_pro_players_general_info():
        player = selected_usports_player()

        if player is None:
            # filter for "Total" rows (case and whitespace insensitive)
            data = pro_players_usports_data[
                pro_players_usports_data["Season"].astype(str).str.strip().str.lower() == "total"
            ].copy()

            # fallback: if no "total" rows exist, show all
            if data.empty:
                data = pro_players_usports_data.copy()

        else:
            # get similar players based on selected U SPORTS player
            data = get_similar_players(
                player,
                input.filter_players_by_position()
            )

        # return only relevant columns
        subset_cols = [c for c in ["Player", "All USports Teams Played For"] if c in data.columns]
        return data[subset_cols]

    # === selected_pro_player ===
    @reactive.calc
    def selected_pro_player():
        s1 = input.pro_players_general_info_table_selected_rows()
        if s1:
            df = filtered_pro_players_general_info()
            return df.iloc[s1[0]]["Player"]
        return None

    # === current_usports_players_table ===
    @output
    @render.data_frame
    def current_usports_players_table():
        df = filtered_current_usports_players()
        return render.DataGrid(
            df,
            selection_mode="row"
        )

    # === pro_players_general_info_table ===
    @output
    @render.data_frame
    def pro_players_general_info_table():
        df = filtered_pro_players_general_info()
        return render.DataGrid(
            df,
            selection_mode="row"
        )

    # === Enable / disable Compare button ===
    @reactive.effect
    def _():
        player_u = selected_usports_player()
        player_p = selected_pro_player()

        if player_u and player_p:
            ui.update_action_button("compare_btn", label="Compare Players", disabled=False)
        else:
            ui.update_action_button("compare_btn", label="Compare Players", disabled=True)

    # === Compare button click ===
    @reactive.effect
    @reactive.event(input.compare_btn)
    def _():
        player_u = selected_usports_player()
        player_p = selected_pro_player()
        if not (player_u and player_p):
            return

        # ---- USPORTS side ----
        usports_history = (
            current_players[current_players["Player"].str.strip() == player_u]
            .sort_values("Season")
            .drop(columns=["Player", "All Seasons"], errors="ignore")
        )

        # ---- Pro player USPORTS side ----
        pro_usports_history = (
            pro_players_usports_data[
                pro_players_usports_data["Player"].str.strip() == player_p
            ]
            .sort_values("Season")
            .drop(
                columns=[
                    "Player",
                    "Height",
                    "Hometown",
                    "All USports Teams Played For",
                ],
                errors="ignore",
            )
            .rename(columns={"USports Team": "Team"})
        )

        # ---- Pro player PRO side ----
        pro_history = (
            pro_data[pro_data["Player"].str.strip() == player_p]
            .sort_values("Season", ascending=False)
            .drop(columns=["Player"], errors="ignore")
        )

        # ---- Modal dialog ----
        ui.modal_show(
            ui.modal(
                ui.div(
                    ui.row(
                        ui.column(
                            6,
                            ui.h4(player_u),
                            ui.p(
                                ui.strong("Position: "),
                                ui.output_text("current_usports_player_position"),
                            ),
                            ui.h4("USPORTS Career"),
                            ui.output_data_frame(
                                lambda: render.DataGrid(usports_history)
                            ),
                        ),
                        ui.column(
                            6,
                            ui.h4(player_p),
                            ui.p(
                                ui.strong("Position: "),
                                ui.output_text("pro_player_position"),
                            ),
                            ui.h4("USPORTS Career"),
                            ui.output_data_frame(
                                lambda: render.DataGrid(pro_usports_history)
                            ),
                            ui.h4("Professional Career"),
                            ui.output_data_frame(
                                lambda: render.DataGrid(pro_history)
                            ),
                        ),
                    )
                ),
                title=f"{player_u} vs {player_p}",
                easy_close=True,
                size="xl",
            )
        )
