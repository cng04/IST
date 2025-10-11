import pandas as pd
from pathlib import Path

base_path = Path(__file__).resolve().parent.parent / "data"

# current usports players
current_players = pd.read_excel(base_path / "processed_current_usports_players.xlsx")

# pro players usports data
pro_players_usports_data = pd.read_excel(base_path / "usports_stats_with_teams.xlsx")

# pro players pro data
pro_data = pd.read_excel(base_path / "pro_season_data.xlsx")


# ==== Helper: extract last name for sorting ====
def extract_last_name(full_name: str) -> str:
    """Return last name of a player from full name string."""
    if isinstance(full_name, str):
        parts = full_name.split()
        return parts[-1] if parts else ""


# ==== Sort players by last name ====
all_players = pd.concat(
    [
        pro_players_usports_data["Player"],
        pro_data["Player"],
    ],
    ignore_index=True,
).dropna().unique()

all_players_sorted = sorted(all_players, key=extract_last_name)


# ==== Calculate unique season counts dynamically from the data ====
season_counts = (
    pro_players_usports_data[pro_players_usports_data["Season"] != "Total"]
    .groupby("Player")
    .size()
    .unique()
)
num_seasons_sorted = sorted(season_counts)


# ==== Sort pro seasons chronologically (descending - most recent first) ====
def parse_season(val):
    try:
        return int(str(val).split("-")[0]) 
    except ValueError:
        return None

season_values = pro_data["Season"].dropna().unique()
parsed = [parse_season(v) for v in season_values if v != "Total"]
parsed = [v for v in parsed if v is not None]
pro_seasons_sorted = sorted(parsed, reverse=True)

# ==== Sort leagues alphabetically ====
leagues_sorted = sorted(pro_data["League"].dropna().unique())

# ==== Sort USPORTS teams alphabetically ====
usports_teams_sorted = sorted(
    pro_players_usports_data["USports Team"].dropna().unique()
)

# ==== Import your algorithm functions (equivalent of source("algo.R")) ====
# Make sure you have an `algo.py` file in the same directory
from algo import *  # or import specific functions if preferred
