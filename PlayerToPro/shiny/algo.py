import pandas as pd
import numpy as np
from data_global import current_players, pro_players_usports_data


# === Equivalent to scale_stats() ===
def scale_stats(df: pd.DataFrame, cols: list[str]) -> pd.DataFrame:
    """
    Standardize specific columns in a DataFrame:
    subtract mean and divide by standard deviation for each column.
    """
    df_scaled = df.copy()
    df_scaled[cols] = (df_scaled[cols] - df[cols].mean()) / df[cols].std(ddof=0)
    return df_scaled


# === Equivalent to similar_positions_map ===
similar_positions_map = {
    "G": ["G", "PG", "SG"],
    "F": ["F", "SF", "PF"],
    "C": ["C", "F/C", "C/F"],
    "G/F": ["G", "F", "SF", "SG", "G/F"],
    "PG": ["PG", "G"],
    "SG": ["SG", "G"],
    "SF": ["SF", "F"],
    "PF": ["PF", "F"],
}

# === Equivalent to get_similar_players() ===
def get_similar_players(usports_player: str, filter_by_similar_position: bool = False, k: int = 10):
    """
    Compute top-k most similar pro players to a given USPORTS player using scaled Euclidean distance.
    Matches R Shiny's get_similar_players() logic exactly.
    """

    usports_player = usports_player.strip().lower()

    # Columns to compare
    stat_cols = ["PPG", "REB", "AST", "STL", "BLK", "FG%", "3P%", "FT%"]

    # --- Grab the USPORTS player's Total row ---
    u_row = (
        current_players.loc[
            (current_players["Player"].astype(str).str.strip().str.lower() == usports_player)
            & (current_players["Season"].astype(str).str.strip().str.lower() == "total"),
            ["Player", "Position"] + stat_cols,
        ]
        .dropna()
        .reset_index(drop=True)
    )

    if u_row.empty:
        raise ValueError(f"Player '{usports_player}' not found in current_players")

    # --- Grab pro players with Total row ---
    pro_raw = (
        pro_players_usports_data.loc[
            pro_players_usports_data["Season"].astype(str).str.strip().str.lower() == "total",
            ["Player", "Position", "All USports Teams Played For"] + stat_cols,
        ]
        .dropna()
        .reset_index(drop=True)
    )

    # --- Scale only on pro players ---
    pro_scaled = pro_raw.copy()
    center = pro_scaled[stat_cols].mean()
    scale_vals = pro_scaled[stat_cols].std(ddof=0)

    pro_scaled_mat = (pro_scaled[stat_cols] - center) / scale_vals

    # --- Apply the same transform to the USPORTS player ---
    u_scaled_vec = (u_row[stat_cols] - center) / scale_vals

    # --- Compute squared Euclidean distances ---
    diff = pro_scaled_mat.values - u_scaled_vec.values
    dists = np.sum(diff**2, axis=1)
    pro_raw["dist"] = dists

    # --- Optional position filter ---
    if filter_by_similar_position:
        u_position = u_row["Position"].iloc[0]
        valid_positions = similar_positions_map.get(u_position)
        if valid_positions:
            pro_raw = pro_raw[pro_raw["Position"].isin(valid_positions)]

    # --- Sort by distance and return top-k ---
    top_k = (
        pro_raw.sort_values("dist")
        .head(k)
        .copy()
    )

    # Round stat columns to one decimal (similar to number(., accuracy = 0.1))
    top_k[stat_cols] = top_k[stat_cols].applymap(lambda x: round(x, 1) if pd.notnull(x) else x)

    # Select columns in final order
    result = top_k[["Player", "All USports Teams Played For", "dist"] + stat_cols]

    return result
