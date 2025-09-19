import pandas as pd
import numpy as np

def main():
    # Load the data
    print("Loading USports data...")
    usports_data = pd.read_excel("../data/usports_stats_with_teams.xlsx")
    
    # Filter to exclude Total rows
    usports_seasons_only = usports_data[usports_data['Season'] != 'Total'].copy()
    
    print(f"Data loaded. Total rows: {len(usports_seasons_only)}")
    print(f"Total unique players: {usports_seasons_only['Player'].nunique()}\n")
    
    # Find fifth-year players (players who played exactly 5 seasons)
    player_season_counts = usports_seasons_only.groupby('Player').size().reset_index(name='num_seasons')
    fifth_year_players = player_season_counts[player_season_counts['num_seasons'] == 5]['Player'].tolist()
    
    print(f"Fifth-year players (played exactly 5 seasons): {len(fifth_year_players)}")
    
    # Get all data for fifth-year players
    fifth_year_data = usports_seasons_only[usports_seasons_only['Player'].isin(fifth_year_players)].copy()
    
    print(f"Total season records for fifth-year players: {len(fifth_year_data)}\n")
    
    # Analyze PF column for fifth-year players
    print("=== PF COLUMN ANALYSIS FOR FIFTH-YEAR PLAYERS ===")
    pf_values = fifth_year_data['PF']
    print(f"Data type of PF column: {pf_values.dtype}")
    
    # Check unique values in PF column
    unique_pf = pf_values.unique()
    print(f"Unique PF values (first 20): {unique_pf[:20]}")
    
    # Count different types of values
    print(f"\nPF VALUE BREAKDOWN:")
    print(f"- Total PF values: {len(pf_values)}")
    print(f"- NA values: {pf_values.isna().sum()}")
    print(f"- Non-NA values: {pf_values.notna().sum()}")
    
    # Try to convert to numeric and see what happens
    pf_numeric = pd.to_numeric(pf_values, errors='coerce')
    print(f"- Values that convert to numeric: {pf_numeric.notna().sum()}")
    print(f"- Values lost in numeric conversion: {pf_numeric.isna().sum() - pf_values.isna().sum()}")
    
    # Check for specific problematic values
    non_numeric_mask = pf_numeric.isna() & pf_values.notna()
    if non_numeric_mask.any():
        print(f"\nProblematic non-numeric PF values:")
        problematic_values = pf_values[non_numeric_mask].unique()
        for i, val in enumerate(problematic_values, 1):
            print(f"  {i}. '{val}'")
    
    # Calculate average PF per player for fifth-year players
    print(f"\n=== INDIVIDUAL PLAYER PF AVERAGES ===")
    
    player_pf_averages = []
    for player in fifth_year_players:
        player_data = fifth_year_data[fifth_year_data['Player'] == player]
        pf_vals = player_data['PF'].tolist()
        pf_numeric_vals = pd.to_numeric(player_data['PF'], errors='coerce')
        
        avg_pf_converted = pf_numeric_vals.mean()
        valid_pf_count = pf_numeric_vals.notna().sum()
        
        player_pf_averages.append({
            'Player': player,
            'seasons_played': len(player_data),
            'pf_values': pf_vals,
            'avg_pf_converted': avg_pf_converted,
            'valid_pf_count': valid_pf_count
        })
    
    player_pf_df = pd.DataFrame(player_pf_averages)
    
    # Show problematic players (those with NaN averages)
    problematic_players = player_pf_df[player_pf_df['avg_pf_converted'].isna()]
    
    if len(problematic_players) > 0:
        print(f"Players with problematic PF averages:")
        for _, row in problematic_players.iterrows():
            player = row['Player']
            pf_vals = row['pf_values']
            print(f"  {player}: PF values = {pf_vals}")
    else:
        print("No players with problematic PF averages found.")
    
    # Calculate overall average PF for fifth-year players
    valid_players = player_pf_df[player_pf_df['avg_pf_converted'].notna()]
    
    if len(valid_players) > 0:
        overall_avg_pf = valid_players['avg_pf_converted'].mean()
        print(f"\n=== FINAL RESULT ===")
        print(f"Valid fifth-year players: {len(valid_players)} out of {len(player_pf_df)}")
        print(f"Overall average PF for fifth-year players: {overall_avg_pf:.2f}")
        
        # Show distribution of individual averages
        print(f"\nDistribution of individual PF averages:")
        print(f"Min: {valid_players['avg_pf_converted'].min():.2f}")
        print(f"Max: {valid_players['avg_pf_converted'].max():.2f}")
        print(f"Median: {valid_players['avg_pf_converted'].median():.2f}")
        print(f"Mean: {valid_players['avg_pf_converted'].mean():.2f}")
    else:
        print(f"\n=== FINAL RESULT ===")
        print("ERROR: No valid PF averages could be calculated for fifth-year players!")
    
    # Sample a few players to show their data
    print(f"\n=== SAMPLE PLAYER DATA ===")
    sample_players = fifth_year_players[:3]
    for player in sample_players:
        player_data = fifth_year_data[fifth_year_data['Player'] == player][['Player', 'Season', 'PF']]
        
        print(f"\n{player}:")
        print(player_data.to_string(index=False))
        pf_vals = player_data['PF'].tolist()
        print(f"  PF values: {pf_vals}")
        avg_pf = pd.to_numeric(player_data['PF'], errors='coerce').mean()
        print(f"  Average PF: {avg_pf:.2f}")
    
    print(f"\n=== VALIDATION COMPLETE ===")

if __name__ == "__main__":
    main()