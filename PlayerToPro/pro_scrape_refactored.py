import requests
from bs4 import BeautifulSoup
import pandas as pd

# Define login URL and payload
login_url = "https://usportshoops.ca/common/loginformhandler.php"
payload = {
    "ushemail": "uwaggs@uwaterloo.ca",
    "ushpswd": "xxxx",  # Replace with your actual password
    "ushgender": "MBB",
    "ushremember": "1"
}

# Start a session to maintain cookies
session = requests.Session()

# Send POST request to login
login_response = session.post(login_url, data=payload)

# Check if login was successful
if login_response.ok:
    print("Login successful!")
else:
    print("Login failed!")

def format_player_name(full_name: str) -> str:
    parts = full_name.strip().split()
    
    if len(parts) == 2:
        # exactly two parts → last-first
        return f"{parts[1]}-{parts[0]}".lower().replace("'", "").replace(".", "")
    elif len(parts) >= 3:
        # three or more parts → only last and first
        return f"{parts[-1]}-{parts[0]}".lower().replace("'", "").replace(".", "")
    else:
        # single name
        return full_name.lower().replace("'", "").replace(".", "")

def scrape_table_with_headers(url, table_selector={"border": "1"}):
    """Generic function to scrape a table and return headers and data."""
    try:
        response = session.get(url)
        response.raise_for_status()
        soup = BeautifulSoup(response.content, "html5lib")
        table = soup.find("table", table_selector)
        
        if not table:
            return None, None
            
        headers = [th.get_text(strip=True) for th in table.find_all("th")]
        rows_data = []
        
        for row in table.find_all("tr")[1:]:
            cols = row.find_all("td")
            if len(cols) > 1:
                row_data = [col.get_text(strip=True) for col in cols]
                rows_data.append(row_data)
                
        return headers, rows_data
    except requests.RequestException as e:
        print(f"Error fetching {url}: {e}")
        return None, None

def scrape_player_summary(summary_url):
    """Scrape the main player summary table and extract player information."""
    headers, player_data = scrape_table_with_headers(summary_url)
    
    if not headers or not player_data:
        print("Failed to scrape player summary data")
        return None, None
        
    player_names = []
    for row in player_data:
        if row:
            player_name = row[0]
            player_names.append({
                "url_name": format_player_name(player_name), 
                "readable_name": player_name
            })
    
    df = pd.DataFrame(player_data, columns=headers)
    return df, player_names

def scrape_pro_seasons_for_players(player_names, pro_seasons_url):
    """Scrape professional season data for all players."""
    all_pro_seasons = []
    pro_headers = []
    failed_players = []
    
    for player in player_names:
        print(f"Scraping pro seasons for {player['url_name']}")
        url = f"{pro_seasons_url}{player['url_name']}"
        headers, rows_data = scrape_table_with_headers(url)
        
        if not headers or not rows_data:
            print(f"No pro seasons data found for {player['readable_name']}")
            failed_players.append(player['readable_name'])
            continue
            
        if not pro_headers:
            pro_headers = ["Player"] + headers
            
        for row in rows_data:
            season_data = [player["readable_name"]] + row
            all_pro_seasons.append(season_data)
    
    df = pd.DataFrame(all_pro_seasons, columns=pro_headers) if all_pro_seasons else pd.DataFrame()
    return df, failed_players

def scrape_usports_career_for_players(player_names, usports_stats_url):
    """Scrape USports career table (after 'Playing Career' title) for player, season, team data."""
    all_career_data = []
    career_headers = ["Player", "Season", "Team"]
    failed_players = []
    
    for player in player_names:
        print(f"Scraping USports career for {player['url_name']}")
        url = f"{usports_stats_url}{player['url_name']}"
        
        try:
            response = session.get(url)
            response.raise_for_status()
            soup = BeautifulSoup(response.content, "html5lib")
            
            # Look for "Playing career:" title
            marker = soup.find("b", string="Playing career:")
            if not marker:
                print(f"No 'Playing career:' section found for {player['readable_name']}")
                failed_players.append(player['readable_name'])
                continue
                
            # Find the table after the "Playing Career" title
            table = marker.find_next("table", {"border": "1"})
            if not table:
                print(f"No career table found for {player['readable_name']}")
                failed_players.append(player['readable_name'])
                continue
                
            # Extract data from each row (skip header row)
            for row in table.find_all("tr")[1:]:
                cols = row.find_all("td")
                if len(cols) >= 2:  # At least season and team columns
                    season = cols[0].get_text(strip=True)
                    team = cols[1].get_text(strip=True)
                    career_data = [player["readable_name"], season, team]
                    all_career_data.append(career_data)
                    
        except requests.RequestException as e:
            print(f"Error scraping USports career for {player['readable_name']}: {e}")
            failed_players.append(player['readable_name'])
    
    df = pd.DataFrame(all_career_data, columns=career_headers) if all_career_data else pd.DataFrame()
    return df, failed_players

def scrape_usports_stats_for_players(player_names, usports_stats_url):
    """Scrape USports statistics for all players."""
    all_usports_stats = []
    all_usports_headers = []
    failed_players = []
    
    for player in player_names:
        print(f"Scraping usports stats for {player['url_name']}")
        url = f"{usports_stats_url}{player['url_name']}"
        
        try:
            response = session.get(url)
            response.raise_for_status()
            soup = BeautifulSoup(response.content, "html5lib")
            
            marker = soup.find("b", string="Overall career statistics")
            if not marker:
                marker = soup.find("b", string="Career regular league season statistics")
                
            if not marker:
                print(f"No stats marker found for {player['readable_name']}")
                failed_players.append(player['readable_name'])
                continue
                
            table = marker.find_next("table", {"border": "1"})
            if not table:
                print(f"No usports stats table found for {player['readable_name']}")
                failed_players.append(player['readable_name'])
                continue
                
            if not all_usports_headers:
                expanded_headers = []
                for th in table.find_all("th"):
                    text = th.get_text(strip=True)
                    colspan = int(th.get("colspan", 1))
                    if colspan == 1:
                        expanded_headers.append(text)
                    else:
                        for i in range(colspan):
                            expanded_headers.append(f"{text}_{i+1}")
                all_usports_headers = ["Player"] + expanded_headers
                
            for row in table.find_all("tr")[1:]:
                cols = row.find_all("td")
                if len(cols) > 1:
                    stats_data = [player["readable_name"]] + [col.get_text(strip=True) for col in cols]
                    all_usports_stats.append(stats_data)
                    
        except requests.RequestException as e:
            print(f"Error scraping USports stats for {player['readable_name']}: {e}")
            failed_players.append(player['readable_name'])
    
    df = pd.DataFrame(all_usports_stats, columns=all_usports_headers) if all_usports_stats else pd.DataFrame()
    return df, failed_players


def save_dataframe_to_excel(df, filename, message=""):
    """Save DataFrame to Excel file with error handling."""
    try:
        df.to_excel(f"data/{filename}", index=False, engine="openpyxl")
        print(f"Data saved to {filename}")
        if message:
            print(message)
        print(df.head())
    except Exception as e:
        print(f"Error saving {filename}: {e}")

def print_failure_summary(step_name, failed_players, total_players):
    """Print a comprehensive summary of failed scrapes for a step."""
    print(f"\n{'='*60}")
    print(f"FAILURE SUMMARY - {step_name}")
    print(f"{'='*60}")
    print(f"Total players: {total_players}")
    print(f"Failed players: {len(failed_players)}")
    print(f"Success rate: {((total_players - len(failed_players)) / total_players * 100):.1f}%")
    
    if failed_players:
        print(f"\nPlayers we couldn't scrape ({len(failed_players)}):")
        for i, player in enumerate(failed_players, 1):
            print(f"{i:3d}. {player}")
    else:
        print("\n✓ All players scraped successfully!")
    print(f"{'='*60}")

def main():
    """Main execution function that checks for existing data before scraping."""
    import os
    
    # URLs
    summary_url = "https://usportshoops.ca/pro/pro-playerlist.php?Gender=MBB" 
    pro_seasons_url = "https://usportshoops.ca/pro/pro-player.php?Gender=MBB&Player="
    usports_stats_url = "https://usportshoops.ca/history/person.php?Gender=MBB&Person="
    
    ### Some urls of names with more than two parts are constructed differently, 
    ### right now will add them manually, but should automate this later
    
    # Initialize variables
    summary_df, player_names = None, []
    pro_seasons_df, usports_stats_df, career_df = pd.DataFrame(), pd.DataFrame(), pd.DataFrame()
    
    # Track failed players for each step
    all_failures = {}
    
    # Step 1: Load or scrape player summary
    summary_file = "data/usports_players_to_pro_summary.xlsx"
    if os.path.exists(summary_file):
        print("Step 1: Loading existing player summary from Excel...")
        try:
            summary_df = pd.read_excel(summary_file, engine="openpyxl")
            # Extract player names from the summary DataFrame
            player_names = []
            if not summary_df.empty and len(summary_df.columns) > 0:
                for _, row in summary_df.iterrows():
                    player_name = row.iloc[0]  # First column contains player names
                    player_names.append({
                        "url_name": format_player_name(player_name), 
                        "readable_name": player_name
                    })
            print(f"Loaded {len(player_names)} players from existing Excel file")
        except Exception as e:
            print(f"Error loading existing summary file: {e}")
            print("Step 1: Scraping player summary...")
            summary_df, player_names = scrape_player_summary(summary_url)
            if summary_df is not None:
                save_dataframe_to_excel(summary_df, "usports_players_to_pro_summary.xlsx")
    else:
        print("Step 1: Scraping player summary...")
        summary_df, player_names = scrape_player_summary(summary_url)
        if summary_df is not None:
            save_dataframe_to_excel(summary_df, "usports_players_to_pro_summary.xlsx")

    # Step 2: Load or scrape pro seasons data
    if player_names:
        pro_seasons_file = "data/pro_season_data.xlsx"
        if os.path.exists(pro_seasons_file):
            print("Step 2: Loading existing pro seasons from Excel...")
            try:
                pro_seasons_df = pd.read_excel(pro_seasons_file, engine="openpyxl")
                print(f"Loaded pro seasons data with {len(pro_seasons_df)} rows")
                all_failures["Step 2: Pro Seasons"] = []  # No failures when loading from file
            except Exception as e:
                print(f"Error loading existing pro seasons file: {e}")
                print("Step 2: Scraping pro seasons...")
                pro_seasons_df, failed_pro_seasons = scrape_pro_seasons_for_players(player_names, pro_seasons_url)
                all_failures["Step 2: Pro Seasons"] = failed_pro_seasons
                if not pro_seasons_df.empty:
                    save_dataframe_to_excel(pro_seasons_df, "pro_season_data.xlsx")
                print_failure_summary("Step 2: Pro Seasons", failed_pro_seasons, len(player_names))
        else:
            print("Step 2: Scraping pro seasons...")
            pro_seasons_df, failed_pro_seasons = scrape_pro_seasons_for_players(player_names, pro_seasons_url)
            all_failures["Step 2: Pro Seasons"] = failed_pro_seasons
            if not pro_seasons_df.empty:
                save_dataframe_to_excel(pro_seasons_df, "pro_season_data.xlsx")
            print_failure_summary("Step 2: Pro Seasons", failed_pro_seasons, len(player_names))
    
    # Step 3: Load or scrape USports stats data
    if player_names:
        usports_stats_file = "data/usports_stats_data.xlsx"
        if os.path.exists(usports_stats_file):
            print("Step 3: Loading existing USports stats from Excel...")
            try:
                usports_stats_df = pd.read_excel(usports_stats_file, engine="openpyxl")
                print(f"Loaded USports stats data with {len(usports_stats_df)} rows")
                all_failures["Step 3: USports Stats"] = []  # No failures when loading from file
            except Exception as e:
                print(f"Error loading existing USports stats file: {e}")
                print("Step 3: Scraping USports stats...")
                usports_stats_df, failed_usports_stats = scrape_usports_stats_for_players(player_names, usports_stats_url)
                all_failures["Step 3: USports Stats"] = failed_usports_stats
                if not usports_stats_df.empty:
                    save_dataframe_to_excel(usports_stats_df, "usports_stats_data.xlsx")
                print_failure_summary("Step 3: USports Stats", failed_usports_stats, len(player_names))
        else:
            print("Step 3: Scraping USports stats...")
            usports_stats_df, failed_usports_stats = scrape_usports_stats_for_players(player_names, usports_stats_url)
            all_failures["Step 3: USports Stats"] = failed_usports_stats
            if not usports_stats_df.empty:
                save_dataframe_to_excel(usports_stats_df, "usports_stats_data.xlsx")
            print_failure_summary("Step 3: USports Stats", failed_usports_stats, len(player_names))
    
    # Step 4: Load or scrape USports career data
    if player_names:
        career_file = "data/usports_career_data.xlsx"
        if os.path.exists(career_file):
            print("Step 4: Loading existing USports career data from Excel...")
            try:
                career_df = pd.read_excel(career_file, engine="openpyxl")
                print(f"Loaded USports career data with {len(career_df)} rows")
                all_failures["Step 4: USports Career"] = []  # No failures when loading from file
            except Exception as e:
                print(f"Error loading existing career data file: {e}")
                print("Step 4: Scraping USports career data...")
                career_df, failed_career = scrape_usports_career_for_players(player_names, usports_stats_url)
                all_failures["Step 4: USports Career"] = failed_career
                if not career_df.empty:
                    save_dataframe_to_excel(career_df, "usports_career_data.xlsx")
                print_failure_summary("Step 4: USports Career", failed_career, len(player_names))
        else:
            print("Step 4: Scraping USports career data...")
            career_df, failed_career = scrape_usports_career_for_players(player_names, usports_stats_url)
            all_failures["Step 4: USports Career"] = failed_career
            if not career_df.empty:
                save_dataframe_to_excel(career_df, "usports_career_data.xlsx")
            print_failure_summary("Step 4: USports Career", failed_career, len(player_names))
    
    # Generate summary content
    summary_lines = []
    summary_lines.append("="*80)
    summary_lines.append("OVERALL SCRAPING SUMMARY")
    summary_lines.append("="*80)
    total_players = len(player_names)
    
    for step, failed_list in all_failures.items():
        success_rate = ((total_players - len(failed_list)) / total_players * 100) if total_players > 0 else 0
        line = f"{step:<30} Failed: {len(failed_list):3d}/{total_players:3d} ({success_rate:5.1f}% success)"
        summary_lines.append(line)
    
    summary_lines.append("\nDETAILED FAILURE BREAKDOWN:")
    for step, failed_list in all_failures.items():
        summary_lines.append(f"\n{step} failures ({len(failed_list)}):")
        if failed_list:
            for player in failed_list:
                summary_lines.append(f"  • {player}")
        else:
            summary_lines.append("  ✓ No failures")
    
    summary_lines.append("\nScraping completed! Use the Jupyter notebook 'player_to_pro_clean.ipynb' to clean the data and merge career information.")
    
    # Print summary to console
    for line in summary_lines:
        print(line)
    
    # Save summary to text file
    try:
        from datetime import datetime
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        summary_filename = f"data/scrape_summary_{timestamp}.txt"
        
        with open(summary_filename, 'w', encoding='utf-8') as f:
            f.write('\n'.join(summary_lines))
        
        print(f"\nSummary saved to: {summary_filename}")
        
    except Exception as e:
        print(f"\nError saving summary to file: {e}")

if __name__ == "__main__":
    main()