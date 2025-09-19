import requests
from bs4 import BeautifulSoup
import pandas as pd
import time

# Start a session to maintain cookies
session = requests.Session()

def scrape_player_data(player_url):
    """Scrape individual player data from their profile page"""
    try:
        response = session.get(player_url)
        soup = BeautifulSoup(response.content, 'html5lib')

        player_data = {
            'playing_career': [],
            'career_stats': []
        }

        # Look for "Playing career:" section
        playing_career_marker = soup.find("b", string="Playing career:")
        if playing_career_marker:
            career_table = playing_career_marker.find_next("table", {"border": "1"})
            if career_table:
                headers = [th.get_text(strip=True) for th in career_table.find("tr").find_all(["th", "td"])]

                for row in career_table.find_all("tr")[1:]:
                    cols = row.find_all("td")
                    if len(cols) >= 3:
                        row_data = {}
                        for i, col in enumerate(cols):
                            if i < len(headers):
                                row_data[headers[i]] = col.get_text(strip=True)

                        # Extract season, team, elig columns specifically
                        career_entry = {
                            'season': row_data.get('Season', ''),
                            'team': row_data.get('Team', ''),
                            'elig': row_data.get('Elig', ''),
                            'full_data': row_data
                        }
                        player_data['playing_career'].append(career_entry)

        # Look for career statistics tables
        # First try "Overall career statistics"
        stats_marker = soup.find("b", string="Overall career statistics")
        if not stats_marker:
            # If not found, try "Career regular league season statistics"
            stats_marker = soup.find("b", string="Career regular league season statistics")

        if stats_marker:
            stats_table = stats_marker.find_next("table", {"border": "1"})
            if stats_table:
                # Handle headers with colspan properly
                header_row = stats_table.find("tr")
                headers = []
                if header_row:
                    for th in header_row.find_all(["th", "td"]):
                        header_text = th.get_text(strip=True)
                        colspan = int(th.get('colspan', 1))
                        if header_text == "GP":
                            header_text = "GP-GS"  # Rename GP to GP-GS for consistency
                        # Add the header text for each column it spans
                        for i in range(colspan):
                            headers.append(f"{header_text}_{i}")

                print(f"Career stats headers with colspan handled: {headers}")

                for row in stats_table.find_all("tr")[1:]:
                    cols = row.find_all("td")
                    if cols:
                        row_data = {}
                        for i, col in enumerate(cols):
                            if i < len(headers):
                                row_data[headers[i]] = col.get_text(strip=True)

                        # Only add if row has meaningful data
                        if any(value.strip() for value in row_data.values() if value):
                            player_data['career_stats'].append(row_data)

        return player_data

    except Exception as e:
        print(f"Error scraping player data from {player_url}: {e}")
        return None

def main():
    scrape_url = "https://usportshoops.ca/history/teamseason.php?Gender=MBB&Season=2024-25&Team=Waterloo&Reflink="
    all_player_data = []

    try:
        response = session.get(scrape_url)
        soup = BeautifulSoup(response.content, 'html5lib')

        # Look for Player roster
        marker = soup.find("b", string="Player roster")

        if not marker:
            print("Could not find 'Player roster' section.")
            return

        table = marker.find_next("table", {"border": "1"})

        if not table:
            print("Could not find the player roster table.")
            return

        # Extract player URLs from the roster table
        player_urls = []
        for row in table.find_all("tr")[1:]:
            cols = row.find_all("td")
            if cols:
                # Look for player name link (usually in second column)
                player_link = cols[1].find("a")
                if player_link and player_link.get('href'):
                    player_name = player_link.get_text(strip=True)
                    player_urls.append((player_name, f"https://usportshoops.ca{player_link.get('href')}"))

        print(f"Found {len(player_urls)} players to scrape")

        # Scrape each player's data
        for i, (player_name, player_url) in enumerate(player_urls):
            print(f"Scraping {i+1}/{len(player_urls)}: {player_name}")
            player_data = scrape_player_data(player_url)

            if player_data:
                player_data['name'] = player_name
                all_player_data.append(player_data)

            # Add delay to be respectful to the server
            time.sleep(1)

        # Save data to files
        # Convert to DataFrame format for easier analysis
        flattened_data = []

        for player in all_player_data:
            base_info = {
                'name': player['name'],
            }

            if player['career_stats']:
                # For single season players, we need to show both the season row and total row
                if len(player['career_stats']) == 1:
                    # Single season: show the season row using the only stats row
                    if player['playing_career']:
                        career = player['playing_career'][0]  # Should only be one season
                        row = base_info.copy()
                        row.update({
                            'career_season': career['season'],
                            'career_team': career['team'],
                            'career_elig': career['elig']
                        })

                        # Add career stats for this season
                        for stat_name, stat_value in player['career_stats'][0].items():
                            row[f'stats_{stat_name}'] = stat_value

                        flattened_data.append(row)
                else:
                    # Multiple seasons: process regular career_stats rows (all except the last one which is total)
                    regular_stats = player['career_stats'][:-1]

                    # Merge regular stats with playing career data in order
                    if player['playing_career'] and regular_stats:
                        for i, career in enumerate(player['playing_career']):
                            if i < len(regular_stats):  # Make sure we have corresponding stats
                                row = base_info.copy()
                                row.update({
                                    'career_season': career['season'],
                                    'career_team': career['team'],
                                    'career_elig': career['elig']
                                })

                                # Add career stats for this season
                                for stat_name, stat_value in regular_stats[i].items():
                                    row[f'stats_{stat_name}'] = stat_value

                                flattened_data.append(row)

                # Handle total row (last element) with special logic
                if len(player['career_stats']) > 0:
                    total_row = player['career_stats'][-1]  # Get the last row which is the total

                    # Create cumulative season range from playing career
                    season_range = ""
                    usports_team = ""

                    if player['playing_career']:
                        seasons = [career['season'] for career in player['playing_career'] if career['season']]
                        if seasons:
                            # Extract years and create range
                            years = []
                            for season in seasons:
                                # Handle formats like "2023-24" or "2023"
                                year_parts = season.split('-')
                                if year_parts:
                                    years.append(int(year_parts[0]))

                            if years:
                                # For single season, show just the year. For multiple, show range
                                if len(player['career_stats']) == 1:
                                    season_range = f"{min(years)}-{min(years) + 1}"  # Single season format
                                else:
                                    season_range = f"{min(years)}-{max(years) + 1}" if len(years) > 1 else years[0]

                        # Get USports team (assume first team entry)
                        usports_team = player['playing_career'][0]['team'] if player['playing_career'] else ""

                    # Create total row with player name, cumulative season range, blank elig, and total stats
                    total_row_data = {
                        'name': player['name'],
                        'career_season': season_range,
                        'career_team': usports_team,
                        'career_elig': ""  # Keep blank as requested
                    }

                    # Add all stats from total row
                    for stat_name, stat_value in total_row.items():
                        total_row_data[f'stats_{stat_name}'] = "Total" if stat_name == "Season_0" else stat_value 

                    flattened_data.append(total_row_data)

            elif player['playing_career']:
                # If no career stats but has playing career, add just the career data
                for career in player['playing_career']:
                    row = base_info.copy()
                    row.update({
                        'career_season': career['season'],
                        'career_team': career['team'],
                        'career_elig': career['elig']
                    })
                    flattened_data.append(row)

        # Save to Excel
        if flattened_data:
            df = pd.DataFrame(flattened_data)
            df.to_excel('data/current_usports_players.xlsx', index=False)
            print(f"Saved data for {len(all_player_data)} players to data/current_usports_players.xlsx")
            print(f"Total rows: {len(flattened_data)}")
        else:
            print("No player data was successfully scraped")

    except Exception as e:
        print(f"Error fetching or parsing the page: {e}")
        return

if __name__ == "__main__":
    main()