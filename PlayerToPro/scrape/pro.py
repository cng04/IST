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

def scrape_usports_to_pro_info(summary_url, pro_seasons_url, usport_stats_url): 
    # Step 1: For each player, scrape their pro career summary
    # Send a GET request to fetch the page content
    response = session.get(summary_url)

    # Parse the page content
    soup = BeautifulSoup(response.content, "html5lib") # html5lib repairs malformed HTML better than html.parser

    # Find the table
    table = soup.find("table", {"border": "1"})

    # Extract headers
    headers = []
    for th in table.find_all("th"):
        col_name = th.get_text(strip=True)
        headers.append(col_name)
    
    # for scraping players pro seasons
    player_names = []

    # Extract rows
    player_data = []
    for row in table.find_all("tr")[1:]:  # Skipping the header row
        cols = row.find_all("td")
        if len(cols) > 1:
            row_data = []
            for i, col in enumerate(cols):
                text = col.get_text(strip=True)
                row_data.append(text)

                if i == 0:
                    player_names.append({"url_name": format_player_name(text), "readable_name": text})
            
            player_data.append(row_data)

    # Convert to DataFrame with custom headers
    df = pd.DataFrame(player_data, columns=headers)

    # Save DataFrame to an Excel file using openpyxl
    df.to_excel("data/usports_players_to_pro_summary.xlsx", index=False, engine="openpyxl")
    print("Data saved to usports_players_to_pro_summary.xlsx")

    # Display the first few rows for verification
    print(df.head())

    # Step 2: For each player, scrape their pro seasons
    all_pro_seasons = []
    pro_headers = []
    pro_seasons_unable_to_scrape = []
    for player in player_names:
        print(f"Scraping pro seasons for {player['url_name']}")
        res = session.get(f"{pro_seasons_url}{player["url_name"]}")
        soup = BeautifulSoup(res.content, "html5lib")
        table = soup.find("table", {"border": "1"})
        if not table:
            print(f"No pro seasons summary table found for {player['readable_name']}")
            pro_seasons_unable_to_scrape.append(player['readable_name'])
            continue

        if not pro_headers:
            pro_headers = ["Player"] + [th.get_text(strip=True) for th in table.find_all("th")]

        for row in table.find_all("tr")[1:]:
            cols = row.find_all("td")
            if len(cols) > 1:
                season_data = [player["readable_name"]] + [col.get_text(strip=True) for col in cols]
                all_pro_seasons.append(season_data)

    # Create pro seasons DataFrame
    df_pro = pd.DataFrame(all_pro_seasons, columns=pro_headers)
    df_pro.to_excel("data/pro_season_data.xlsx", index=False, engine="openpyxl")
    print("Saved pro season tables")

    # Display the first few rows for verification
    print(df_pro.head())
    print(f"Players whose pro league summaries I couldn't scrape: {pro_seasons_unable_to_scrape}")

    # Step 3: Scrape each player's USports Stats now
    all_usports_stats = []
    all_usports_headers = []
    usports_stats_unable_to_scrape = []
    for player in player_names:
        print(f"Scraping usports stats for {player['url_name']}")
        res = session.get(f"{usport_stats_url}{player['url_name']}")
        soup = BeautifulSoup(res.content, "html5lib")

        # Find the "Overall career Statistics" marker
        marker = soup.find("b", string="Overall career statistics")
        
        if not marker:
            marker = soup.find("b", string="Career regular league season statistics")

        if not marker:
            print(f"No stats marker found for {player['readable_name']}")
            usports_stats_unable_to_scrape.append(player['readable_name'])
            continue

        usports_stats_table = marker.find_next("table", {"border": "1"})
        if not usports_stats_table:
            print(f"No usports stats table found for {player['readable_name']}")
            usports_stats_unable_to_scrape.append(player['readable_name'])
            continue

        print(f"usports_stats_table: {usports_stats_table}")

        if not all_usports_headers:
            expanded_headers = []
            for th in usports_stats_table.find_all("th"):
                text = th.get_text(strip=True)
                colspan = int(th.get("colspan", 1))
                if colspan == 1:
                    expanded_headers.append(text)
                else:
                    for i in range(colspan):
                        expanded_headers.append(f"{text}_{i+1}")
            all_usports_headers = ["Player"] + expanded_headers
            print(f"usports headers: {all_usports_headers}")

        for row in usports_stats_table.find_all("tr")[1:]:
            cols = row.find_all("td")
            if len(cols) > 1:
                stats_data = [player["readable_name"]] + [col.get_text(strip=True) for col in cols]
                all_usports_stats.append(stats_data)

    # Create usports stats DataFrame
    df_usports_stats = pd.DataFrame(all_usports_stats, columns=all_usports_headers)
    df_usports_stats.to_excel("data/usports_stats_data.xlsx", index=False, engine="openpyxl")
    print("Saved usports stats tables")

    # Display the first few rows for verification
    print(df_usports_stats.head())

    # Display player whose table I was unable to scrape 
    print(f"Players whose USPORTS stats I couldn't scrape: {usports_stats_unable_to_scrape}")

# Scraping
summary_url = "https://usportshoops.ca/pro/pro-playerlist.php?Gender=MBB" 
pro_seasons_url = "https://usportshoops.ca/pro/pro-player.php?Gender=MBB&Player="
usports_stats_url = "https://usportshoops.ca/history/person.php?Gender=MBB&Person="

### Some urls of names with more than two parts are constructed differently, right now will add them manually, but should automate this later
# unique_names = ["Sami Al Uariachi", "Adil El Makssoud", "Kevin Leander Fiabema", "Atanas (Nasko) Golomeev", "Daniel Gonzalez Longarela", "Marc Antoine Horth", "Misi Boye Jeanneau-Mubiala", "Michael Kelvin II", "Franck Olivier Kouagnia", "Islam Luiz de Toledo", "David John Oates", "Jean Emmanuel Pierre-Charles", "Ismar Serferagic Kriese", "Henry Van Herk", "Bas van Willigen", "Samuel Willis III"]
scrape_usports_to_pro_info(summary_url, pro_seasons_url, usports_stats_url)


