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
    if len(parts) > 1:
        return "-".join(parts[1:] + [parts[0]]).lower()
    return full_name.lower()

def scrape_usports_to_pro_info(url): 
    # Step 1: For each player, scrape their pro career summary
    # Send a GET request to fetch the page content
    response = session.get(url)

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
                    print(f"Found player: {text}")
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
    pro_seasons_url = "https://usportshoops.ca/pro/pro-player.php?Gender=MBB&Player="
    for player in player_names:
        res = session.get(f"{pro_seasons_url}{player["url_name"]}")
        soup = BeautifulSoup(res.content, "html5lib")
        table = soup.find("table", {"border": "1"})
        if not table:
            continue

        for row in table.find_all("tr")[1:]:
            cols = row.find_all("td")
            if len(cols) > 1:
                season_data = [player["readable_name"]] + [col.get_text(strip=True) for col in cols]
                all_pro_seasons.append(season_data)

    # Create pro seasons DataFrame
    pro_headers = ["Player", "Season", "Country", "League", "Team"]
    df_pro = pd.DataFrame(all_pro_seasons, columns=pro_headers)
    df_pro.to_excel("data/pro_season_data.xlsx", index=False, engine="openpyxl")
    print("Saved pro season tables")

    # Display the first few rows for verification
    print(df_pro.head())

# USports to Pro Summary Page
usports_gone_pro_summary_url = "https://usportshoops.ca/pro/pro-playerlist.php?Gender=MBB" 
player_names = scrape_usports_to_pro_info(usports_gone_pro_summary_url)
