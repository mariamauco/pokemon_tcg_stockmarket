import requests
import pandas as pd

def fetch_pokemon_cards(api_key: str, search_query: str):
    """
    Fetches Pokémon card data based on the given search query with pagination and formats it into a DataFrame.
    
    Parameters:
    - api_key (str): The API key for authentication.
    - search_query (str): The search query to filter cards.
    
    Returns:
    - pd.DataFrame: A DataFrame containing all card details.
    """
    page_num = 1
    page_size = 250  # Max allowed page size
    all_cards = []
    url = 'https://api.pokemontcg.io/v2/cards'
    headers = {'X-Api-Key': api_key}

    while True:
        params = {'q': search_query, 'page': page_num, 'pageSize': page_size}
        response = requests.get(url, headers=headers, params=params)

        # If error return empty dataframe
        if response.status_code != 200:
            print(f"Error: Received status code {response.status_code}")
            return pd.DataFrame() 

        data = response.json()
        
        if 'data' not in data or not isinstance(data['data'], list):
            print("Error: Unexpected response format")
            return pd.DataFrame()

        for card in data["data"]:
            all_cards.append({
                "id": card["id"],
                "name": card["name"],
                "pokedex number": card.get("nationalPokedexNumbers", [None])[0],
                "supertype": card.get("supertype", ""),
                "subtypes": ", ".join(card.get("subtypes", [])),
                "hp": card.get("hp", ""),
                "types": ", ".join(card.get("types", [])),
                "attacks": ", ".join([f"{atk['name']} ({atk.get('damage', '0')})" for atk in card.get("attacks", [])]),
                "weaknesses": ", ".join([f"{w['type']} {w['value']}" for w in card.get("weaknesses", [])]),
                "retreat cost": len(card.get("retreatCost", [])),
                "set name": card["set"]["name"],
                "release date": card["set"].get("releaseDate", ""),
                "artist": card.get("artist", ""),
                "rarity": card.get("rarity", ""),
                "card image (small)": card["images"]["small"],
                "card image hires": card["images"]["large"],
                "tcg player url": card.get("tcgplayer", {}).get("url", ""),
                "tcg price date": card.get("tcgplayer", {}).get("updatedAt", ""),
                "tcg market price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("market", ""),
                "tcg low price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("low", ""),
                "tcg high price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("high", ""),
                "tcg market price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("market", ""),
                "tcg low price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("low", ""),
                "tcg high price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("high", ""),
                "tcg market price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("market", ""),
                "tcg low price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("low", ""),
                "tcg high price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("high", ""),
            })

        total_cards = data.get('totalCount', len(all_cards))
        
        if len(all_cards) >= total_cards:
            break  # Stop when all cards are fetched

        page_num += 1  # Move to the next page

    return pd.DataFrame(all_cards)

def fetch_pokemon_card(api_key: str, card_id: str) -> pd.DataFrame:
    """
    Fetches details of a Pokémon TCG card and returns the data as a DataFrame.
    
    Parameters:
    card_id (str): The ID of the Pokémon card.
    api_key (str): The API key for authentication.
    
    Returns:
    pd.DataFrame: A DataFrame containing the card details.
    """
    url = f'https://api.pokemontcg.io/v2/cards/{card_id}'
    headers = {'X-Api-Key': api_key}
    response = requests.get(url, headers=headers)
    #print(response)
    all_cards = []
    
    # If error return empty dataframe
    if response.status_code != 200:
        print(f"Error: Received status code {response.status_code}")
        return pd.DataFrame() 

    data = response.json()
    
    if 'data' not in data or not isinstance(data['data'], list):
        print("Error: Unexpected response format")
        return pd.DataFrame()

    for card in data["data"]:
        all_cards.append({
            "id": card["id"],
            "name": card["name"],
            "pokedex number": card.get("nationalPokedexNumbers", [None])[0],
            "supertype": card.get("supertype", ""),
            "subtypes": ", ".join(card.get("subtypes", [])),
            "hp": card.get("hp", ""),
            "types": ", ".join(card.get("types", [])),
            "attacks": ", ".join([f"{atk['name']} ({atk.get('damage', '0')})" for atk in card.get("attacks", [])]),
            "weaknesses": ", ".join([f"{w['type']} {w['value']}" for w in card.get("weaknesses", [])]),
            "retreat cost": len(card.get("retreatCost", [])),
            "set name": card["set"]["name"],
            "release date": card["set"].get("releaseDate", ""),
            "artist": card.get("artist", ""),
            "rarity": card.get("rarity", ""),
            "card image (small)": card["images"]["small"],
            "card image hires": card["images"]["large"],
            "tcg player url": card.get("tcgplayer", {}).get("url", ""),
            "tcg price date": card.get("tcgplayer", {}).get("updatedAt", ""),
            "tcg market price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("market", ""),
            "tcg low price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("low", ""),
            "tcg high price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("high", ""),
            "tcg market price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("market", ""),
            "tcg low price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("low", ""),
            "tcg high price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("high", ""),
            "tcg market price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("market", ""),
            "tcg low price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("low", ""),
            "tcg high price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("high", ""),
        })

    return pd.DataFrame(all_cards)

def fetch_single_pokemon_card(api_key: str, card_id: str) -> pd.DataFrame:
    """
    Fetches details of a Pokémon TCG card and returns the data as a DataFrame.
    
    Parameters:
    card_id (str): The ID of the Pokémon card.
    api_key (str): The API key for authentication.
    
    Returns:
    pd.DataFrame: A DataFrame containing the card details.
    """
    url = f'https://api.pokemontcg.io/v2/cards/{card_id}'
    headers = {'X-Api-Key': api_key}
    response = requests.get(url, headers=headers)
    #print(response)
    all_cards = []
    
    # If error return empty dataframe
    if response.status_code != 200:
        print(f"Error: Received status code {response.status_code}")
        return pd.DataFrame() 

    data = response.json()

    card = data["data"]

    all_cards.append({
        "id": card["id"],
        "name": card["name"],
        "pokedex number": card.get("nationalPokedexNumbers", [None])[0],
        "supertype": card.get("supertype", ""),
        "subtypes": ", ".join(card.get("subtypes", [])),
        "hp": card.get("hp", ""),
        "types": ", ".join(card.get("types", [])),
        "attacks": ", ".join([f"{atk['name']} ({atk.get('damage', '0')})" for atk in card.get("attacks", [])]),
        "weaknesses": ", ".join([f"{w['type']} {w['value']}" for w in card.get("weaknesses", [])]),
        "retreat cost": len(card.get("retreatCost", [])),
        "set name": card["set"]["name"],
        "release date": card["set"].get("releaseDate", ""),
        "artist": card.get("artist", ""),
        "rarity": card.get("rarity", ""),
        "card image (small)": card["images"]["small"],
        "card image hires": card["images"]["large"],
        "tcg player url": card.get("tcgplayer", {}).get("url", ""),
        "tcg price date": card.get("tcgplayer", {}).get("updatedAt", ""),
        "tcg market price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("market", ""),
        "tcg low price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("low", ""),
        "tcg high price usd (normal)": card.get("tcgplayer", {}).get("prices", {}).get("normal", {}).get("high", ""),
        "tcg market price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("market", ""),
        "tcg low price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("low", ""),
        "tcg high price usd (reverse holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("reverseHolofoil", {}).get("high", ""),
        "tcg market price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("market", ""),
        "tcg low price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("low", ""),
        "tcg high price usd (holofoil)": card.get("tcgplayer", {}).get("prices", {}).get("holofoil", {}).get("high", ""),
    })

    return pd.DataFrame(all_cards)
