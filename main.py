import os
import pandas as pd
import json
from dotenv import load_dotenv
from pokemon_api import fetch_pokemon_cards  
from pokemon_api import fetch_single_pokemon_card
from pokemontcgsdk import Set

# Load Pokemon TCG Developer API Key
load_dotenv()
api_key = os.getenv("TCG_API_KEY")

if not api_key:
    raise ValueError("TCG_API_KEY is missing! Make sure to set it in your .env file.")

# Finds cards for every major set
query = ["set.id:base1*", "set.id:gym1*", "set.id:bw1*", "set.id:xy1*", "set.id:sm1*", "set.id:swsh2*", "set.id:sv1*", "set.id:sv9*", ]


# Function to fetch cards data and save it in the folder 'data/'
def fetch_and_save_cards(queries):
    dfs = [fetch_pokemon_cards(api_key, query) for query in queries]
    combined_df = pd.concat(dfs, ignore_index=True)
    return combined_df


def add_leftover_cards(cards_df, counts, scores, api_key):
    cards_df = cards_df.copy()

    cards_df['count'] = 0
    cards_df['score'] = 0

    new_cards = []

    for card_id in counts.keys():
        count_entry = counts.get(card_id)
        score_entry = scores.get(card_id)

        count = count_entry[3] if count_entry else 0
        score = score_entry[3] if score_entry else 0

        # update if card already exists in the DataFrame
        match = cards_df['id'].str.strip() == card_id.strip()
        if match.any():
            cards_df.loc[match, 'count'] = count
            cards_df.loc[match, 'score'] = score
        else:
            # fetch card details from the API
            new_card_df = fetch_single_pokemon_card(api_key, card_id)

            if not new_card_df.empty:
                new_card_df['count'] = count
                new_card_df['score'] = score
                new_cards.append(new_card_df)
    
    if new_cards:
        new_data = pd.concat(new_cards, ignore_index=True)
        cards_df = pd.concat([cards_df, new_data], ignore_index=True)

    return cards_df
            
def convert__to_csv(cards_df, filename):
    cards_df.to_csv(f"data/{filename}", encoding='utf-8', index=False)
    print(f"Saved {filename} with {len(cards_df)} cards.")


def dropNoValueCards(cards_df):
    # Step 1: Replace NaNs with 0 to avoid comparison issues
    price_cols = [
        'tcg market price usd (normal)',
        'tcg market price usd (reverse holofoil)',
        'tcg market price usd (holofoil)',
        'tcg low price usd (normal)',
        'tcg low price usd (reverse holofoil)',
        'tcg low price usd (holofoil)',
        'tcg high price usd (normal)',
        'tcg high price usd (reverse holofoil)',
        'tcg high price usd (holofoil)',
    ]

    cards_df[price_cols] = cards_df[price_cols].apply(pd.to_numeric, errors='coerce')

    # Step 2: Drop rows where all TCG prices are zero
    cards_df = cards_df[~(cards_df[price_cols] < 0.03).all(axis=1)]

    return cards_df

with open("data/decks/card_counts.json", "r") as f:
    counts = json.load(f)
with open("data/decks/card_scores.json", "r") as f:
    scores = json.load(f)

sets = fetch_and_save_cards(query) # Fetch cards from the API
updated = add_leftover_cards(sets, counts, scores, api_key) # Add leftover cards to the DataFrame
updated2 = dropNoValueCards(updated) # Drop cards with no value
convert__to_csv(updated2, "pokemonCards.csv") # Save the DataFrame to a CSV file to use for R analysis