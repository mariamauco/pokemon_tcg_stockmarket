import requests
from collections import defaultdict
import time
import json

# variables
decks_api = "https://infinite-api.tcgplayer.com/content/articles/search/?contentType=deck&game=pokemon&age=1y&sort=created&order=desc&rows=48&offset={}"
deck_details_api = "https://infinite-api.tcgplayer.com/deck/pokemon/{}/?source=infinite-content&subDecks=true&cards=true&stats=true"
max_decks = 260
rows_per_page = 48

PLACEMENT_POINTS = {
    "1st": 10,
    "2nd": 9,
    "3rd-4th": 8,
    "5th-8th": 7,
    "9th-16th": 6
}
DEFAULT_POINTS = 3


def get_deck_ids():
    deck_ids = []
    for offset in range(0, max_decks, rows_per_page):
        response = requests.get(decks_api.format(offset))
        if response.ok:
            data = response.json()
            for deck in data["result"]:
                deck_ids.append((deck['deckID'], deck['deckData'].get('deckName', ''), deck['deckData'].get('eventRank', '')))
        else:
            print(f"Failed to fetch deck IDs: {response.status_code}")
            break
        time.sleep(0.5)
    return deck_ids

def get_deck_data(deck_id):
    url = deck_details_api.format(deck_id)
    response = requests.get(url)
    return response.json()

def score_from_placement(placement):
    return PLACEMENT_POINTS.get(placement, DEFAULT_POINTS)

def build_card_usage_scores(deck_ids):
    card_counts = defaultdict(lambda: ("", "", "", 0)) # (set, number, supertype, quantity)
    card_scores = defaultdict(lambda: ("", "", "", 0)) # (set, number, supertype, score)

    for deck_id, _, placement in deck_ids:
        #print(f"Processing deck {deck_id} (placement: {placement})")
        try:
            deck_data = get_deck_data(deck_id)
            cards = deck_data['result']['deck']['subDecks']['maindeck']

            score = score_from_placement(placement)

            size = 0
            for card in cards:
                size += card['quantity']
            
            if size < 60:
                print(f"Deck {deck_id} has less than 60 cards ({size} cards), skipping.")
                continue

            for card in cards:
                card_id = card['cardID']
                quantity = card['quantity']

                

                cardinfo = deck_data['result']['cards'][str(card_id)]
                cardID = cardinfo['cardID']

                if cardID not in card_counts:
                    card_scores[cardID] = (cardinfo['set'], cardinfo['cardNumber'], cardinfo['supertype'], score)
                    card_counts[cardID] = (cardinfo['set'], cardinfo['cardNumber'], cardinfo['supertype'], quantity)
                else:
                    card_scores[cardID] = (card_scores[cardID][0], card_scores[cardID][1], card_scores[cardID][2], card_scores[cardID][3] + score)
                    card_counts[cardID] = (card_counts[cardID][0], card_counts[cardID][1], card_counts[cardID][2], card_counts[cardID][3] + quantity)


        except Exception as e:
            print(f"Error processing deck {deck_id}: {e}")
        time.sleep(0.4)

    return card_counts, card_scores


def split_by_supertype(input_dict):
    pokémon = {}
    trainer = {}
    energy = {}

    for card_id, (set_code, number, supertype, value) in input_dict.items():
        if supertype == "Pokémon":
            pokémon[card_id] = (set_code, number, supertype, value)
        elif supertype == "Trainer":
            trainer[card_id] = (set_code, number, supertype, value)
        elif supertype == "Energy":
            energy[card_id] = (set_code, number, supertype, value)

    return pokémon, trainer, energy

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
    "Accept": "application/json",
    "Referer": "https://www.tcgplayer.com/",
}
deck_ids = get_deck_ids()
card_counts, card_scores = build_card_usage_scores(deck_ids)

with open("data/decks/card_counts.json", "w") as f:
    json.dump(card_counts, f, indent=4)
with open("data/decks/card_scores.json", "w") as f:
    json.dump(card_scores, f, indent=4)


counts_pokemon, counts_trainer, counts_energy = split_by_supertype(card_counts)
scores_pokemon, scores_trainer, scores_energy = split_by_supertype(card_scores)

with open("data/decks/counts_pokemon.json", "w") as f:
    json.dump(counts_pokemon, f, indent=4)
with open("data/decks/counts_trainer.json", "w") as f:
    json.dump(counts_trainer, f, indent=4)
with open("data/decks/counts_energy.json", "w") as f: 
    json.dump(counts_energy, f, indent=4)
with open("data/decks/scores_pokemon.json", "w") as f:
    json.dump(scores_pokemon, f, indent=4)
with open("data/decks/scores_trainer.json", "w") as f:
    json.dump(scores_trainer, f, indent=4)
with open("data/decks/scores_energy.json", "w") as f:
    json.dump(scores_energy, f, indent=4)
    