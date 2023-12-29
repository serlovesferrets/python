from statistics import mean

# Versione 1
products = (
    ("Mela", "Lunedì", 1.0),
    ("Mela", "Martedì", 1.2),
    ("Mela", "Mercoledì", 1.1),
    ("Banana", "Lunedì", 0.8),
    ("Banana", "Lunedì", 0.9),
    ("Banana", "Lunedì", 0.7),
)


def mean_price(products, name: str, weekday: str) -> float:
    prices = [
        product[2]
        for product in products
        if product[0] == name and product[1] == weekday
    ]
    return mean(prices)


# Test
def test_prezzo_medio_corretto() -> None:
    assert mean_price(products, "Banana", "Lunedì") == 0.8
    assert mean_price(products, "Mela", "Lunedì") == 1.0
