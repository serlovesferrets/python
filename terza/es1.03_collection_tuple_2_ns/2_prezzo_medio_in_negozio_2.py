from statistics import mean

# Versione 2
products = (
    ("Mela", ("Lunedì", 1.0), ("Martedì", 1.2), ("Mercoledì", 1.1)),
    ("Banana", ("Lunedì", 0.8), ("Martedì", 0.7), ("Mercoledì", 0.8)),
)


def mean_price(products, name: str) -> float:
    product = [product for product in products if product[0] == name][1:]
    prices = [price for (_, price) in product]
    return mean(prices)


def total_mean(products) -> float:
    all_prices = []
    product_prices = [product[1:] for product in products]
    for days in product_prices:
        for _, price in days:
            all_prices.append(price)
    return mean(all_prices)


def base_product_with_days(products, name, finder):
    product = [product for product in products if product[0] == name][0][1:]
    max_price = finder(*[price for (_, price) in product])
    days = [day for (day, price) in product if price == max_price]
    return (max_price, days)


def max_product_with_days(products, name):
    return base_product_with_days(products, name, max)


def min_product_with_days(products, name):
    return base_product_with_days(products, name, min)


# Test
def test_media_totale_corretta() -> None:
    assert round(total_mean(products), 2) == 0.93


def test_max_product_with_days_funziona() -> None:
    assert max_product_with_days(
        products,
        "Banana",
    ) == (0.8, ["Lunedì", "Mercoledì"])


def test_min_product_with_days_funziona() -> None:
    assert min_product_with_days(
        products,
        "Mela",
    ) == (1.0, ["Lunedì"])
