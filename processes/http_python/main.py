import requests
import time
from multiprocessing import Pool


def make_request(url: str) -> str:
    try:
        res = requests.get(url)
        res.raise_for_status()
        return f"Ok, {url}"
    except requests.exceptions.RequestException as e:
        return f"Error: {e}"


websites = [
    "https://envato.com",
    "http://amazon.com",
    "http://facebook.com",
    "http://google.com",
    "http://google.fr",
    "http://google.es",
    "http://internet.org",
    "http://gmail.com",
    "http://stackoverflow.com",
    "http://github.com",
    "http://heroku.com",
    "http://really-cool-available-domain.com",
    "http://djangoproject.com",
    "http://rubyonrails.org",
    "http://basecamp.com",
    "http://trello.com",
    "http://yiiframework.com",
    "http://shopify.com",
    "http://another-really-interesting-domain.co",
    "http://airbnb.com",
    "http://instagram.com",
    "http://snapchat.com",
    "http://youtube.com",
    "http://baidu.com",
    "http://yahoo.com",
    "http://live.com",
    "http://linkedin.com",
    "http://yandex.ru",
    "http://netflix.com",
    "http://wordpress.com",
    "http://bing.com",
]


def serial_all_requests() -> None:
    for site in websites:
        res = make_request(site)
        print(res)


def async_all_requests() -> None:
    CHUNK_SIZE = 5
    p = Pool()
    results = p.map_async(make_request, websites, chunksize=CHUNK_SIZE)

    for res in results.get():
        print(res)


if __name__ == "__main__":
    time_start = time.perf_counter()
    serial_all_requests()
    time_end = time.perf_counter()
    print(f"# TOOK: {time_end - time_start}")

    print("\n---\n")

    time_start = time.perf_counter()
    async_all_requests()
    time_end = time.perf_counter()
    print(f"# TOOK: {time_end - time_start}")
