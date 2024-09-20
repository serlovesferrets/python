from typing import Callable
from menu import Menu
from dataclasses import dataclass


def retry_until(provider, predicate, err):
    result = provider()
    if not predicate(result):
        print(err)
        return retry_until(provider, predicate, err)
    else:
        return result


def could_cast(caster):
    def result(subj):
        try:
            caster(subj)
            return True
        except:
            return False

    return result


def safe_int(prompt: str, err="Non un numero intero! Riprova.") -> int:
    pred = could_cast(lambda maybe_int: int(maybe_int))
    return retry_until(lambda: input(prompt), pred, err)


def safe_float(prompt: str, err="Non un numero! Riprova.") -> float:
    pred = could_cast(lambda maybe_float: float(maybe_float))
    return retry_until(lambda: input(prompt), pred, err)


@dataclass
class Apartment:
    address: str
    rooms: int
    price: float
    nights: int

    def total(self) -> float:
        return float(self.price) * float(self.nights)

    def __repr__(self) -> str:
        return f"""# {self.address}
Posti: {self.rooms}
Prezzo: {self.price}
Notti: {self.nights}"""


apartments: dict[int, Apartment] = {}

safe_code: Callable[[], int] = lambda: retry_until(
    lambda: safe_int("Inserisci il codice: "),
    lambda c: int(c) not in apartments,
    "Questo codice è già stato usato!",
)


def apartment_from_io() -> tuple[int, Apartment]:
    code = safe_code()
    address = input("Inserisci l'indirizzo: ")
    rooms = safe_int("Inserisci i posti: ")
    price = safe_float("Inserisci il prezzo: ")
    nights = safe_int("Inserisci il numero di notti: ")

    apartment = Apartment(address, rooms, price, nights)
    return (code, apartment)


def add_apartment(
    code: int,
    apartment: Apartment,
    *,
    okay="Appartamento aggiunto!",
    err="Il codice è già stato usato!",
) -> str:
    if code not in apartments:
        apartments[code] = apartment
        return okay
    else:
        return err


def populate() -> None:
    base_apartments = {
        1: Apartment("Via Rossi 23", 3, 55, 2),
        2: Apartment("Via Verdi 32", 4, 65, 4),
        3: Apartment("Via Bianchi 19", 5, 75, 2),
    }

    for k, v in base_apartments.items():
        print(add_apartment(k, v, err=f"Codice {k} già usato!"))


def delete(code: int) -> str:
    if code not in apartments:
        return "Codice non esistente!"
    else:
        apartments.pop(code)
        return "Appartamento rimosso!"


def delete_menu() -> None:
    print(delete(safe_int("Inserisci il codice: ")))


def show_apartment_menu() -> None:
    code = safe_int("Inserisci il codice: ")
    if code not in apartments:
        print("Codice non esistente!")
    else:
        print(apartments[code])


def add_apartment_menu() -> None:
    code, apartment = apartment_from_io()
    apartments[code] = apartment


def total_and_max():
    revenues = [a.total() for a in apartments.values()]
    return (sum(revenues), max(revenues))


def total_and_max_menu() -> None:
    total, max = total_and_max()
    print(f"Guadagno totale: {total:.2f}")
    print(f"Guadagno massimo: {max:.2f}")


def edit_apartment_menu() -> None:
    code = safe_code()
    apartment = apartments[code]
    address = input(f"Indirizzo: {apartment.address} -> ? ")
    rooms = safe_int(f"Posti: {apartment.rooms} -> ? ")
    price = safe_float(f"Prezzo: {apartment.price} -> ? ")
    nights = safe_int(f"Notti: {apartment.nights} -> ? ")

    apartment = Apartment(address or apartment.address, rooms, price, nights)
    apartments[code] = apartment


if __name__ == "__main__":
    Menu(
        {
            "Popola": populate,
            "Aggiungi un appartamento": add_apartment_menu,
            "Elimina appartamenti con un certo codice": delete_menu,
            "Stampa appartamento": show_apartment_menu,
            "Stampa tutti gli appartamenti": lambda: print(apartments),
            "Modifica un appartamento": edit_apartment_menu,
            "Guadagno totale e massimo": total_and_max_menu,
        }
    ).run()
