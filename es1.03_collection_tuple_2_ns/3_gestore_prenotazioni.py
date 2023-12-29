from dataclasses import dataclass


# Esempio di utilizzo delle funzioni
@dataclass
class Prenotazione:
    nome: str
    data: str
    persone: int
    id_tavolo: int


def aggiungi_prenotazione(
    lista_prenotazioni: list[Prenotazione],
    nome: str,
    data: str,
    persone: int,
    id_tavolo: int,
) -> None:
    lista_prenotazioni.append(Prenotazione(nome, data, persone, id_tavolo))


def rimuovi_prenotazione(
    lista_prenotazioni: list[Prenotazione],
    nome: str,
    data: str,
) -> None:
    index = -1

    for i, prenotazione in enumerate(lista_prenotazioni):
        if prenotazione.nome == nome and prenotazione.data == data:
            index = i

    if index == -1:
        return
    lista_prenotazioni.pop(index)


def tavoli_disponibili(
    lista_prenotazioni: list[Prenotazione],
    data: str,
):
    prenotazioni = [p for p in lista_prenotazioni if p.data == data]
    tavoli = [p.id_tavolo for p in prenotazioni]
    return [i for i in range(1, 11) if i not in tavoli]


def prenotazioni_cliente(
    lista_prenotazioni: list[Prenotazione],
    nome: str,
) -> list[tuple[str, str, int, int]]:
    prenotazioni = [p for p in lista_prenotazioni if p.nome == nome]
    return [(p.nome, p.data, p.persone, p.id_tavolo) for p in prenotazioni]


def conto_totale(
    lista_prenotazioni: list[Prenotazione],
    data: str,
):
    # Algoritmo fittizio per calcolare il prezzo, poiché nessuno
    # è stato proposto.
    return 50 * len([p for p in prenotazioni if p.data == data])


prenotazioni: list[Prenotazione] = []

aggiungi_prenotazione(prenotazioni, "Maria", "04-10-2023", 4, 3)
aggiungi_prenotazione(prenotazioni, "Pietro", "04-10-2023", 2, 2)
aggiungi_prenotazione(prenotazioni, "Carlo", "04-10-2023", 5, 5)

output = f"""Tavoli disponibili per il 04-10-2023: {
        tavoli_disponibili(prenotazioni,'04-10-2023')}
Prenotazioni di Maria: {prenotazioni_cliente(prenotazioni, "Maria")}
Conto totale per il 04-10-2023: {conto_totale(prenotazioni, "04-10-2023")}"""

print(output)


# Test
def test_output_corretto() -> None:
    # Onestamente non so come calcolare il conto
    expected = """Tavoli disponibili per il 04-10-2023: [1, 4, 6, 7, 8, 9, 10]
Prenotazioni di Maria: [('Maria', '04-10-2023', 4, 3)]
Conto totale per il 04-10-2023: 220"""
    assert output == expected
