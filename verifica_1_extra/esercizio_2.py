from menu import Menu
from statistics import mean

# **Esercizio 2:**
# Immagina di gestire i risultati di un torneo sportivo attraverso una
# tupla contenente le informazioni sulle partite. Ogni voce della tupla
# rappresenta una partita e contiene le seguenti informazioni: squadra di
# casa, squadra ospite, punteggio della squadra di casa, punteggio
# della squadra ospite.

tupla_partite = (
    ("SquadraA", "SquadraB", 3, 2),
    ("SquadraC", "SquadraD", 1, 1),
    ("SquadraB", "SquadraC", 2, 4),
    ("SquadraD", "SquadraA", 0, 3),
    ("SquadraB", "SquadraD", 1, 2),
)

# Definisci le seguenti funzioni utilizzando le funzioni predefinite sum,
# min, max per le liste:
# 1. **media_gol_partite(tupla_partite)**: una funzione che accetti come
#    parametro la tupla delle partite e restituisca la media dei gol
#    segnati in tutte le partite.
# 2. **media_gol_squadra(tupla_partite, squadra)**: una funzione che accetti
#    come parametri la tupla delle partite e il nome di una squadra, e
#    restituisca la media dei gol segnati dalla squadra in tutte le partite
#    in cui ha partecipato.
# 3. **partita_con_piu_gol(tupla_partite)**: una funzione che restituisca
#    una tupla contenente la partita con il maggior numero di gol segnati
#    e i relativi punteggi.
# 4. **partita_con_meno_gol(tupla_partite)**: una funzione che restituisca
#    una tupla contenente la partita con il minor numero di gol segnati
#    e i relativi punteggi.
# 5. **percentuale_vittorie_squadra(tupla_partite, squadra)**: una funzione
#    che restituisca la percentuale di partite vinte dalla squadra rispetto
#    al totale delle partite disputate.
# 6. Prevedi un menu di scelta che consenta all'utente di selezionare
#    un'operazione tra le opzioni disponibili.


def media_gol_partite() -> float:
    gol = [c + o for (_, _, c, o) in tupla_partite]
    return mean(gol)


def menu_media_gol_partite():
    media = media_gol_partite()
    print(f"La media dei gol di ogni partita è {media:.2f}.")


def media_gol_squadra(squadra: str) -> float:
    gol: list[int] = []
    for casa, ospite, punti_casa, punti_ospite in tupla_partite:
        if casa == squadra:
            gol.append(punti_casa)
        if ospite == squadra:
            gol.append(punti_ospite)

    if not gol:
        gol.append(0)
    return mean(gol)


def menu_media_gol_squadra():
    squadra = input("Inserisci una squadra: ")
    media = media_gol_squadra(squadra)
    print(f"La media dei gol della squadra {squadra} è {media:.2f}.")


def partita_con_piu_gol() -> tuple[str, tuple[int, int]]:
    partite = [
        (
            f"{c} - {o}",
            (pc, po),
            pc + po,
        )
        for (c, o, pc, po) in tupla_partite
    ]

    nome_partita, punteggio, piu_gol = "", (0, 0), -1
    for partita, punteggio_partita, gol in partite:
        if piu_gol < gol:
            nome_partita, piu_gol = partita, gol
            punteggio = punteggio_partita

    return (nome_partita, punteggio)


def menu_partita_con_piu_gol():
    partita, (c, o) = partita_con_piu_gol()
    print(f"la partita con più gol è stata {partita} ({c} - {o})")


def partita_con_meno_gol() -> tuple[str, tuple[int, int]]:
    partite = [
        (
            f"{c} - {o}",
            (pc, po),
            pc + po,
        )
        for (c, o, pc, po) in tupla_partite
    ]

    nome_partita, punteggio, meno_gol = "", (0, 0), 999_999_999
    for partita, punteggio_partita, gol in partite:
        if meno_gol > gol:
            nome_partita, meno_gol = partita, gol
            punteggio = punteggio_partita

    return (nome_partita, punteggio)


def menu_partita_con_meno_gol():
    partita, (c, o) = partita_con_meno_gol()
    print(f"la partita con meno gol è stata {partita} ({c} - {o})")


def percentuale_vittorie_squadra(squadra: str) -> float:
    squadra = squadra.lower()
    wins, other = 0.0, 0.0
    for c, o, pc, po in tupla_partite:
        if c.lower() == squadra:
            if pc > po:
                wins += 1
            else:
                other += 1
        if o.lower() == squadra:
            if pc < po:
                wins += 1
            else:
                other += 1
    sum = wins + other
    return wins / sum * 100


def menu_percentuale_vittorie_squadra():
    squadra = input("Inserisci una squadra: ")
    percentuale = percentuale_vittorie_squadra(squadra)
    print(f"La percentuale è del {percentuale:.2f}%.")


menu = Menu({
    "Media dei gol totale": menu_media_gol_partite,
    "Media gol di una squadra": menu_media_gol_squadra,
    "Partita con più gol": menu_partita_con_piu_gol,
    "Partita con meno gol": menu_partita_con_meno_gol,
    "Vittorie di una squadra": menu_percentuale_vittorie_squadra,
})

menu.run()
