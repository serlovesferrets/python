from statistics import mean

VALORI_NON_VALIDI = 234890607894

"""
Esercizio n.2 -  Rilevazioni pluviometriche

Nella tupla_pluviometrica sono memorizzate le rilevazioni pluviometriche di
alcune città lombarde. Per ogni rilevazione si riporta:

    - città;
    - provincia;
    - anno;
    - mese;
    - valore (quantitativo di pioggia in mm).

Utilizzare le funzioni predefinite sum, min, max per le liste.

Richieste:

1. Definire una funzione chiamata **media_globale** che accetti come
   parametro la tupla delle rilevazioni pluviometriche e restituisca il
   quantitativo medio di pioggia rilevata nell'anno 2023.

2. Definire una funzione chiamata **media** che accetti come parametri la
   tupla delle rilevazioni pluviometriche, la provincia e il mese di
   riferimento e restituisca il quantitativo medio di pioggia rilevata.

3. Definire una funzione chiamata **pioggiaMax** che restituisca una tupla
   contenente  la città  e il mese (o i mesi ) più piovoso/i della provincia
   di Milano.

4. Definire una funzione chiamata **pioggiaMin** che restituisca una tupla
   contenente il mese con minor precipitazioni.

5. Definire una funzione chiamata **provinciaPer** che restituisca una tupla
   contenente la percentuale delle precipitazioni per provincia
   rispetto al totale.

6. Prevedere un menu di scelta.
"""

tupla_pluviometrica = (
    (("Vittuone", "Milano"), (2022, ("gennaio", 20))),
    (("Vittuone", "Milano"), (2023, ("marzo", 80))),
    (("Vittuone", "Milano"), (2023, ("aprile", 60))),
    (("Vittuone", "Milano"), (2023, ("maggio", 80))),
    (("Vittuone", "Milano"), (2023, ("luglio", 30))),
    (("Vittuone", "Milano"), (2023, ("agosto", "N/D"))),
    (("Varenna", "Lecco"), (2023, ("luglio", 150))),
    (("Morbegno", "Sondrio"), (2023, ("luglio", 165))),
    (("Magenta", "Milano"), (2023, ("luglio", 165))),
)


def media_globale():
    misure = []
    for tupla in tupla_pluviometrica:
        ((città, provincia), (anno, (mese, misura))) = tupla
        if misura == "N/D":
            continue
        if anno == 2023:
            misure.append(misura)
    if not misure:
        return 0
    return sum(misure) / len(misure)


def media(provincia_richiesta: str, mese_richiesto: str):
    misure: list[int] = []
    for tupla in tupla_pluviometrica:
        ((città, provincia), (anno, (mese, misura))) = tupla
        if misura == "N/D":
            continue
        if (
            provincia_richiesta == provincia
            and mese_richiesto == mese
        ):
            misure.append(misura)
    if not misure:
        return VALORI_NON_VALIDI
    return sum(misure) / len(misure)


def pioggia_max():
    misure = [(-1, "niente")]
    max_misura = 0
    for tupla in tupla_pluviometrica:
        ((città, provincia), (anno, (mese, misura))) = tupla
        if misura == "N/D" or provincia != "Milano":
            continue
        if misura > max_misura:
            misure.clear()
            max_misura, max_mese = misura, mese
            misure.append((misura, mese))
            continue
        if misura == max_misura:
            misure.append((max_misura, mese))
    return misure


def pioggia_min():
    misure = [(-1, "niente")]
    min_misura = 100_000_000_000
    for tupla in tupla_pluviometrica:
        ((città, provincia), (anno, (mese, misura))) = tupla
        if misura == "N/D":
            continue
        if misura < min_misura:
            misure.clear()
            min_misura, min_mese = misura, mese
            misure.append((misura, mese))
            continue
        if misura == min_misura:
            misure.append((min_misura, mese))
    return misure


def provincia_per():
    totale = 0
    percentuali_miste = []
    percentuali = {}
    for tupla in tupla_pluviometrica:
        ((città, provincia), (anno, (mese, misura))) = tupla
        if misura == "N/D":
            continue
        totale += misura

    for tupla in tupla_pluviometrica:
        ((città, provincia), (anno, (mese, misura))) = tupla
        if misura == "N/D":
            continue
        percentuali_miste.append((città, (misura / totale * 100)))

    for città, percentuale in percentuali_miste:
        if città not in percentuali:
            percentuali[città] = percentuale
        else:
            percentuali[città] += percentuale

    tupla_di_output = tuple(
        (città, round(percentuale, 2)) for (città, percentuale) in percentuali.items()
    )

    return tupla_di_output


menù = {
    "1": "Media globale",
    "2": "Media provincia e mese",
    "3": "Max pioggia",
    "4": "Min pioggia",
    "5": "Percentuali precipitazioni",
    "6": "Esci",
}


def print_menù():
    for i, entry in menù.items():
        print(f"{i}: {entry}")


def main():
    while True:
        print_menù()
        scelta = input("Scegli un'opzione: ")
        if scelta not in menù:
            print("Opzione non valida!")
            continue

        scelta = int(scelta)
        if scelta == 1:
            media = media_globale()
            print(f"La media globale è {round(media, 2)}.")
        elif scelta == 2:
            provincia = input("Inserisci una provincia: ")
            mese = input("Inserisi un mese: ")
            la_media = media(provincia, mese)
            if la_media == VALORI_NON_VALIDI:
                print("Valori non validi!")
            else:
                print(la_media)
        elif scelta == 3:
            print(pioggia_max())
        elif scelta == 4:
            print(pioggia_min())
        elif scelta == 5:
            percentuali = provincia_per()
            for provincia, percentuale in percentuali:
                print(f"{provincia}: {percentuale}%")
        elif scelta == 6:
            print("Grazie!")
            break


if __name__ == "__main__":
    main()
