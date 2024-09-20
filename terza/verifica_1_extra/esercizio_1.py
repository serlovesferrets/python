from statistics import mean

tupla_consumi_energetici: list[
    tuple[
        str,
        list[
            tuple[
                str,
                tuple[str, float],
            ]
        ],
    ]
] = [
    (
        "Milano",
        [
            ("gennaio", ("elettrico", 300)),
            ("gennaio", ("gas", 150)),
            ("gennaio", ("elettrico", 280)),
            ("gennaio", ("gas", 120)),
            ("febbraio", ("elettrico", 280)),
            ("febbraio", ("gas", 120)),
        ],
    ),
    (
        "Brescia",
        [
            ("gennaio", ("elettrico", 280)),
            ("gennaio", ("gas", 140)),
            ("febbraio", ("elettrico", 260)),
            ("febbraio", ("gas", 130)),
        ],
    ),
]


def analizza_consumi_energetici(
    citta: str,
    risorsa: str,
):  # -> tuple[float, list[tuple[float, str]]]:
    # (media_risorsa, (max_risorsa, meseMax_risorsa)
    consumi = [c for c in tupla_consumi_energetici if c[0] == citta][0][1]
    valori = [(v[0], v[1][1]) for v in consumi if v[1][0] == risorsa]
    misure: list[float] = []

    mese_e_misure: dict[str, list[float]] = {}

    for mese, valore in valori:
        if mese not in mese_e_misure:
            mese_e_misure[mese] = []

        misure.append(valore)
        mese_e_misure[mese].append(valore)

    dati = tuple(
        (mese, max(valori_di_cui_fare_media))
        for (mese, valori_di_cui_fare_media) in mese_e_misure.items()
    )
    media = mean(misure)
    return (media, dati)


# Esempio di utilizzo
risultato_analisi = analizza_consumi_energetici("Milano", "elettrico")
print(risultato_analisi)
