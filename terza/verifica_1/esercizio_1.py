from statistics import mean

"""
Supponiamo di avere una tupla contenente i dati delle vendite  mensili
suddivise per reparto. Ogni voce nella tupla è strutturata nel seguente modo:

dati_vendite_mensili = ("reparto", [("mese1", 150), ("mese2", 175)])

Dove:
- "reparto" rappresenta il nome del reparto;
- "meseX" indica il mese di riferimento;
- "valoreX" indica l' ammontare delle vendite del meseX.

Si potrebbe verificare la situazione N/D (indica valore non disponibile)

Per calcolare l'ammontare medio delle vendite di un reparto definisci una
funzione che riceva come parametro il nome del reparto e restituisca una tupla
con questa struttura (ImpMedio,(valoreMax,meseMax),(valoreMin,meseMin)).
Dove meseMax e meseMin sono i mesi in cui si sono registrate le vendite
maggiori e le vendite  minori. **N.B*:** Nel calcolo della media non includere
i mesi per i quale non risulta disponibile il valore.

Ad esempio:
("repartoA", [
    ("gennaio", "N/D"),
    ("febbraio", 20000),
... ("Dicembre",23000)]
 )

La media andrà calcolata su 11 mesi e non su 12.

**N.B:** Prevedere la possibilità che la tupla ritornata sia vuota
"""

dati_vendite_mensili: list[
    tuple[
        str,
        list[tuple[str, float | None]],
    ]
] = [
    (
        "tecnologia",
        [
            ("Gennaio", 15_000),
            ("Febbraio", 12_000),
            ("Marzo", None),
            ("Aprile", 19_000),
            ("Maggio", 10_000),
            ("Giugno", None),
        ],
    ),
    (
        "alimentari",
        [
            ("Gennaio", 9_000),
            ("Febbraio", 15_000),
            ("Marzo", 11_000),
            ("Aprile", 13_000),
            ("Maggio", 12_000),
            ("Giugno", 17_000),
        ],
    ),
    (
        "agricoltura",
        [
            ("Gennaio", None),
            ("Febbraio", None),
            ("Marzo", None),
            ("Aprile", None),
            ("Maggio", None),
            ("Giugno", None),
        ],
    ),
]


def vendite_reparto(reparto: str) -> list[tuple[str, float | None]]:
    vendite = [
        dati_reparto[1]
        for dati_reparto in dati_vendite_mensili
        if dati_reparto[0] == reparto
    ]

    if len(vendite) == 0:
        return []
    return vendite[0]


def dati_vendite(vendite: list[tuple[str, float | None]]):
    somma = [vendita[1] for vendita in vendite if vendita[1] is not None]
    if not somma:
        return ()
    media = mean(somma)
    max_mese, min_mese = (0.0, "Nessun guadagno"), (
        10_000_000_000_000.0,
        "Nessun guadagno",
    )
    for vendita in vendite:
        mese, guadagno = vendita
        if not guadagno:
            continue

        if guadagno >= max_mese[0]:
            max_mese = (guadagno, mese)
        if guadagno <= min_mese[0]:
            min_mese = (guadagno, mese)

    return (media, max_mese, min_mese)


def main():
    for dato_vendita in dati_vendite_mensili:
        reparto = dato_vendita[0]
        vendite = vendite_reparto(reparto)
        dati = dati_vendite(vendite)
        if dati == ():
            print(f"""
Non sono disponibili dati per il reparto {reparto}!
""")
            return
        media, (max_valore, max_mese), (min_valore, min_mese) = dati
        output = f"""
# Per il reparto {reparto}:
La media delle vendite è {round(media, 2)}.
Il valore massimo è {max_valore}, registrato durante {max_mese}.
Il valore minimo è {min_valore}, registrato durante {min_mese}.
"""
        print(output)


if __name__ == "__main__":
    main()
