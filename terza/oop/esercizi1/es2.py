from dataclasses import dataclass

"""
Creare una classe per rappresentare una macchina (marca,modello,colore,anno,
targa), avente anche il dunder method __str__(self) che restituisce una
stringa ottenuta concatenando tutti gli attributi dell'oggetto. Crea un oggetto,
assegna dei valori e visualizzali utilizzando il metodo print(oggetto)
"""


@dataclass
class Macchina:
    marca: str
    modello: str
    colore: str
    anno: int
    targa: str

    # Essendo dataclass implementa __str__(self) automaticamente


buick = Macchina(
    "Buick",
    "Envision",
    "grigio scuro",
    2023,
    "EHV 766",
)

print(buick)
