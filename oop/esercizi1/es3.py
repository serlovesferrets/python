from es2 import Macchina

"""
Crea una lista di oggetti macchina (marca,modello,colore,anno, targa), inserisci
n oggetti, visualizzali uno ad uno tramite un ciclo.
"""

macchine = [
    Macchina("Ford", "Mustang", "rosso", 2022, "ABC 123"),
    Macchina("Toyota", "Corolla", "bianco", 2021, "DEF 456"),
    Macchina("Honda", "Civic", "blu", 2020, "GHI 789"),
    Macchina("Mercedes", "C-Class", "nero", 2019, "JKL 012"),
    Macchina("BMW", "3 Series", "argento", 2018, "MNO 345"),
]

map(print, macchine)
