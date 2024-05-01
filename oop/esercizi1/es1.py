from math import pi

"""
Definisci una classe per rappresentare una circonferenza che, prendendo in input
il raggio, definisca dei metodi per il calcolo di diametro, circonferenza e
area. Documenta i metodi utilizzando le Docstring.
"""


class Cerchio:
    def __init__(self, raggio: float):
        self.raggio = raggio

    def diametro(self) -> float:
        "Ritorna il diametro"
        return self.raggio * 2

    def circonferenza(self) -> float:
        "Ritorna la circonferenza"
        return 2 * pi * self.raggio

    def area(self) -> float:
        "Ritorna l'area"
        return pi * (self.raggio**2)
