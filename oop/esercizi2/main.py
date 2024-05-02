from dataclasses import dataclass
from functools import reduce
from typing import Callable

"""
Crea una classe in grado di contenere la pagella di uno studente di cui si fornisce il nome.
La pagella sarà composta da una lista di tuple (materia,voto,assenza)

Crea un metodo per ognuna delle seguenti richieste

1. Popola la pagella con dati a piacere inseriti dall'utente.
2. Visualizza la pagella inserita
3. Visualizza la somma delle ore di assenza in tutte le materie
4. Visualizza il numero delle materie insufficienti
5. Visualizza la media dei voti in tutte le materie
6. Visualizza il voto massimo e minimo e le rispettive materie. 

n.b.Questo metodo deve restituire un dizionario con le due chiavi: massimo e minimo.  es. di utilizzo: 

maxmin=pagella.votoMaxMin()
massimo=maxmin['massimo']
minimo=maxmin['minimo']
"""


@dataclass
class Materia:
    nome: str
    voto: int
    assenze: int


@dataclass
class Pagella:
    materie: list[Materia]

    def assenze(self) -> int:
        return sum((map(lambda x: x.assenze, self.materie)))

    def insufficienze(self) -> int:
        return len(list(filter(lambda x: x < 6, map(lambda x: x.voto, self.materie))))

    def media(self) -> float:
        count = len(self.materie)
        total = sum(map(lambda x: x.voto, self.materie))

        return total / count

    def voto_per(self, pred: Callable[[Materia, Materia], bool]) -> Materia:
        return reduce(lambda l, r: l if pred(l, r) else r, self.materie)

    def voto_max(self) -> Materia:
        return self.voto_per(lambda l, r: l.voto > r.voto)

    def voto_min(self) -> Materia:
        return self.voto_per(lambda l, r: l.voto < r.voto)

    def voto_max_min(self):
        return {
            "minimo": self.voto_min(),
            "massimo": self.voto_max(),
        }
