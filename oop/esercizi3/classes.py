from dataclasses import dataclass
from typing import Callable
from functools import reduce


@dataclass
class Materia:
    nome: str
    voto: int
    assenze: int


@dataclass
class Pagella:
    studente: str
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
