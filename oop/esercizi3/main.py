from classes import Pagella, Materia
from dataclasses import dataclass

"""
##1.Pagelle 2

Creare la classe Tabellone per contenere una serie di pagelle. Usare la classe Pagella creata in Pagelle 1(il notebook precedente)


```
function Tabellone(){
this.pagelle = new Array(); //array di oggetti pagella
this.push = function (pagella); //inserisce una pagella di uno studente
this.pop = function (studente); //estrae la pagella di uno studente
this.cercaPagella = function (studente); //verifica se la pagella di uno studente è presente e restituisce l'indice, -1 se non è presente
this.tutto = function (); //restituisce il tabellone, cioè l'array di pagelle in forma di stringa
}
```
La classe Tabellone al posto del metodo tutto() implementa il dunder method [__repr__(self)](https://www.pythontutorial.net/python-oop/python-__repr__/) che con l'istruzione print(tabellone) restituirà una stringa ottenuta concatenando le tuple di tutte le materie di tutti gli studenti.<br>


Testare il codice verificando ogni metodo. <br>
Invece di far inserire i dati all'utente, sostituire il metodo popola della classe Pagella con quello fornito sotto. Non importa che le pagelle siano tutte uguali.
"""


@dataclass
class Tabellone:
    pagelle: list[Pagella]

    def push(self, pagella: Pagella) -> None:
        self.pagelle.insert(0, pagella)

    def pop(self) -> Pagella:
        return self.pagelle.pop()

    def cerca_tabella(self, *, studente: str) -> Pagella | None:
        pagelle = [p for p in self.pagelle if p.studente == studente]

        return None if not pagelle else pagelle[0]


def popola(pagella: Pagella) -> None:
    pagella.materie.append(Materia("ITALIANO", 8, 5))
    pagella.materie.append(Materia("STORIA", 8, 4))
    pagella.materie.append(Materia("MATEMATICA", 10, 3))
    pagella.materie.append(Materia("LINGUA INGLESE", 9, 0))
    pagella.materie.append(Materia("TELECOMUNICAZIONI", 10, 7))
    pagella.materie.append(Materia("INFORMATICA", 5, 0))
    pagella.materie.append(Materia("SISTEMI E RETI", 5, 0))
    pagella.materie.append(Materia("TECN. PROG. SIST. I.", 4, 0))
    pagella.materie.append(Materia("SCIENZE MOTORIE E SP", 6, 0))


tabellone = Tabellone([])

p_1 = Pagella("MARCO", [])
popola(p_1)

p_2 = Pagella("POLO", [])
popola(p_2)

tabellone.push(p_1)
tabellone.push(p_2)

print(tabellone)
