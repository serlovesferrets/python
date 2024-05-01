from es2 import Macchina
from es3 import macchine

"""
Crea una classe Garage che preveda i seguenti metodi (espressi in javascript):

```javascript
function Garage()
{
    this.macchine = new Array(); // array di macchine
    this.push = function(macchina); // inserisce un oggetto
    this.pop = function(); // estrae l'ultimo oggetto inserito
    this.remove = function(targa); // rimuove la macchina indicata
    this.cercaMacchina = function(targa); // verifica se una macchina è presente
}
```

Implementa nella classe il dunder method __str(self)__ per stampare come 
stringa tutto il contenuto della lista che contiene le macchine.

Inserisci delle macchine nel Garage e testa le diverse funzioni dell'oggetto
"""


class Garage:
    array: list[Macchina] = []

    def push(self, macchina: Macchina) -> None:
        self.array.append(macchina)

    def pop(self) -> None:
        self.array.pop()

    def remove(self, targa: str) -> None:
        self.array = [m for m in self.array if m.targa != targa]


garage = Garage()

for macchina in macchine:
    garage.push(macchina)
