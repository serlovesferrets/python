from __future__ import annotations
from dataclasses import dataclass
from typing import (Any, Callable)
from fable_modules.fable_library.double import divide
from fable_modules.fable_library.list import (of_array, FSharpList, fold, average, cons, empty, is_empty, filter, max_by, min_by, map as map_1)
from fable_modules.fable_library.map import (contains_key, add, find, empty as empty_1, iterate)
from fable_modules.fable_library.option import map
from fable_modules.fable_library.reflection import (TypeInfo, float64_type, union_type, string_type, record_type)
from fable_modules.fable_library.string_ import (to_text, printf, to_console, interpolate)
from fable_modules.fable_library.types import (Array, Union, Record, to_string)
from fable_modules.fable_library.util import compare_primitives
from menu import (run, from_)

def _expr6() -> TypeInfo:
    return union_type("Program.Pagamento", [], Pagamento, lambda: [[("Item", float64_type)], [("Item", float64_type)], []])


class Pagamento(Union):
    def __init__(self, tag: int, *fields: Any) -> None:
        super().__init__()
        self.tag: int = tag or 0
        self.fields: Array[Any] = list(fields)

    @staticmethod
    def cases() -> list[str]:
        return ["Contanti", "Carta", "Non disponibile"]

    def __str__(self, __unit: None=None) -> str:
        self_1: Pagamento = self
        if self_1.tag == 1:
            return to_text(printf("carta"))

        elif self_1.tag == 2:
            return to_text(printf("non disponibile"))

        else: 
            return to_text(printf("contanti"))



Pagamento_reflection = _expr6

def Pagamento__get_Value(self_1: Pagamento) -> float:
    if self_1.tag == 1:
        return self_1.fields[0]

    elif self_1.tag == 2:
        raise Exception("nessun valore")

    else: 
        return self_1.fields[0]



def _expr7() -> TypeInfo:
    return record_type("Program.Reparto", [], Reparto, lambda: [("Sezione", string_type), ("Categoria", string_type)])


@dataclass(eq = False, repr = False)
class Reparto(Record):
    Sezione: str
    Categoria: str

Reparto_reflection = _expr7

def _expr8() -> TypeInfo:
    return record_type("Program.Prodotto", [], Prodotto, lambda: [("Nome", string_type), ("Pagamento", Pagamento_reflection())])


@dataclass(eq = False, repr = False)
class Prodotto(Record):
    Nome: str
    Pagamento: Pagamento

Prodotto_reflection = _expr8

def _expr9() -> TypeInfo:
    return record_type("Program.Vendita", [], Vendita, lambda: [("Reparto", Reparto_reflection()), ("Prodotto", Prodotto_reflection())])


@dataclass(eq = False, repr = False)
class Vendita(Record):
    Reparto: Reparto
    Prodotto: Prodotto

Vendita_reflection = _expr9

def get_value(vendita: Vendita) -> float:
    match_value: Pagamento = vendita.Prodotto.Pagamento
    (pattern_matching_result, v) = (None, None)
    if match_value.tag == 1:
        pattern_matching_result = 0
        v = match_value.fields[0]

    elif match_value.tag == 2:
        pattern_matching_result = 1

    else: 
        pattern_matching_result = 0
        v = match_value.fields[0]

    if pattern_matching_result == 0:
        return v

    elif pattern_matching_result == 1:
        raise Exception("non disponibile")



def get_value_default(default_value: float, vendita: Vendita) -> float:
    match_value: Pagamento = vendita.Prodotto.Pagamento
    (pattern_matching_result, v) = (None, None)
    if match_value.tag == 1:
        pattern_matching_result = 0
        v = match_value.fields[0]

    elif match_value.tag == 2:
        pattern_matching_result = 1

    else: 
        pattern_matching_result = 0
        v = match_value.fields[0]

    if pattern_matching_result == 0:
        return v

    elif pattern_matching_result == 1:
        return default_value



tupla_vendite: FSharpList[Vendita] = of_array([Vendita(Reparto("A", "Informatica"), Prodotto("Prodotto A", Pagamento(0, 1000))), Vendita(Reparto("A", "Informatica"), Prodotto("Prodotto B", Pagamento(0, 1500))), Vendita(Reparto("A", "Informatica"), Prodotto("Prodotto C", Pagamento(1, 1200.0))), Vendita(Reparto("A", "Informatica"), Prodotto("Prodotto D", Pagamento(0, 200.0))), Vendita(Reparto("A", "Informatica"), Prodotto("Prodotto E", Pagamento(0, 800.0))), Vendita(Reparto("A", "Informatica"), Prodotto("Prodotto F", Pagamento(2))), Vendita(Reparto("B", "Elettronica"), Prodotto("Prodotto A", Pagamento(0, 1500.0))), Vendita(Reparto("B", "Elettronica"), Prodotto("Prodotto B", Pagamento(1, 900.0)))])

def folder(tupled_arg: tuple[float, float], vendita: Vendita) -> tuple[float, float]:
    acc: float = tupled_arg[0]
    count: float = tupled_arg[1]
    match_value: Pagamento = vendita.Prodotto.Pagamento
    if match_value.tag == 1:
        return (acc + match_value.fields[0], count + 1.0)

    elif match_value.tag == 2:
        return (acc, count)

    else: 
        return (acc + match_value.fields[0], count + 1.0)



pattern_input_0040126: tuple[float, float] = fold(folder, (0.0, 0.0), tupla_vendite)

totale: float = pattern_input_0040126[0]

conto: float = pattern_input_0040126[1]

media_globale: float = divide(totale, conto)

def menu_media_globale(__unit: None=None) -> None:
    to_console(interpolate("La media globale è %.2f%P().", [media_globale]))


def media(categoria: str, pagamento: str) -> float | None:
    def mapping(list_2: FSharpList[float], categoria: Any=categoria, pagamento: Any=pagamento) -> float:
        class ObjectExpr12:
            @property
            def GetZero(self) -> Callable[[], float]:
                def _arrow10(__unit: None=None) -> float:
                    return 0.0

                return _arrow10

            @property
            def Add(self) -> Callable[[float, float], float]:
                def _arrow11(x_1: float, y: float) -> float:
                    return x_1 + y

                return _arrow11

            @property
            def DivideByInt(self) -> Callable[[float, int], float]:
                return divide

        return average(list_2, ObjectExpr12())

    def _arrow13(__unit: None=None, categoria: Any=categoria, pagamento: Any=pagamento) -> FSharpList[float] | None:
        def folder(acc: FSharpList[float], vendita: Vendita) -> FSharpList[float]:
            if (vendita.Reparto.Categoria.lower() == categoria) if (to_string(vendita.Prodotto.Pagamento) == pagamento) else False:
                return cons(Pagamento__get_Value(vendita.Prodotto.Pagamento), acc)

            else: 
                return acc


        xs: FSharpList[float] = fold(folder, empty(), tupla_vendite)
        return None if is_empty(xs) else xs

    return map(mapping, _arrow13())


def menu_media(__unit: None=None) -> None:
    to_console(printf("Quale categoria vuoi? "))
    categoria_richiesta: str = input().lower()
    to_console(printf("Che tipo di pagamento vuoi? "))
    to_console("La media è %P(F2).")


def vendita_in_base_a(determinatore: Callable[[Callable[[Vendita], float], FSharpList[Vendita]], Vendita], mempty: float) -> FSharpList[Vendita]:
    def _arrow14(vendita: Vendita, determinatore: Any=determinatore, mempty: Any=mempty) -> float:
        return get_value_default(mempty, vendita)

    valore: Vendita = determinatore(_arrow14, tupla_vendite)
    def predicate(vendita_2: Vendita, determinatore: Any=determinatore, mempty: Any=mempty) -> bool:
        return get_value(vendita_2) == get_value(valore)

    def folder(acc: FSharpList[Vendita], vendita_1: Vendita, determinatore: Any=determinatore, mempty: Any=mempty) -> FSharpList[Vendita]:
        match_value: Pagamento = vendita_1.Prodotto.Pagamento
        if match_value.tag == 0:
            return cons(vendita_1, acc)

        elif match_value.tag == 2:
            return acc

        else: 
            return cons(vendita_1, acc)


    return filter(predicate, fold(folder, empty(), tupla_vendite))


def _arrow16(projection: Callable[[Vendita], float], list_1: FSharpList[Vendita]) -> Vendita:
    class ObjectExpr15:
        @property
        def Compare(self) -> Callable[[float, float], int]:
            return compare_primitives

    return max_by(projection, list_1, ObjectExpr15())


vendita_max: FSharpList[Vendita] = vendita_in_base_a(_arrow16, -2147483648)

def _arrow18(projection: Callable[[Vendita], float], list_1: FSharpList[Vendita]) -> Vendita:
    class ObjectExpr17:
        @property
        def Compare(self) -> Callable[[float, float], int]:
            return compare_primitives

    return min_by(projection, list_1, ObjectExpr17())


vendita_min: FSharpList[Vendita] = vendita_in_base_a(_arrow18, 2147483647)

def menu_vendita_max(__unit: None=None) -> None:
    to_console("La vendita massima è %P(F2).")


def menu_vendita_min(__unit: None=None) -> None:
    to_console("La vendita minima è %P(F2).")


def _expr19() -> TypeInfo:
    return record_type("Program.DatoMedia", [], DatoMedia, lambda: [("Reparto", string_type), ("Percentuale", float64_type)])


@dataclass(eq = False, repr = False)
class DatoMedia(Record):
    Reparto: str
    Percentuale: float
    def __str__(self, __unit: None=None) -> str:
        self_1: DatoMedia = self
        return to_text(printf("%s: %.2f"))(self_1.Reparto)(self_1.Percentuale)


DatoMedia_reflection = _expr19

def mapping(vendita: Vendita) -> DatoMedia:
    return DatoMedia(vendita.Reparto.Categoria, divide(get_value_default(0, vendita), totale) * 100.0)


media_per_not_unique: FSharpList[DatoMedia] = map_1(mapping, tupla_vendite)

def folder(acc: Any, value: DatoMedia) -> Any:
    if contains_key(value.Reparto, acc):
        return add(value.Reparto, find(value.Reparto, acc) + value.Percentuale, acc)

    else: 
        return add(value.Reparto, value.Percentuale, acc)



class ObjectExpr20:
    @property
    def Compare(self) -> Callable[[str, str], int]:
        return compare_primitives


media_per: Any = fold(folder, empty_1(ObjectExpr20()), media_per_not_unique)

def menu_media_per(__unit: None=None) -> None:
    def action(k: str, v: float) -> None:
        to_console(printf("%s: %.2f%% "))(k)(v)

    iterate(action, media_per)


def _arrow22(__unit: None=None) -> None:
    menu_media_globale()


def _arrow23(__unit: None=None) -> None:
    menu_media()


def _arrow24(__unit: None=None) -> None:
    menu_vendita_max()


def _arrow25(__unit: None=None) -> None:
    menu_vendita_min()


def _arrow26(__unit: None=None) -> None:
    menu_media_per()


run(from_(of_array([("Media globale", _arrow22), ("Media per categoria e pagamento", _arrow23), ("Vendita massima", _arrow24), ("Vendita minima", _arrow25), ("Percentuale vendite per reparto", _arrow26)])))

