from __future__ import annotations
from typing import (Any, Callable, TypeVar)
from fable_modules.fable_library.double import divide
from fable_modules.fable_library.list import (of_array, FSharpList, try_find, filter, map as map_1, average_by, max_by, min_by)
from fable_modules.fable_library.option import (map, value)
from fable_modules.fable_library.string_ import (to_console, printf)
from fable_modules.fable_library.util import compare_primitives

__A = TypeVar("__A")

__B = TypeVar("__B")

misurazioni: FSharpList[tuple[str, FSharpList[tuple[str, float | None]]]] = of_array([("Milano", of_array([("Gennaio", None), ("Febbraio", 0.0), ("Marzo", 9.0), ("Aprile", 12.0), ("Maggio", 16.0)])), ("Brescia", of_array([("Gennaio", 9.0), ("Febbraio", None), ("Marzo", 0.0), ("Aprile", None), ("Maggio", 7.0)])), ("Napoli", of_array([("Gennaio", 15.0), ("Febbraio", 16.0), ("Marzo", 7.0), ("Aprile", None), ("Maggio", None)]))])

def calcola_dati(nome_1: str) -> tuple[float, tuple[str, float], tuple[str, float]] | None:
    def op_greater_greater_equals(a: __A | None, b: Callable[[__A], __B], nome_1: Any=nome_1) -> __B | None:
        return map(b, a)

    def _arrow0(option: Any | None=None, nome_1: Any=nome_1) -> bool:
        return option is None

    def _arrow1(option_1: Any | None=None, nome_1: Any=nome_1) -> bool:
        return option_1 is not None

    def _arrow2(option_2: Any | None=None, nome_1: Any=nome_1) -> Any:
        return value(option_2)

    pattern_input: tuple[Callable[[Any | None], bool], Callable[[Any | None], bool], Callable[[Any | None], Any]] = (_arrow0, _arrow1, _arrow2)
    is_none: Callable[[Any | None], bool] = pattern_input[0]
    get: Callable[[Any | None], Any] = pattern_input[2]
    def predicate(arg: tuple[str, FSharpList[tuple[str, float | None]]], nome_1: Any=nome_1) -> bool:
        return nome_1 == arg[0]

    def _arrow3(tuple_1: tuple[str, FSharpList[tuple[str, float | None]]], nome_1: Any=nome_1) -> FSharpList[tuple[str, float | None]]:
        return tuple_1[1]

    def _arrow4(list_2: FSharpList[tuple[str, float | None]], nome_1: Any=nome_1) -> FSharpList[tuple[str, float | None]]:
        def predicate_1(arg_1: tuple[str, float | None]) -> bool:
            return pattern_input[1](arg_1[1])

        return filter(predicate_1, list_2)

    def _arrow5(list_3: FSharpList[tuple[str, float | None]], nome_1: Any=nome_1) -> FSharpList[tuple[str, float | None]]:
        def predicate_2(arg_3: tuple[str, float | None]) -> bool:
            return 0.0 != get(arg_3[1])

        return filter(predicate_2, list_3)

    def _arrow6(list_4: FSharpList[tuple[str, float | None]], nome_1: Any=nome_1) -> FSharpList[tuple[str, float]]:
        def mapping(tupled_arg: tuple[str, float | None]) -> tuple[str, float]:
            return (tupled_arg[0], get(tupled_arg[1]))

        return map_1(mapping, list_4)

    dati_1: FSharpList[tuple[str, float]] | None = op_greater_greater_equals(op_greater_greater_equals(op_greater_greater_equals(op_greater_greater_equals(try_find(predicate, misurazioni), _arrow3), _arrow4), _arrow5), _arrow6)
    def _arrow10(list_5: FSharpList[tuple[str, float]], nome_1: Any=nome_1) -> float:
        def projection(tuple_4: tuple[str, float]) -> float:
            return tuple_4[1]

        class ObjectExpr9:
            @property
            def GetZero(self) -> Callable[[], float]:
                def _arrow7(__unit: None=None) -> float:
                    return 0.0

                return _arrow7

            @property
            def Add(self) -> Callable[[float, float], float]:
                def _arrow8(x_2: float, y_2: float) -> float:
                    return x_2 + y_2

                return _arrow8

            @property
            def DivideByInt(self) -> Callable[[float, int], float]:
                return divide

        return average_by(projection, list_5, ObjectExpr9())

    media: float | None = op_greater_greater_equals(dati_1, _arrow10)
    def _arrow12(list_6: FSharpList[tuple[str, float]], nome_1: Any=nome_1) -> tuple[str, float]:
        def projection_1(tuple_5: tuple[str, float]) -> float:
            return tuple_5[1]

        class ObjectExpr11:
            @property
            def Compare(self) -> Callable[[float, float], int]:
                return compare_primitives

        return max_by(projection_1, list_6, ObjectExpr11())

    mese_max: tuple[str, float] | None = op_greater_greater_equals(dati_1, _arrow12)
    def _arrow14(list_7: FSharpList[tuple[str, float]], nome_1: Any=nome_1) -> tuple[str, float]:
        def projection_2(tuple_6: tuple[str, float]) -> float:
            return tuple_6[1]

        class ObjectExpr13:
            @property
            def Compare(self) -> Callable[[float, float], int]:
                return compare_primitives

        return min_by(projection_2, list_7, ObjectExpr13())

    mese_min: tuple[str, float] | None = op_greater_greater_equals(dati_1, _arrow14)
    if True if (True if is_none(media) else is_none(mese_max)) else is_none(mese_min):
        return None

    else: 
        return (get(media), get(mese_max), get(mese_min))



to_console(printf("Di quale città vuoi i dati? "))

nome: str = input()

dati: tuple[float, tuple[str, float], tuple[str, float]] | None = calcola_dati(nome)



if dati is not None:
    val_min: float = dati[2][1]
    val_max: float = dati[1][1]
    mese_min: str = dati[2][0]
    mese_max: str = dati[1][0]
    media: float = dati[0]
    to_console(printf("\r\nMedia: %.2f\r\nMese con valore massimo: %s, %.2f\r\nMese con valore minimo: %s, %.2f\r\n"))(media)(mese_max)(val_max)(mese_min)(val_min)

else: 
    to_console(printf("Niente!"))


