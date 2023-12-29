from __future__ import annotations
from dataclasses import dataclass
from typing import (Callable, Any)
from fable_modules.fable_library.int32 import parse
from fable_modules.fable_library.list import (FSharpList, map_indexed, reverse, length, cons, head, where)
from fable_modules.fable_library.reflection import (TypeInfo, int32_type, string_type, unit_type, lambda_type, record_type, list_type, class_type)
from fable_modules.fable_library.string_ import (to_console, printf)
from fable_modules.fable_library.types import (Record, FSharpException)
from fable_modules.fable_library.util import get_enumerator

def _expr0() -> TypeInfo:
    return record_type("Menu.MenuItem", [], MenuItem, lambda: [("Id", int32_type), ("Description", string_type), ("Function", lambda_type(unit_type, unit_type))])


@dataclass(eq = False, repr = False)
class MenuItem(Record):
    Id: int
    Description: str
    Function: Callable[[], None]

MenuItem_reflection = _expr0

def _expr1() -> TypeInfo:
    return record_type("Menu.Menu", [], Menu, lambda: [("Items", list_type(MenuItem_reflection())), ("ExitOpt", int32_type)])


@dataclass(eq = False, repr = False)
class Menu(Record):
    Items: FSharpList[MenuItem]
    ExitOpt: int

Menu_reflection = _expr1

def _expr2() -> TypeInfo:
    return class_type("Menu.NotMenuItemException", None, NotMenuItemException, class_type("System.Exception"))


class NotMenuItemException(FSharpException):
    def __init__(self, Data0: str) -> None:
        super().__init__()
        self.Data0 = Data0


NotMenuItemException_reflection = _expr2

def new_line(__unit: None=None) -> None:
    to_console(("" + "\n") + "")


def from_(items: FSharpList[tuple[str, Callable[[], None]]]) -> Menu:
    def mapping(i: int, tupled_arg: tuple[str, Callable[[], None]], items: Any=items) -> MenuItem:
        return MenuItem(i + 1, tupled_arg[0], tupled_arg[1])

    menu_items: FSharpList[MenuItem] = map_indexed(mapping, items)
    def _arrow5(__unit: None=None, items: Any=items) -> FSharpList[MenuItem]:
        items_1: FSharpList[MenuItem] = reverse(menu_items)
        def _arrow4(__unit: None=None) -> None:
            pass

        return cons(MenuItem(length(menu_items) + 1, "Exit", _arrow4), items_1)

    items_with_exit: FSharpList[MenuItem] = reverse(_arrow5())
    return Menu(items_with_exit, length(items_with_exit))


def print_opts(menu: Menu) -> None:
    with get_enumerator(menu.Items) as enumerator:
        while enumerator.System_Collections_IEnumerator_MoveNext():
            item: MenuItem = enumerator.System_Collections_Generic_IEnumerator_1_get_Current()
            to_console(((("" + str(item.Id)) + ". ") + item.Description) + "")


def run(menu: Menu) -> None:
    should_loop: bool = True
    new_line()
    print_opts(menu)
    to_console(printf("Quale opzione vuoi? "))
    while should_loop:
        pattern_input: tuple[bool, int]
        try: 
            value_1: int = parse(input(), 511, False, 32) or 0
            if True if (value_1 < 1) else (value_1 > menu.ExitOpt):
                raise NotMenuItemException("Non un\'opzione valida! Riprova.")

            pattern_input = (True, value_1)

        except Exception as match_value:
            if isinstance(match_value, NotMenuItemException):
                to_console(("" + match_value.Data0) + "")
                pattern_input = (False, -1)

            else: 
                raise match_value


        ok: bool = pattern_input[0]
        choice: int = pattern_input[1] or 0
        if ok:
            def predicate(item: MenuItem, menu: Any=menu) -> bool:
                return item.Id == choice

            func: Callable[[], None] = head(where(predicate, menu.Items)).Function
            new_line()
            func()

        should_loop = True if (not ok) else (choice != menu.ExitOpt)
        if should_loop:
            new_line()
            print_opts(menu)
            to_console(printf("Quale opzione vuoi? "))



