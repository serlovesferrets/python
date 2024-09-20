from __future__ import annotations
from abc import abstractmethod
from typing import Protocol
from fable_modules.fable_library.double import divide
from fable_modules.fable_library.reflection import (TypeInfo, class_type)
from fable_modules.fable_library.string_ import (to_console, printf)

def _expr0() -> TypeInfo:
    return class_type("Program.Triangle", None, Triangle)


class Triangle:
    def __init__(self, __unit: None=None) -> None:
        pass


Triangle_reflection = _expr0

def Triangle__ctor(__unit: None=None) -> Triangle:
    return Triangle(__unit)


def Triangle__Area_7B00E9A0(self_1: Triangle, b: float, h: float) -> float:
    return divide(b * h, 2.0)


def Triangle__Perimeter_Z7AD9E565(self_1: Triangle, a: float, b: float, c: float) -> float:
    return (a + b) + c


triangle: Triangle = Triangle__ctor()

arg: float = Triangle__Area_7B00E9A0(triangle, 3.0, 4.0)

to_console(printf("Area del triangolo: %A"))(arg)

arg: float = Triangle__Perimeter_Z7AD9E565(triangle, 5.0, 3.0, 5.0)

to_console(printf("Perimetro del triangolo: %A"))(arg)

def _expr1() -> TypeInfo:
    return class_type("Program.Triangle\'", None, Triangle_0027)


class Triangle_0027:
    def __init__(self, a: float, b: float, c: float, h: float) -> None:
        self.a: float = a
        self.b: float = b
        self.c: float = c
        self.h: float = h


Triangle_0027_reflection = _expr1

def Triangle_0027__ctor_77D16AC0(a: float, b: float, c: float, h: float) -> Triangle_0027:
    return Triangle_0027(a, b, c, h)


def Triangle_0027__Area(self_1: Triangle_0027) -> float:
    return divide(self_1.b * self_1.h, 2.0)


def Triangle_0027__Perimeter(self_1: Triangle_0027) -> float:
    return (self_1.a + self_1.b) + self_1.c


triangle_0027: Triangle_0027 = Triangle_0027__ctor_77D16AC0(5.0, 2.0, 5.0, 4.5)

arg: float = Triangle_0027__Area(triangle_0027)

to_console(printf("Area del triangolo: %A"))(arg)

arg: float = Triangle_0027__Perimeter(triangle_0027)

to_console(printf("Perimetro del triangolo: %A"))(arg)

to_console(("" + str(triangle_0027)) + "")

def _expr2() -> TypeInfo:
    return class_type("Program.Triangle\'\'", None, Triangle_0027_0027)


class Triangle_0027_0027:
    def __init__(self, a: float, b: float, c: float, h: float) -> None:
        self.a: float = a
        self.b: float = b
        self.c: float = c
        self.h: float = h

    def __str__(self, __unit: None=None) -> str:
        self_1: Triangle_0027_0027 = self
        return ((("Area: " + str(Triangle_0027_0027__Area(self_1))) + "; Perimetro: ") + str(Triangle_0027_0027__Perimeter(self_1))) + ""


Triangle_0027_0027_reflection = _expr2

def Triangle_0027_0027__ctor_77D16AC0(a: float, b: float, c: float, h: float) -> Triangle_0027_0027:
    return Triangle_0027_0027(a, b, c, h)


def Triangle_0027_0027__Area(self_1: Triangle_0027_0027) -> float:
    return divide(self_1.b * self_1.h, 2.0)


def Triangle_0027_0027__Perimeter(self_1: Triangle_0027_0027) -> float:
    return (self_1.a + self_1.b) + self_1.c


triangle_0027_0027: Triangle_0027_0027 = Triangle_0027_0027__ctor_77D16AC0(5.0, 2.0, 5.0, 4.5)

to_console(("" + str(triangle_0027_0027)) + "")

class IShape(Protocol):
    @abstractmethod
    def Area(self) -> float:
        ...

    @abstractmethod
    def Perimeter(self) -> float:
        ...


def _expr3() -> TypeInfo:
    return class_type("Program.Square", None, Square)


class Square:
    def __init__(self, side: float) -> None:
        self.side: float = side

    def Area(self, __unit: None=None) -> float:
        self_1: Square = self
        return self_1.side * self_1.side

    def Perimeter(self, __unit: None=None) -> float:
        self_1: Square = self
        return self_1.side * 4.0


Square_reflection = _expr3

def Square__ctor_5E38073B(side: float) -> Square:
    return Square(side)


def _expr4() -> TypeInfo:
    return class_type("Program.Rectangle", None, Rectangle)


class Rectangle:
    def __init__(self, base_0027: float, height: float) -> None:
        self.base_0027: float = base_0027
        self.height: float = height

    def Area(self, __unit: None=None) -> float:
        self_1: Rectangle = self
        return self_1.base_0027 * self_1.height

    def Perimeter(self, __unit: None=None) -> float:
        self_1: Rectangle = self
        return (self_1.base_0027 * self_1.height) * 2.0


Rectangle_reflection = _expr4

def Rectangle__ctor_7B00E9A0(base_0027: float, height: float) -> Rectangle:
    return Rectangle(base_0027, height)


def print_area(shape: IShape) -> None:
    to_console(("" + str(shape.Area())) + "")


print_area(Rectangle__ctor_7B00E9A0(5.0, 3.0))

print_area(Square__ctor_5E38073B(4.0))

