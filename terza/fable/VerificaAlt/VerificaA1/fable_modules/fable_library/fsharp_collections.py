from typing import Any, Callable, TypeVar

from .util import IComparer_1, IEqualityComparer_1, compare, equals, physical_hash, structural_hash


_T_ = TypeVar("_T_")

_T = TypeVar("_T")


def HashIdentity_FromFunctions(hash_1: Callable[[_T], int], eq: Callable[[_T, _T], bool]) -> IEqualityComparer_1[Any]:
    class ObjectExpr0(IEqualityComparer_1[Any]):
        def Equals(self, x: _T_, y: _T_, hash_1: Any = hash_1, eq: Any = eq) -> bool:
            return eq(x, y)

        def GetHashCode(self, x_1: _T_ | None = None, hash_1: Any = hash_1, eq: Any = eq) -> int:
            return hash_1(x_1)

    return ObjectExpr0()


def HashIdentity_Structural(__unit: None = None) -> IEqualityComparer_1[Any]:
    return HashIdentity_FromFunctions(structural_hash, equals)


def HashIdentity_Reference(__unit: None = None) -> IEqualityComparer_1[Any]:
    def _arrow1(e: _T, e_1: _T) -> bool:
        return e == e_1

    return HashIdentity_FromFunctions(physical_hash, _arrow1)


def ComparisonIdentity_FromFunction(comparer: Callable[[_T, _T], int]) -> IComparer_1[_T]:
    class ObjectExpr2(IComparer_1[_T_]):
        def Compare(self, x: _T_, y: _T_, comparer: Any = comparer) -> int:
            return comparer(x, y)

    return ObjectExpr2()


def ComparisonIdentity_Structural(__unit: None = None) -> IComparer_1[Any]:
    return ComparisonIdentity_FromFunction(compare)


__all__ = [
    "HashIdentity_FromFunctions",
    "HashIdentity_Structural",
    "HashIdentity_Reference",
    "ComparisonIdentity_FromFunction",
    "ComparisonIdentity_Structural",
]
