import functools as ft
import math
from typing import Any, Callable, Optional, Self, cast

type Error = str


def _is_vector(maybe_vector) -> bool:
    return type(maybe_vector) == Vector


class Vector[T]:
    _elts: list[T]

    def __init__(self, *items: T):
        self._elts = [*items]

    def __repr__(self) -> str:
        return self._elts.__repr__()

    def same_len_as(self, other: Self) -> bool:
        return len(self._elts) == len(other._elts)

    def __getitem__(self, index: int) -> T | Error:
        if index >= len(self._elts):
            return f"Index out of bounds: len({len(self._elts)}) <= i({index})"

        return self._elts[index]

    # No __setitem__, immutable vector

    def __eq__(self, other: object) -> bool:
        if not _is_vector(other):
            return False

        casted: Self = cast(Self, other)

        if len(self._elts) != len(casted._elts):
            return False

        zipped: list[tuple[T, T]] = list(zip(self._elts, casted._elts))

        def are_equal(state: bool, both: tuple[T, T]) -> bool:
            (x, y) = both
            return state and x == y

        return ft.reduce(are_equal, zipped, True)

    def sum(self) -> Optional[int]:
        """Only call if [T] supports arithmetic operations."""
        return sum(self._elts)  # type: ignore

    def zip_with(self, other: Self, with_fn: Callable[[T, T], T]) -> Self:
        zipped_weird_type: list[tuple[T, T]] = list(zip(self._elts, other._elts))
        zipped = cast(list[tuple[Self, Self]], zipped_weird_type)

        return cast(
            Self,
            Vector(*(with_fn(cast(T, x), cast(T, y)) for (x, y) in zipped)),
        )

    def __add__(self, other: Self) -> Optional[Self]:
        """Only call if [T] supports arithmetic operations."""
        if not self.same_len_as(other):
            return

        def unsafe_add(x: Any, y: Any) -> Any:
            return x + y

        return self.zip_with(other, unsafe_add)

    def __sub__(self, other: Self) -> Optional[Self]:
        """Only call if [T] supports arithmetic operations."""
        if not self.same_len_as(other):
            return

        def unsafe_sub(x: Any, y: Any) -> Any:
            return x - y

        return self.zip_with(other, unsafe_sub)

    def __mul__(self, other: Self) -> Optional[int]:
        """Only call if [T] supports arithmetic operations."""
        if not self.same_len_as(other):
            return

        def unsafe_mul(x: Any, y: Any) -> Any:
            return x * y

        multiplied_vec = self.zip_with(other, unsafe_mul)
        return sum(multiplied_vec._elts)  # type: ignore

    dot = __mul__

    def norm(self) -> Optional[float]:
        """Only call if [T] supports arithmetic operations."""
        return math.sqrt(self * self)  # type: ignore
