from dataclasses import dataclass
from typing import Literal, Optional

type PropertyType = Literal[
    "monolocal",
    "bilocal",
    "three-room apartment",
    "four-room apartment",
]


@dataclass
class Property:
    code: str
    squared_meters: float
    property_type: PropertyType
    address: str
    price: float
    sold: bool = False

    def __hash__(self) -> int:
        return self.code.__hash__()


type Error = str


def main() -> None:
    # Es 1
    property_1 = Property(
        code="thomas-casa-123",
        squared_meters=25,
        property_type="monolocal",
        address="Via Marcallo sul Ticino",
        price=80_000,
    )

    print(property_1)

    # Es 2
    property_2 = Property(
        code="villa-del-sole-456",
        squared_meters=120,
        property_type="four-room apartment",
        address="Viale dei Girasoli",
        price=350_000,
    )

    property_3 = Property(
        code="appartamento-centrale-789",
        squared_meters=65,
        property_type="monolocal",
        address="Piazza del Duomo",
        price=180_000,
    )

    property_4 = Property(
        code="casale-in-campagna-321",
        squared_meters=180,
        property_type="three-room apartment",
        address="Strada Provinciale 45",
        price=220_000,
    )

    property_5 = Property(
        code="bilocale-vista-mare-654",
        squared_meters=45,
        property_type="monolocal",
        address="Lungomare Cristoforo Colombo",
        price=120_000,
    )

    properties = [property_2, property_3, property_4, property_5]

    for i, property in enumerate(properties):
        print(f"Proprietà numero {i + 1}: {property}")


if __name__ == "__main__":
    main()


# Es 3
class Agency:
    properties: set[Property] = set()
    revenue: float = 0

    def add_property(self, property: Property) -> None | Error:
        for own_property in self.properties:
            if own_property.code == property.code:
                return f"Proprietà già aggiunta! {property.code}"
        self.properties.add(property)

    def sell_property(self, property_code: str) -> None | Error:
        found = False
        for property in self.properties:
            if property.code == property_code:
                found = True
                property.sold = True
                self.revenue += property.price
        if not found:
            return f"Proprietà non trovata! ({property_code})"

    def update_price(self, property_code: str, new_price: float) -> None | Error:
        found = False
        for property in self.properties:
            if property.code == property_code:
                found = True
                property.price = new_price
        if not found:
            return f"Proprietà non trovata! ({property_code})"

    def get_properties_of_type(self, property_type: PropertyType) -> set[Property]:
        return {
            property
            for property in self.properties
            if property.property_type == property_type
        }

    def find_property(self, property_code: str) -> Optional[Property]:
        for property in self.properties:
            if property.code == property_code:
                return property

    def get_properties(self) -> set[Property]:
        return set(self.properties.copy())

    def get_revenue(self) -> float:
        return self.revenue

    def get_mean_price_of_type(self, property_type: PropertyType) -> float:
        total, count = 0, 0

        for property in self.properties:
            if property.property_type == property_type:
                count += 1
                total += property.price

        return 0 if count == 0 else total / count
