from dataclasses import dataclass, field


@dataclass
class Vehicle:
    code: str
    brand: str
    model: str
    price: float
    revision_year: int

    def sheet(self) -> str:
        return f"""Codice veicolo: {self.code}
    Marca:{self.brand}
    Modello: {self.model}
    Prezzo: {self.price}
    Anno Revisione: {self.revision_year}"""

    def modify_vehicle(self) -> None:
        choice = input(
            "Cosa vuoi modificare? 1-Codice 2-Marca 3-Modello 4-Prezzo 5-Anno revisione\n> "
        )

        if choice == "1":
            code = input("Inserire codice: ")
            self.code = code
        elif choice == "2":
            brand = input("Inserire marca: ")
            self.brand = brand
        elif choice == "3":
            model = input("Inserire modello: ")
            self.model = model
        elif choice == "4":
            price = float(input("Inserire prezzo: "))
            self.price = price
        elif choice == "5":
            revision_year = int(input("Inserire anno revisione: "))
            self.revision_year = revision_year
        else:
            print("Scelta non valida.")


@dataclass
class Car(Vehicle):
    length: float
    width: float

    def sheet(self) -> str:
        return (
            super().sheet()
            + f"""
    Lunghezza: {self.length}
    Larghezza: {self.width}"""
        )


@dataclass
class Motorcycle(Vehicle):
    kind: str
    power: float

    def sheet(self) -> str:
        return (
            super().sheet()
            + f"""
        Tipo: {self.kind}
        Potenza: {self.power}"""
        )


@dataclass
class Sale:
    code: str
    date: str
    vendor_code: str
    vehicles: list[Vehicle] = field(default_factory=list)

    def add_vehicle(self, vehicle: Vehicle) -> None:
        if isinstance(vehicle, Car):
            print("Aggiunta macchina")
        elif isinstance(vehicle, Motorcycle):
            print("Aggiunto motociclo")
        else:
            print("Aggiunto veicolo")

        self.vehicles.append(vehicle)

    def remove_vehicle(self, vehicle_code: str) -> None:
        size = len(self.vehicles)
        self.vehicles = [v for v in self.vehicles if v.code != vehicle_code]

        if len(self.vehicles) == size:
            print("Codice non trovato.")

    def gain(self) -> None:
        size = len(self.vehicles)
        suffix = "o" if size == 1 else "i"
        print(f"Vendut{suffix} {size} veicol{suffix}.")
        print(f"Ricavo: {sum([v.price for v in self.vehicles])}")

    def sale_details(self) -> tuple[float, float, float, float]:
        car_reward = 3
        motorcycle_reward = 2

        (car_sum, mcl_sum, unclassified_sum) = (0, 0, 0)
        reward = 0

        for vehicle in self.vehicles:
            vehicle.sheet()

            if isinstance(vehicle, Car):
                car_sum += vehicle.price
                reward += vehicle.price / 100 * car_reward
            elif isinstance(vehicle, Motorcycle):
                mcl_sum += vehicle.price
                reward += vehicle.price / 100 * motorcycle_reward
            else:
                unclassified_sum += vehicle.price

        return (car_sum, mcl_sum, car_sum + mcl_sum, reward)


@dataclass
class Sales:
    name: str
    code: str
    sales: list[Sale] = field(default_factory=list)

    def add_sale(self, sale: Sale) -> None:
        self.sales.append(sale)

    def remove_sale(self, sale_code: str) -> None:
        size = len(self.sales)
        self.sales = [v for v in self.sales if v.code != sale_code]

        if len(self.sales) == size:
            print("Codice non trovato.")

    def total_sales(self) -> tuple[float, float, float]:
        (car_total, mcl_total, total) = (0, 0, 0)

        for sale in self.sales:
            (car, mcl, unclassified, _) = sale.sale_details()
            car_total += car
            mcl_total += mcl
            total += car_total + mcl_total + unclassified

        return (car_total, mcl_total, total)


def main() -> None:
    car = Car(
        code="1",
        brand="MarcaA",
        model="ModelloX",
        price=25000,
        revision_year=2015,
        length=4.5,
        width=1.85,
    )

    car.modify_vehicle()
    print(car.sheet())

    motorcycle = Motorcycle(
        code="2",
        brand="MarcaB",
        model="ModelloY",
        price=7000,
        revision_year=2020,
        kind="Sport",
        power=75,
    )

    motorcycle.modify_vehicle()
    print(motorcycle.sheet())

    sale = Sale(code="S001", date="2023-10-01", vendor_code="V001")

    sale.add_vehicle(car)
    sale.add_vehicle(motorcycle)

    sale.gain()

    car_sum, mcl_sum, total_sum, reward = sale.sale_details()
    print(
        f"Totale Auto: {car_sum}, Totale Motociclette: {mcl_sum}, Totale: {total_sum}, Provisione: {reward}"
    )

    sales = Sales(name="Vendite Mensili", code="VM001")

    sales.add_sale(sale)

    car_total, mcl_total, total = sales.total_sales()
    print(
        f"Totale Vendite Auto: {car_total}, Totale Vendite Motociclette: {mcl_total}, Totale Complessivo: {total}"
    )

    sale.remove_vehicle("1")
    sale.gain()
    print(f"Veicoli rimanenti nella vendita: {[v.code for v in sale.vehicles]}")


if __name__ == "__main__":
    main()
