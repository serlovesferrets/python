from statistics import mean
from menu import Menu

"""
2) Media voti di N studenti
1. Creare un dizionario di N studenti.
   Ogni elemento del dizionario contiene come chiave il nome dello studente e
   come valore una lista di tre numeri, cioè i voti in
   [Matematica, Fisica e Chimica].
2. Inserendo il nome di uno studente, il programma restituisce in output la
   media dei suoi voti (arrotandata a due cifre dopo la virgola).
"""

grades: dict[str, list[float]] = {
    "Marco": [7, 8, 9],
    "Pietro": [4, 5, 6],
    "Baulardo": [10, 5, 5],
    "Liuberto": [8, 6, 9],
}

grades_means: dict[str, float] = {
    name: mean(the_grades) for (name, the_grades) in grades.items()
}


def menu_print_students() -> None:
    for student in grades:
        print(f"- {student}")


def menu_get_mean() -> None:
    in_name = input("Inserisci un nome: ").title()
    while in_name not in grades_means:
        in_name = input("Nome non trovato! Riprova. ").title()

    print(f"La media dei voti di {in_name} è {grades_means[in_name]:.2f}")


menu = Menu({
    "Stampa degli studenti": menu_print_students,
    "Ricerca di una media": menu_get_mean
})

menu.run()
