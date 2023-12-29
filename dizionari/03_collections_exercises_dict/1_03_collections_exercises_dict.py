from statistics import mean

"""
1) Media voti di uno studente
Supponiamo di dover gestire i voti di uno studente in diverse materie.

Utilizza un dizionario per memorizzare i voti dello studente, come chiave la
materia e come valore il voto.

Il programma chiede all'utente di inserire i voti per ciascuna materia e alla
fine dell'inserimento calcola la media dei voti utilizzando una funzione.
"""

# Esempio
grades: dict[str, list[float]] = {}


def input_grade(
    *,
    prompt: str,
    on_invalid_float: str,
    on_invalid_input: str,
) -> tuple[str, float]:
    text: list[str] = input(prompt).split(" ")
    try:
        subject, grade = text[0], float(text[1])
        return (subject, grade)
    except ValueError:
        return input_grade(
            prompt=on_invalid_float,
            on_invalid_input=on_invalid_input,
            on_invalid_float=on_invalid_float,
        )
    except IndexError:
        return input_grade(
            prompt=on_invalid_input,
            on_invalid_float=on_invalid_float,
            on_invalid_input=on_invalid_input,
        )


grades_count = int(input("Quanti voti vuoi inserire? "))

for i in range(grades_count):
    subject, subject_grades = input_grade(
        prompt=f"{i + 1}. Inserisci una materia e un voto: ",
        on_invalid_float="Non un numero! Riprova. ",
        on_invalid_input="Input invalido! Segui la forma <materia> <voto>. ",
    )

    if subject not in grades:
        grades[subject] = []

    grades[subject].append(subject_grades)

means: list[float] = []

print("Voti dello studente:")
for subject, grades_list in grades.items():
    the_mean = mean(grades_list)
    means.append(the_mean)
    print(f"{subject}: {the_mean:.2f}")

print(f"La media totale è {mean(means):.2f}.")
